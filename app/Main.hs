{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8        as SBS
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Conduit
import           Data.Conduit.Binary          (sinkFile)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as C
import           Data.Conduit.Network
import qualified Data.Conduit.Network         as DCN (HostPreference (..))
import qualified Data.Conduit.Text            as DCT
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  (mempty, (<>))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import qualified Paths_hnormalise
import           System.Exit                  (exitFailure, exitSuccess)

import Debug.Trace
--------------------------------------------------------------------------------
import           HNormalise                   (normaliseRsyslog)
import           HNormalise.Config            (Config (..), loadConfig)
import           HNormalise.Internal  (Rsyslog (..))
import           HNormalise.Json
import           HNormalise.Parser    (parseRsyslogLogstashString)

--------------------------------------------------------------------------------
data Options = Options
    { oConfigFilePath :: !(Maybe FilePath)
    , oJsonInput      :: !Bool
    , oVersion        :: !Bool
    , oTestFilePath   :: !(Maybe FilePath)
    } deriving (Show)

--------------------------------------------------------------------------------
parserOptions :: OA.Parser Options
parserOptions = Options
    <$> (OA.optional $ OA.strOption $
            OA.long "configfile" <>
            OA.short 'c' <>
            OA.metavar "FILENAME" <>
            OA.help "configuration file location ")
    <*> (OA.switch $
            OA.long "jsoninput" <>
            OA.short 'j' <>
            OA.help "Input will be delivered as JSON (slower)")
    <*> (OA.switch $
            OA.long "version" <>
            OA.short 'v' <>
            OA.help "Display version and exit" <>
            OA.hidden)
    <*> (OA.optional $ OA.strOption $
            OA.long "test" <>
            OA.short 't' <>
            OA.metavar "OUTPUT FILENAME" <>
            OA.help "run in test modus, sinking output to the given file")


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> parserOptions)
    (OA.fullDesc
        <> OA.progDesc "Normalise rsyslog messages"
        <> OA.header ("hNormalise v" <> showVersion Paths_hnormalise.version)
    )

--------------------------------------------------------------------------------
data Normalised = Transformed !SBS.ByteString
                | Original !SBS.ByteString

--------------------------------------------------------------------------------
normalise :: Bool            -- ^ Is the input a JSON string?
          -> T.Text          -- ^ Input
          -> Normalised      -- ^ Transformed or Original result
normalise jsonInput logLine =
    let n = --if jsonInput then Data.Aeson.decodeStrict logLine >>= normaliseRsyslog
            --else
            case parse parseRsyslogLogstashString logLine of
                    Done _ r    -> Just r
                    Partial c   -> case c T.empty of
                                        Done _ r -> Just r
                                        _        -> Nothing
                    _           -> Nothing
    in case n of
        Just j  -> Transformed j
        Nothing -> Original $ BS.toStrict $ encode $ logLine

--------------------------------------------------------------------------------
messageSink success failure = loop
  where
    loop = do
        v <- await
        case v of
            Just (Transformed json) -> do
                yield json $$ appSink success
                yield (SBS.pack "\n") $$ appSink success
                loop
            Just (Original l) -> do
                yield l $$ appSink failure
                yield (SBS.pack "\n") $$ appSink success
                loop
            Nothing -> return ()


--------------------------------------------------------------------------------
-- for testing purposes
mySink = loop
  where
    loop = do
        v <- await
        case v of
            Just (Transformed json) -> do
                yield (SBS.pack "success: ")
                yield json
                yield (SBS.pack "\n")
                loop
            Just (Original l) -> do
                yield (SBS.pack "fail - original: ")
                yield l
                yield (SBS.pack "\n")
                loop
            Nothing -> return ()


--------------------------------------------------------------------------------
main :: IO ()
main = do
    options <- OA.execParser parserInfo

    when (oVersion options) $ do
        putStrLn $ showVersion Paths_hnormalise.version
        exitSuccess

    config <- loadConfig (oConfigFilePath options)

    let lHost = case listenHost config of
                    Just h  -> T.unpack h
                    Nothing -> "*"

    runTCPServer (serverSettings (fromJust $ listenPort config) "*") $ \appData ->
        case oTestFilePath options of
            Nothing ->
                runTCPClient (clientSettings (fromJust $ successPort config) "localhost") $ \successServer ->
                runTCPClient (clientSettings (fromJust $ failPort config) "localhost") $ \failServer ->
                    appSource appData
                        $= DCT.decode DCT.utf8
                        $= DCT.lines
                        $= C.map (normalise $ oJsonInput options)
                        $$ messageSink successServer failServer
            Just testSinkFileName ->
                runResourceT$ appSource appData
                    $= DCT.decode DCT.utf8
                    $= DCT.lines
                    $= C.map (normalise $ oJsonInput options)
                    $= mySink
                    $$ sinkFile testSinkFileName
