{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.IO.Class       ( liftIO, MonadIO )
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8   as BS
import qualified Data.ByteString.Char8        as SBS
import           Data.Conduit
import           Data.Conduit.Binary          (sinkFile)
import qualified Data.Conduit.Combinators     as C
import qualified Data.Conduit.Binary          as CB
import           Data.Conduit.Network
import           Data.Monoid                  (mempty, (<>))
import qualified Data.Text.Encoding           as TE
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import qualified Paths_hnormalise
import           System.Exit                  (exitFailure, exitSuccess)
--------------------------------------------------------------------------------
import HNormalise                             (normaliseRsyslog)
import HNormalise.Rsyslog.Json
import HNormalise.Config                      (loadConfig)

--------------------------------------------------------------------------------
data Options = Options
    { oConfigFilePath :: !(Maybe FilePath)
    , oVersion :: !Bool
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
            OA.long "version" <>
            OA.short 'v' <>
            OA.help "Display version and exit" <>
            OA.hidden)


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> parserOptions)
    (OA.fullDesc
        <> OA.progDesc "Normalise rsyslog messages"
        <> OA.header ("hNormalise v" <> showVersion Paths_hnormalise.version)
    )

--------------------------------------------------------------------------------
data Normalised = Transformed SBS.ByteString
                | Original SBS.ByteString

--------------------------------------------------------------------------------
normalise :: SBS.ByteString  -- information arrives as a string representing JSON information
          -> Normalised
normalise logLine =
    case Data.Aeson.decodeStrict logLine >>= normaliseRsyslog of
        Just j  -> Transformed j
        Nothing -> Original logLine

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
mySink p = loop
  where
    s = sinkFile p
    loop = do
        v <- await
        case v of
            Just (Transformed json) -> do
                yield (SBS.pack "success") $$ s
                --yield (SBS.snoc json '\n') $$ s
                loop
            Just (Original l) -> do
                yield (SBS.pack "fail") $$ s
                --yield (SBS.snoc l '\n') $$ s
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

    runTCPServer (serverSettings 4000 "*") $ \appData ->
        --runTCPClient (clientSettings 4017 "localhost") $ \successServer ->
        --runTCPClient (clientSettings 4018 "localhost") $ \failServer -> do
        --appSource appData $= CB.lines $= C.map normalise $$ messageSink successServer failServer
        runResourceT$ appSource appData $= CB.lines $= C.map normalise $$ mySink "/tmp/test"
