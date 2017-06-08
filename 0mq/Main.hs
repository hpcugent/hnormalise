{-# LANGUAGE FlexibleContexts  #-}
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
import qualified Data.Conduit.Text            as DCT
import qualified Data.Conduit.ZMQ4            as ZMQC
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  (mempty, (<>))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import qualified Paths_hnormalise
import           System.Exit                  (exitFailure, exitSuccess)
import qualified System.ZMQ4.Monad            as SMM (makeSocket, runZMQ, bind, connect, Pull(..), Push(..))
import           Text.Printf                  (printf)

import           Debug.Trace
--------------------------------------------------------------------------------
import           HNormalise
import           HNormalise.Config            (Config (..), PortConfig(..), loadConfig)
import           HNormalise.Internal          (Rsyslog (..))
import           HNormalise.Json

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
-- | 'messageSink' yields the parsed JSON downstream, or if parsing fails, yields the original message downstream
messageSink success failure = loop
  where
    loop = do
        v <- await
        case v of
            Just (Transformed json) -> do
                yield json $$ success
                yield (SBS.pack "\n") $$ success
                loop
            Just (Original l) -> do
                yield l $$ failure
                yield (SBS.pack "\n") $$ failure
                loop
            Nothing -> return ()


--------------------------------------------------------------------------------
-- | 'mySink' yields the results downstream with the addition of a string mentioning success or failure
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
-- | 'main' starts a TCP server, listening to incoming data and connecting to TCP servers downstream to
-- for the pipeline.
main :: IO ()
main = do
    options <- OA.execParser parserInfo

    when (oVersion options) $ do
        putStrLn $ showVersion Paths_hnormalise.version
        exitSuccess

    config <- loadConfig (oConfigFilePath options)
    trace (show config) $ return ()

    let lHost = case ports config >>= listenHost of
                    Just h  -> T.unpack h
                    Nothing -> "*"

    let fs = fields config

    runResourceT $ SMM.runZMQ 1 $ do

        -- 0mq socket to bind for the upstream input
        s <- SMM.makeSocket SMM.Pull
        SMM.bind s "tcp://*:31338"

        -- 0mq sockets to connect to the downstream outputs
        successSocket <- SMM.makeSocket SMM.Push
        SMM.connect successSocket $ printf "tcp://%s:%d" (fromJust $ ports config >>= successHost) (fromJust $ ports config >>= successPort)

        failSocket <- SMM.makeSocket SMM.Push
        SMM.connect failSocket $ printf "tcp://%s:%d" (fromJust $ ports config >>= failHost) (fromJust $ ports config >>= failPort)

        case oTestFilePath options of
            Nothing ->
                    case oJsonInput options of
                        True  -> ZMQC.zmqSource s
                                    $= CB.lines
                                    $= C.map (normaliseJsonInput fs)
                                    $$ messageSink (ZMQC.zmqSink successSocket []) (ZMQC.zmqSink failSocket [])
                        False -> ZMQC.zmqSource s
                                    $= DCT.decode DCT.utf8
                                    $= DCT.lines
                                    $= C.map (normaliseText fs)
                                    $$ messageSink (ZMQC.zmqSink successSocket []) (ZMQC.zmqSink failSocket [])
            Just testSinkFileName ->
                ZMQC.zmqSource s
                    $= (case oJsonInput options of
                        True  -> CB.lines $= C.map (normaliseJsonInput fs)
                        False -> DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs))
                    $= mySink
                    $$ sinkFile testSinkFileName
