{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent.MVar      (modifyMVar_, newMVar, newEmptyMVar, putMVar, readMVar, tryTakeMVar, withMVar, MVar)
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (whileM_, untilM_)
import           Control.Monad.Trans          (lift)
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
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import qualified Paths_hnormalise
import           System.Exit                  (exitFailure, exitSuccess)
import           System.Posix.Signals         (installHandler, Handler(CatchOnce), sigINT, sigTERM)
import qualified System.ZMQ4                  as ZMQ (Receiver, Sender, Socket, Size, context, term, setIoThreads, Context)
import qualified System.ZMQ4                  as ZMQ (withContext, withSocket, bind, send, receive, connect, Push(..), Pull(..))
import           Text.Printf                  (printf)

import           Debug.Trace
--------------------------------------------------------------------------------
import           HNormalise
import           HNormalise.Config            ( Config (..)
                                              , ConnectionType(..)
                                              , InputConfig(..)
                                              , OutputConfig(..)
                                              , TcpOutputConfig(..)
                                              , TcpPortConfig(..)
                                              , ZeroMQOutputConfig(..)
                                              , ZeroMQPortConfig(..)
                                              , connectionType
                                              , loadConfig)
import           HNormalise.Internal          (Rsyslog (..))
import           HNormalise.Json
import           HNormalise.Verbose

--------------------------------------------------------------------------------
data Options = Options
    { oConfigFilePath :: !(Maybe FilePath)
    , oJsonInput      :: !Bool
    , oVersion        :: !Bool
    , oTestFilePath   :: !(Maybe FilePath)
    , oVerbose        :: !Bool
    } deriving (Show)

--------------------------------------------------------------------------------
parserOptions :: OA.Parser Options
parserOptions = Options
    <$> OA.optional ( OA.strOption $
            OA.long "configfile" <>
            OA.short 'c' <>
            OA.metavar "FILENAME" <>
            OA.help "configuration file location ")
    <*> OA.switch (
            OA.long "jsoninput" <>
            OA.short 'j' <>
            OA.help "Input will be delivered as JSON (slower)")
    <*> OA.switch (
            OA.long "version" <>
            OA.short 'v' <>
            OA.help "Display version and exit" <>
            OA.hidden)
    <*> OA.optional ( OA.strOption $
            OA.long "test" <>
            OA.short 't' <>
            OA.metavar "OUTPUT FILENAME" <>
            OA.help "run in test modus, sinking output to the given file")
    <*> (OA.switch $
            OA.long "verbose" <>
            OA.help "Verbose output" <>
            OA.hidden)


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> parserOptions)
    (OA.fullDesc
        <> OA.progDesc "Normalise rsyslog messages"
        <> OA.header ("hNormalise v" <> showVersion Paths_hnormalise.version)
    )

--------------------------------------------------------------------------------
-- | 'handler' will catch SIGTERM and modify the MVar to allow the upstream
-- connection to be closed cleanly when using ZeroMQ. Code taken from
-- http://zguide.zeromq.org/hs:interrupt
handler :: MVar () -> IO ()
handler s_interrupted = do
    putStrLn "Interrupt received"
    putMVar s_interrupted ()

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
runTCPConnection options config = do
    let listenHost = fromJust $ input config >>= (\(InputConfig t _) -> t) >>= (\(TcpPortConfig h p) -> h)
    let listenPort = fromJust $ input config >>= (\(InputConfig t _) -> t) >>= (\(TcpPortConfig h p) -> p)
    runTCPServer (serverSettings listenPort "*") $ \appData -> do
        let fs = fields config
        case oTestFilePath options of
            Nothing -> do
                let successHost = TE.encodeUtf8 $ fromJust $ output config >>= (\(OutputConfig t _) -> t) >>= (\(TcpOutputConfig s f) -> s) >>= (\(TcpPortConfig h p) -> h)
                let successPort = fromJust $ output config >>= (\(OutputConfig t _) -> t) >>= (\(TcpOutputConfig s f) -> s) >>= (\(TcpPortConfig h p) -> p)
                let failureHost = TE.encodeUtf8 $ fromJust $ output config >>= (\(OutputConfig t _) -> t) >>= (\(TcpOutputConfig s f) -> f) >>= (\(TcpPortConfig h p) -> h)
                let failurePort = fromJust $ output config >>= (\(OutputConfig t _) -> t) >>= (\(TcpOutputConfig s f) -> f) >>= (\(TcpPortConfig h p) -> p)

                runTCPClient (clientSettings successPort successHost) $ \successServer ->
                    runTCPClient (clientSettings failurePort failureHost) $ \failServer ->
                        let normalisationConduit = if oJsonInput options
                                                        then CB.lines $= C.map (normaliseJsonInput fs)
                                                        else DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                        in appSource appData
                            $= normalisationConduit
                            $$ messageSink (appSink successServer) (appSink failServer)
            Just testSinkFileName ->
                let normalisationConduit = if oJsonInput options
                                                then CB.lines $= C.map (normaliseJsonInput fs)
                                                else DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                in runResourceT $ appSource appData
                    $= normalisationConduit
                    $= mySink
                    $$ sinkFile testSinkFileName

-- | 'zmqInterruptibleSource' converts a regular 0mq recieve operation on a socket into a conduit source
-- The source is halted when something is put into the MVar, effectively stopping the program from checking
-- for new incoming messages.
zmqInterruptibleSource m s = do
    whileM_
        (liftIO $ do
            val <- tryTakeMVar m
            case val of
                Just _ ->  return False
                Nothing -> return True)
        (lift (ZMQ.receive s) >>= yield)
    liftIO $ putStrLn "Done!"

--------------------------------------------------------------------------------
normalisationConduit options config =
    let fs = fields config
    in if oJsonInput options
        then CB.lines $= C.map (normaliseJsonInput fs)
        else DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)


--------------------------------------------------------------------------------
runZeroMQConnection options config s_interrupted verbose' = do
    let fs = fields config
    let listenHost = fromJust $ input config >>= (\(InputConfig _ z) -> z) >>= (\(ZeroMQPortConfig m h p) -> h)
    let listenPort = fromJust $ input config >>= (\(InputConfig _ z) -> z) >>= (\(ZeroMQPortConfig m h p) -> p)

    case oTestFilePath options of
        Nothing -> ZMQ.withContext $ \ctx ->
                ZMQ.withSocket ctx ZMQ.Pull $ \s -> do
                    ZMQ.bind s $ printf "tcp://%s:%d" listenHost listenPort
                    verbose' $ printf "Listening on tcp://%s:%d" listenHost listenPort

                    let successHost = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> s) >>= (\(ZeroMQPortConfig m h p) -> h)
                    let successPort = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> s) >>= (\(ZeroMQPortConfig m h p) -> p)
                    let failureHost = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> f) >>= (\(ZeroMQPortConfig m h p) -> h)
                    let failurePort = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> f) >>= (\(ZeroMQPortConfig m h p) -> p)

                    ZMQ.withSocket ctx ZMQ.Push $ \successSocket ->
                      ZMQ.withSocket ctx ZMQ.Push $ \failureSocket -> do
                        ZMQ.connect successSocket $ printf "tcp://%s:%d" successHost successPort
                        verbose' $ printf "Pushing successful parses on tcp://%s:%d" successHost successPort

                        ZMQ.connect failureSocket $ printf "tcp://%s:%d" failureHost failurePort
                        verbose' $ printf "Pushing failed parses on tcp://%s:%d" failureHost failurePort

                        let normalisationConduit = if oJsonInput options
                                                        then CB.lines $= C.map (normaliseJsonInput fs)
                                                        else DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                        liftIO $ zmqInterruptibleSource s_interrupted s
                            $= normalisationConduit
                            $$ messageSink (ZMQC.zmqSink successSocket []) (ZMQC.zmqSink failureSocket [])

        {-Just testSinkFileName ->
            let normalisationConduit = case oJsonInput options of
                                            True  -> CB.lines $= C.map (normaliseJsonInput fs)
                                            False -> DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)

            in ZMQ.withContext $ \ctx -> do
                    ZMQ.withSocket ctx ZMQ.Pull $ \s -> do
                        ZMQ.bind s $ printf "tcp://%s:%d" listenHost listenPort
                        liftIO $ zmqInterruptibleSource s_interrupted s
                            $= normalisationConduit
                            $= mySink
                            $$ sinkFile testSinkFileName
-}

--------------------------------------------------------------------------------
-- | 'main' starts a TCP server, listening to incoming data and connecting to TCP servers downstream to
-- for the pipeline.
main :: IO ()
main = do
    options <- OA.execParser parserInfo

    when (oVersion options) $ do
        putStrLn $ showVersion Paths_hnormalise.version
        exitSuccess

    let verbose' = makeVerbose (oVerbose options)
    config <- loadConfig (oConfigFilePath options)

    verbose' $ "Config: " ++ show config

    -- For now, we only support input and output configurations of the same type,
    -- i.e., both TCP, both ZeroMQ, etc.
    case connectionType config of
        TCP    -> void $ runTCPConnection options config
        ZeroMQ -> do
            verbose' "ZeroMQ connections"
            s_interrupted <- newEmptyMVar
            installHandler sigINT (CatchOnce $ handler s_interrupted) Nothing
            verbose' "SIGINT handler installed"
            installHandler sigTERM (CatchOnce $ handler s_interrupted) Nothing
            verbose' "SIGTERM handler installed"
            runZeroMQConnection options config s_interrupted verbose'

    exitSuccess
