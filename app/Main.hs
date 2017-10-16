{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent.Chan
import           Control.Concurrent.Extra
import           Control.Concurrent.MVar      (modifyMVar_, newMVar, newEmptyMVar, putMVar, readMVar, tryTakeMVar, withMVar, takeMVar, MVar)
import           Control.Concurrent.QSem
import           Control.DeepSeq
import           Control.Exception            (evaluate)
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (whileM_, untilM_)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)
import           Data.Aeson
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8        as SBS
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Conduit
import           Data.Conduit.Async
import           Data.Conduit.Binary          (sinkFile)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as C
import           Data.Conduit.Network
import qualified Data.Conduit.Text            as DCT
import qualified Data.Conduit.ZMQ4            as ZMQC
import           Data.Maybe                   (fromJust, isJust, fromMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time
import           Data.Time.Format
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import qualified Paths_hnormalise
import           System.Exit                  (exitFailure, exitSuccess)
import           System.Posix.Signals         (installHandler, Handler(CatchOnce), sigINT, sigTERM)
import qualified System.ZMQ4                  as ZMQ (withContext, withSocket, bind, send, receive, connect, Push(..), Pull(..))
import           Text.Printf                  (printf)

import           Debug.Trace
--------------------------------------------------------------------------------
import           HNormalise
import           HNormalise.Config            ( Config (..)
                                              , ConnectionType(..)
                                              , InputConfig(..)
                                              , LoggingConfig(..)
                                              , OutputConfig(..)
                                              , TcpOutputConfig(..)
                                              , TcpPortConfig(..)
                                              , ZeroMQOutputConfig(..)
                                              , ZeroMQPortConfig(..)
                                              , connectionType
                                              , defaultLoggingFrequency
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
parserOptions :: OA.Parser Main.Options
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
parserInfo :: OA.ParserInfo Main.Options
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
messageSink success failure messageCount frequency = loop
  where
    loop = do
        v <- await
        case v of
            Just n -> case enc n of
                (Transformed json) -> do
                    Data.Conduit.yield json $$ success
                    Data.Conduit.yield (SBS.pack "\n") $$ success
                    liftIO $ increaseCount (1, 0) messageCount frequency
                    loop
                (Original l) -> do
                    Data.Conduit.yield l $$ failure
                    Data.Conduit.yield (SBS.pack "\n") $$ failure
                    liftIO $ increaseCount (0, 1) messageCount frequency
                    loop
            Nothing -> return ()

increaseCount (s, f) messageCount frequency = do
    (s', f') <- takeMVar messageCount
    when ((s' + f') `mod` frequency == 0) $ do
        epoch_int <- (read . formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
        printf "%ld - message count: %10d (success: %10d, fail: %10d)\n" epoch_int (s' + f') s' f'
    putMVar messageCount (s' + s, f' + f)

--------------------------------------------------------------------------------
-- | 'mySink' yields the results downstream with the addition of a string mentioning success or failure
-- for testing purposes
mySink messageCount frequency = loop
  where
    loop = do
        v <- await
        case v of
            Just (Transformed json) -> do
                Data.Conduit.yield (SBS.pack "success: ")
                Data.Conduit.yield json
                Data.Conduit.yield (SBS.pack "\n")
                -- liftIO $ increaseCount (1, 0) messageCount frequency
                loop
            Just (Original l) -> do
                Data.Conduit.yield (SBS.pack "fail - original: ")
                Data.Conduit.yield l
                Data.Conduit.yield (SBS.pack "\n")
                --liftIO $ increaseCount (0, 1) messageCount frequency
                loop
            Nothing -> return ()

--------------------------------------------------------------------------------
{-runTCPConnection options config messageCount = do
    let listenHost = fromJust $ input config >>= (\(InputConfig t _) -> t) >>= (\(TcpPortConfig h p) -> h)
    let listenPort = fromJust $ input config >>= (\(InputConfig t _) -> t) >>= (\(TcpPortConfig h p) -> p)
    let frequency = fromMaybe defaultLoggingFrequency (logging config >>= (\(LoggingConfig f) -> f))
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
                        runResourceT $ appSource appData
                            =$=& normalisationConduit options config
                            =$$& messageSink (appSink successServer) (appSink failServer) messageCount frequency
            Just testSinkFileName -> runResourceT $ appSource appData
                =$=& normalisationConduit options config
                =$=& mySink messageCount frequency
                $$& sinkFile testSinkFileName
-}
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
        (liftIO (ZMQ.receive s) >>= Data.Conduit.yield)
    liftIO $ putStrLn "Done!"

pipelineC :: Int -> Consumer o (ResourceT IO) r -> Consumer o (ResourceT IO) r
pipelineC buffer sink = do
    sem <- liftIO $ newQSem buffer  -- how many are in flow, to avoid memory leaks
    chan <- liftIO newChan          -- the items in flow (type o)
    bar <- liftIO newBarrier        -- the result type (type r)
    me <- liftIO myThreadId
    liftIO $ printf "normalising thread has tid %s\n" (show me)
    liftIO $ flip forkFinally (either (throwTo me) (signalBarrier bar)) $ do
        t <- myThreadId
        printf "sinking thread has tid %s\n" (show t)
        runResourceT . runConduit $
            whileM (do
                x <- liftIO $ readChan chan
                liftIO $ signalQSem sem
                whenJust x Data.Conduit.yield
                return $ isJust x) =$=
            sink
    awaitForever $ \x -> liftIO $ do
        waitQSem sem
        writeChan chan $ Just x
    liftIO $ writeChan chan Nothing
    liftIO $ waitBarrier bar

enc v = case v of
    Left l -> Original l
    Right n -> Transformed $ encodeNormalisedRsyslog n
{-# INLINE enc #-}

--------------------------------------------------------------------------------
normalisationConduit options config nCount =
    let fs = fields config
    in if oJsonInput options
        then undefined -- CB.lines $= C.map (normaliseJsonInput fs)
        else CB.lines $= count fs -- $= C.map (normaliseText fs)
  where count fs = do
            m <- await
            case m of
                Just m' -> do
                    --v <- liftIO $ takeMVar nCount
                    --when (v `mod` 10000 == 0) $ liftIO $ printf "normalised: %d messages\n" v
                    --liftIO $ putMVar nCount (v+1)
                    m'' <- liftIO $ evaluate $ force (normaliseText fs) m'
                    Data.Conduit.yield m''
                    count fs
                Nothing -> return ()

--------------------------------------------------------------------------------
runZeroMQConnection :: Main.Options
                    -> Config
                    -> MVar a1
                    -> MVar (Int, Int)
                    -> Verbose
                    -> IO ()
runZeroMQConnection options config s_interrupted messageCount verbose' = do
    let fs = fields config
    let listenHost = fromJust $ input config >>= (\(InputConfig _ z) -> z) >>= (\(ZeroMQPortConfig m h p) -> h)
    let listenPort = fromJust $ input config >>= (\(InputConfig _ z) -> z) >>= (\(ZeroMQPortConfig m h p) -> p)
    let frequency = fromMaybe defaultLoggingFrequency (logging config >>= (\(LoggingConfig f) -> f))
    nCount <- newMVar (0 :: Int)

    case oTestFilePath options of
        Nothing -> ZMQ.withContext $ \ctx ->
                ZMQ.withSocket ctx ZMQ.Pull $ \s -> do
                    ZMQ.connect s $ printf "tcp://%s:%d" listenHost listenPort
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


                        runResourceT $ zmqInterruptibleSource s_interrupted s
                            $= normalisationConduit options config nCount
                            $$ pipelineC 100 (messageSink (ZMQC.zmqSink successSocket []) (ZMQC.zmqSink failureSocket []) messageCount frequency)

        Just testSinkFileName ->
            ZMQ.withContext $ \ctx ->
                ZMQ.withSocket ctx ZMQ.Pull $ \s -> do
                    ZMQ.connect s $ printf "tcp://%s:%d" listenHost listenPort
                    verbose' $ printf "Listening on tcp://%s:%d" listenHost listenPort
                    runResourceT $ zmqInterruptibleSource s_interrupted s
                        $= normalisationConduit options config nCount
                        $$ pipelineC 100 (let sf = sinkFile testSinkFileName in messageSink sf sf messageCount frequency)

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
    messageCount <- newMVar ((0,0) :: (Int, Int))
    case connectionType config of
 --       TCP    -> void $ runTCPConnection options config messageCount
        ZeroMQ -> do
            verbose' "ZeroMQ connections"
            s_interrupted <- newEmptyMVar
            installHandler sigINT (CatchOnce $ handler s_interrupted) Nothing
            verbose' "SIGINT handler installed"
            installHandler sigTERM (CatchOnce $ handler s_interrupted) Nothing
            verbose' "SIGTERM handler installed"
            runZeroMQConnection options config s_interrupted messageCount verbose'

    exitSuccess
