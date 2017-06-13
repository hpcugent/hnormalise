{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent.MVar      (modifyMVar_, newMVar, withMVar, MVar)
import           Control.Monad
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (whileM_)
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
import           System.Posix.Signals         (installHandler, Handler(Catch), sigINT, sigTERM)
import qualified System.ZMQ4                  as ZMQ (Receiver, Sender, Socket)
import qualified System.ZMQ4.Monad            as SMM (makeSocket, runZMQ, bind, connect, receive, Pull(..), Push(..))
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
-- | 'handler' will catch SIGTERM and modify the MVar to allow the upstream
-- connection to be closed cleanly when using ZeroMQ. Code taken from
-- http://zguide.zeromq.org/hs:interrupt
handler :: MVar Int -> IO ()
handler s_interrupted = trace "Interrupt received" $ modifyMVar_ s_interrupted (return . (+1))

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
                trace "successfull parse" $ yield (SBS.pack "success: ")
                yield json
                yield (SBS.pack "\n")
                loop
            Just (Original l) -> do
                trace "failed parse" $ yield (SBS.pack "fail - original: ")
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
                        let normalisationConduit = case oJsonInput options of
                                                        True  -> CB.lines $= C.map (normaliseJsonInput fs)
                                                        False -> DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                        in appSource appData
                            $= normalisationConduit
                            $$ messageSink (appSink successServer) (appSink failServer)
            Just testSinkFileName ->
                let normalisationConduit = case oJsonInput options of
                                                True  -> CB.lines $= C.map (normaliseJsonInput fs)
                                                False -> DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                in runResourceT $ appSource appData
                    $= normalisationConduit
                    $= mySink
                    $$ sinkFile testSinkFileName


zmqInterruptibleSource :: (ZMQ.Receiver s, Ord a, Num a) => MVar a -> ZMQ.Socket s -> Source IO SBS.ByteString
zmqInterruptibleSource m s = whileM_
    (liftIO $ withMVar m (\val -> if val > 0 then return False else return True))
    (lift (SMM.receive s) >>= yield)

--------------------------------------------------------------------------------
runZeroMQConnection options config s_interrupted = do
    runResourceT $ SMM.runZMQ 1 $ do

        let fs = fields config

        let listenHost = fromJust $ input config >>= (\(InputConfig _ z) -> z) >>= (\(ZeroMQPortConfig m h p) -> h)
        let listenPort = fromJust $ input config >>= (\(InputConfig _ z) -> z) >>= (\(ZeroMQPortConfig m h p) -> h)

        -- 0mq socket to bind for the upstream input
        s <- SMM.makeSocket SMM.Pull
        SMM.bind s $ printf "tcp://%s:%d" listenHost listenPort

        --((\v -> trace ("listening on port: " ++ show v) v)(fromJust $ ports config >>= listenPort))

        case oTestFilePath options of
            Nothing -> do
                    -- 0mq sockets to connect to the downstream outputs
                    let successHost = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> s) >>= (\(ZeroMQPortConfig m h p) -> h)
                    let successPort = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> s) >>= (\(ZeroMQPortConfig m h p) -> p)
                    successSocket <- SMM.makeSocket SMM.Push
                    SMM.connect successSocket $ printf "tcp://%s:%d" successHost successPort

                    let failureHost = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> f) >>= (\(ZeroMQPortConfig m h p) -> h)
                    let failurePort = fromJust $ output config >>= (\(OutputConfig _ z) -> z) >>= (\(ZeroMQOutputConfig s f) -> f) >>= (\(ZeroMQPortConfig m h p) -> p)
                    failSocket <- SMM.makeSocket SMM.Push
                    SMM.connect failSocket $ printf "tcp://%s:%d"  failureHost failurePort

                    let normalisationConduit = case oJsonInput options of
                                                    True  -> CB.lines $= C.map (normaliseJsonInput fs)
                                                    False -> DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                    liftIO $ zmqInterruptibleSource s_interrupted s
                        $= normalisationConduit
                        $$ messageSink (ZMQC.zmqSink successSocket []) (ZMQC.zmqSink failSocket [])

            {- FIXME: I cannot get the types for this part right.
            Just testSinkFileName ->
                let normalisationConduit = case oJsonInput options of
                                                True  -> CB.lines $= C.map (normaliseJsonInput fs)
                                                False -> DCT.decode DCT.utf8 $= DCT.lines $= C.map (normaliseText fs)
                in ZMQC.zmqSource s
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

    config <- loadConfig (oConfigFilePath options)
    trace (show config) $ return ()
    -- install the signal handlers for a clean shutdown


    -- For now, we only support input and output configurations of the same type,
    -- i.e., both TCP, both ZeroMQ, etc.
    case connectionType config of
        TCP    -> void $ runTCPConnection options config
        ZeroMQ -> do
            s_interrupted <- newMVar 0
    --        installHandler sigINT (Catch $ handler s_interrupted) Nothing
    --        installHandler sigTERM (Catch $ handler s_interrupted) Nothing
            runZeroMQConnection options config s_interrupted

    exitSuccess
