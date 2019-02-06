{- hnormalise - a log normalisation library
 -
 - Copyright Ghent University (c) 2017
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - * Redistributions of source code must retain the above copyright
 - notice, this list of conditions and the following disclaimer.
 -
 - * Redistributions in binary form must reproduce the above
 - copyright notice, this list of conditions and the following
 - disclaimer in the documentation and/or other materials provided
 - with the distribution.
 -
 - * Neither the name of Author name here nor the names of other
 - contributors may be used to endorse or promote products derived
 - from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 - "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 - LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 - A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 - OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative             ((<$>), (<*>))
import           Control.Concurrent.MVar         (MVar, newEmptyMVar, newMVar,
                                                  putMVar)
import           Control.Monad
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Loops             (untilM_, whileM_)
import           Control.Monad.Trans             (lift)
import           Data.Maybe
import           Data.Monoid                     ((<>))
import           Data.Version                    (showVersion)
import qualified Options.Applicative             as OA
import qualified Paths_hnormalise
import           System.Exit                     (exitFailure, exitSuccess)
import           System.Posix.Signals            (Handler (CatchOnce),
                                                  installHandler, sigINT,
                                                  sigTERM)
import           Text.Printf                     (printf)

--------------------------------------------------------------------------------
import           HNormalise
import           HNormalise.Communication.ZeroMQ (runZeroMQConnection,
                                                  zmqInterruptibleSource)
import           HNormalise.Config               (Config (..),
                                                  ConnectionType (..),
                                                  InputConfig (..),
                                                  LoggingConfig (..),
                                                  OutputConfig (..),
                                                  TcpOutputConfig (..),
                                                  TcpPortConfig (..),
                                                  ZeroMQOutputConfig (..),
                                                  ZeroMQPortConfig (..),
                                                  connectionType,
                                                  defaultLoggingFrequency,
                                                  loadConfig)
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
    messageCount <- newMVar ((0,0) :: (Int, Int))
    case connectionType config of
        ZeroMQ -> do
            verbose' "ZeroMQ connections"
            s_interrupted <- newEmptyMVar
            installHandler sigINT (CatchOnce $ handler s_interrupted) Nothing
            verbose' "SIGINT handler installed"
            installHandler sigTERM (CatchOnce $ handler s_interrupted) Nothing
            verbose' "SIGTERM handler installed"
            let frequency = fromMaybe defaultLoggingFrequency (logging config >>= (\(LoggingConfig f) -> f))
            let fs = fields config
            let zmqPorts = fromJust $ input config >>= (\(InputConfig _ z) -> z)
            let zmqOutput = fromJust $ output config >>= (\(OutputConfig _ z) -> z)
            runZeroMQConnection fs frequency zmqPorts zmqOutput s_interrupted messageCount verbose'

    exitSuccess
