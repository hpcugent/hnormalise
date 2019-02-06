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

{-# LANGUAGE OverloadedStrings #-}

module HNormalise.Communication.ZeroMQ
    ( zmqInterruptibleSource
    , runZeroMQConnection
    ) where


import           Control.Concurrent.MVar      (MVar, modifyMVar_, newEmptyMVar,
                                               newMVar, putMVar, readMVar,
                                               takeMVar, tryTakeMVar, withMVar)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (untilM_, whileM_)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Conduit
import qualified Data.Conduit.ZMQ4            as ZMQC
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import qualified System.ZMQ4                  as ZMQ (Pull (..), Push (..),
                                                      bind, connect, receive,
                                                      send, withContext,
                                                      withSocket)
import           Text.Printf                  (printf)

--------------------------------------------------------------------------------
import           HNormalise.Config            (ZeroMQOutputConfig (..),
                                               ZeroMQPortConfig (..))
import           HNormalise.Util
import           HNormalise.Verbose


--------------------------------------------------------------------------------
-- | 'zmqInterruptibleSource' converts a regular 0mq recieve operation on a socket into a conduit source
-- The source is halted when something is put into the MVar, effectively stopping the program from checking
-- for new incoming messages.
zmqInterruptibleSource m s = do
    whileM_
        (liftIO $ do
            val <- tryTakeMVar m
            case val of
                Just _  ->  return False
                Nothing -> return True)
        (liftIO (ZMQ.receive s) >>= yield)
    liftIO $ putStrLn "Done!"


--------------------------------------------------------------------------------
runZeroMQConnection :: Maybe [(Text, Text)]
                    -> Int
                    -> ZeroMQPortConfig
                    -> ZeroMQOutputConfig
                    -> MVar a1
                    -> MVar (Int, Int)
                    -> Verbose
                    -> IO ()
runZeroMQConnection fields frequency (ZeroMQPortConfig _ listenHost listenPort) (ZeroMQOutputConfig success failure) s_interrupted messageCount verbose' = do
    let successHost = fromJust $ success >>= host
    let successPort = fromJust $ success >>= port
    let failureHost = fromJust $ failure >>= host
    let failurePort = fromJust $ failure >>= port

    ZMQ.withContext $ \ctx ->
        ZMQ.withSocket ctx ZMQ.Pull $ \s -> do
            ZMQ.connect s $ printf "tcp://%s:%d" (fromJust listenHost) (fromJust listenPort)
            verbose' $ printf "Listening on tcp://%s:%d" (fromJust listenHost) (fromJust listenPort)

            ZMQ.withSocket ctx ZMQ.Push $ \successSocket ->
                ZMQ.withSocket ctx ZMQ.Push $ \failureSocket -> do
                    ZMQ.connect successSocket $ printf "tcp://%s:%d" successHost successPort
                    verbose' $ printf "Pushing successful parses on tcp://%s:%d" successHost successPort

                    ZMQ.connect failureSocket $ printf "tcp://%s:%d" failureHost failurePort
                    verbose' $ printf "Pushing failed parses on tcp://%s:%d" failureHost failurePort

                    runResourceT $ zmqInterruptibleSource s_interrupted s
                        $= normalisationConduit fields
                        $$ pipelineC 100 (messageSink (ZMQC.zmqSink successSocket []) (ZMQC.zmqSink failureSocket []) messageCount frequency)


{- FIXME: This is the sink to a file code and should be put in another runwrapper.
        Just testSinkFileName ->
            ZMQ.withContext $ \ctx ->
                ZMQ.withSocket ctx ZMQ.Pull $ \s -> do
                    ZMQ.connect s $ printf "tcp://%s:%d" listenHost listenPort
                    verbose' $ printf "Listening on tcp://%s:%d" listenHost listenPort
                    runResourceT $ zmqInterruptibleSource s_interrupted s
                        $= normalisationConduit options config
                        $$ pipelineC 100 (let sf = sinkFile testSinkFileName in messageSink sf sf messageCount frequency)
-}
