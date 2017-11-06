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

module HNormalise.Communication.Input.ZeroMQ
    ( zmqInterruptibleSource
    ) where


import           Control.Concurrent.MVar      (modifyMVar_, newMVar, newEmptyMVar, putMVar, readMVar, tryTakeMVar, withMVar, takeMVar, MVar)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (whileM_, untilM_)
import           Data.Conduit
import qualified System.ZMQ4                  as ZMQ (withContext, withSocket, bind, send, receive, connect, Push(..), Pull(..))

--------------------------------------------------------------------------------
import HNormalise.Config

--------------------------------------------------------------------------------
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
        (liftIO (ZMQ.receive s) >>= yield)
    liftIO $ putStrLn "Done!"
