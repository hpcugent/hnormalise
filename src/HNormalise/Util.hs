{- hnormalise - a log normalisation library
 -
 - Copyright Ghent University (c) 2017-2019
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

module HNormalise.Util
    ( messageSink
    , normalisationConduit
    , pipelineC
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.Chan
import           Control.Concurrent.Extra
import           Control.DeepSeq
import           Control.Exception            (evaluate)
import           Control.Monad.Extra
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (untilM_, whileM_)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Char8        as SBS
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as C
import           Data.Conduit.Network
import qualified Data.Conduit.Text            as DCT
import           Data.Maybe                   (fromJust, isJust)
import           Data.Time
import           Data.Time.Format
import           Text.Printf                  (printf)

--------------------------------------------------------------------------------
import           HNormalise
import           HNormalise.Json

--------------------------------------------------------------------------------
-- | 'messageSink' yields the parsed JSON downstream, or if parsing fails, yields the original message downstream
messageSink success failure messageCount frequency = loop
  where
    loop = do
        v <- await
        case v of
            Just n -> case n of
                Right json -> do
                    Data.Conduit.yield (encodeNormalisedRsyslog json) $$ success
                    Data.Conduit.yield (SBS.pack "\n") $$ success
                    liftIO $ increaseCount (1, 0) messageCount frequency
                    loop
                Left l -> do
                    Data.Conduit.yield l $$ failure
                    Data.Conduit.yield (SBS.pack "\n") $$ failure
                    liftIO $ increaseCount (0, 1) messageCount frequency
                    loop
            Nothing -> return ()

--------------------------------------------------------------------------------
increaseCount (s, f) messageCount frequency = do
    (s', f') <- takeMVar messageCount
    when ((s' + f') `mod` frequency == 0) $ do
        epoch_int <- (read . formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
        printf "%ld - message count: %10d (success: %10d, fail: %10d)\n" epoch_int (s' + f') s' f'
    putMVar messageCount (s' + s, f' + f)

--------------------------------------------------------------------------------
-- The following code was adapted from https://github.com/ndmitchell/hoogle
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

--------------------------------------------------------------------------------
normalisationConduit fields = CB.lines $= normalise fields
  where normalise fs = do
            m <- await
            case m of
                Just m' -> do
                    m'' <- liftIO $ evaluate $ force (normaliseText fs) m'
                    Data.Conduit.yield m''
                    normalise fs
                Nothing -> return ()

