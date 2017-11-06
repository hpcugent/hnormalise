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

module HNormalise.Communication.Output.File where

import qualified Data.ByteString.Char8        as SBS
import qualified Data.ByteString.Lazy.Char8   as BS
import           Data.Conduit

--------------------------------------------------------------------------------
import           HNormalise.Json              (encodeNormalisedRsyslog)

--------------------------------------------------------------------------------
-- | 'mySink' yields the results downstream with the addition of a string mentioning success or failure
-- for testing purposes
mySink messageCount frequency = loop
  where
    loop = do
        v <- await
        case v of
            Just (Right json) -> do
                yield (SBS.pack "success: ")
                yield $ encodeNormalisedRsyslog json
                yield (SBS.pack "\n")
                -- liftIO $ increaseCount (1, 0) messageCount frequency
                loop
            Just (Left l) -> do
                yield (SBS.pack "fail - original: ")
                yield l
                yield (SBS.pack "\n")
                --liftIO $ increaseCount (0, 1) messageCount frequency
                loop
            Nothing -> return ()
