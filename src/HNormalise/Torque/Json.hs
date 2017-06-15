{- hnormalise - a log normalisation library
 -
 - Copyright Andy Georges (c) 2017
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module HNormalise.Torque.Json where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Monoid                 ((<>))

--------------------------------------------------------------------------------
import           HNormalise.Torque.Internal

--------------------------------------------------------------------------------

instance ToJSON TorqueJobShortNode where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobFQNode where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueExecHost where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueWalltime where
    toEncoding (TorqueWalltime d h m s) = toEncoding $ (((d * 24 + h) * 60) + m) * 60 + s

instance ToJSON TorqueJobNode where
    toEncoding (TSN n) = toEncoding n
    toEncoding (TFN ns) = toEncoding ns

instance ToJSON TorqueResourceRequest where
    toEncoding = genericToEncoding defaultOptions
{-    toEncoding (TorqueResourceRequest mem advres naccesspolicy ncpus neednodes nice nodeCount nodes select qos pmem vmem pvmem walltime) =
        pairs (  "mem" .= mem
              <> "advres" .= advres
              <> "naccesspolicy" .= naccesspolicy
              <> "ncpus" .= ncpus
              <> "neednodes" .= case neednodes of
                                    Left n   -> toEncoding n
                                    Right ns -> toEncoding ns
              <> "nice" .= nice
              <> "nodeCount" .= nodeCount
              <> "nodes" .= case nodes of
                                Left n   -> toEncoding n
                                Right ns -> toEncoding ns
              <> "select" .= select
              <> "qos" .= qos
              <> "pmem" .= pmem
              <> "vmem" .= vmem
              <> "pvmem" .= pvmem
              <> "walltime" .= walltime)
-}
instance ToJSON TorqueResourceUsage where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobTime where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobExit where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobName where
    toEncoding = genericToEncoding defaultOptions
