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

module HNormalise.Torque.Json where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Monoid                ((<>))

--------------------------------------------------------------------------------
import           HNormalise.Torque.Internal

--------------------------------------------------------------------------------
instance  ToJSON TorqueEntryType where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobShortNode where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobFQNode where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueExecHost where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueWalltime where
    toEncoding (TorqueWalltime d h m s) = toEncoding $ (((d * 24 + h) * 60) + m) * 60 + s

instance ToJSON TorqueJobNode where
    toEncoding (TSN n)  = toEncoding n
    toEncoding (TFN ns) = toEncoding ns

instance ToJSON TorqueResourceRequest where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueResourceUsage where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobTime where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobExit where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobName where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueRequestor where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobStart where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobQueue where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobDelete where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobAbort where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobRerun where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueParseResult where
    toEncoding (TorqueQueue ts)  = toEncoding ts
    toEncoding (TorqueStart ts)  = toEncoding ts
    toEncoding (TorqueDelete ts) = toEncoding ts
    toEncoding (TorqueExit ts)   = toEncoding ts
    toEncoding (TorqueAbort ts)  = toEncoding ts
    toEncoding (TorqueRerun ts)  = toEncoding ts
