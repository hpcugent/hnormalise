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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HNormalise.Lmod.Internal where

--------------------------------------------------------------------------------
import           Control.DeepSeq  (NFData)
import           Data.Text
import           GHC.Generics     (Generic)
--------------------------------------------------------------------------------

import           HNormalise.Torque.Internal (TorqueJobName)

data LmodParseResult
    = LmodLoadParse LmodLoad
    | LmodCommandParse LmodCommand
    deriving (Eq, Show, Generic)

data LmodModule = LmodModule
    { name    :: !Text
    , version :: !Text
    } deriving (Show, Eq, Generic)

data LmodJobId = LmodSlurmJobId { number:: !Int }
               | LmodTorqueJobId TorqueJobName
    deriving (Show, Eq, Generic)

data LmodInfo = LmodInfo
    { username :: !Text
    , cluster  :: !Text
    , jobid    :: !(Maybe LmodJobId)
    } deriving (Show, Eq, Generic)

data LmodLoad = LmodLoad
    { info     :: !LmodInfo
    , userload :: !Bool
    , modul    :: !LmodModule
    , filename :: !Text
    } deriving (Show, Eq, Generic)

data LmodCommand = LmodCommand
    { info      :: !LmodInfo
    , command   :: !Text
    , arguments :: !Text
    } deriving (Eq, Show, Generic)

instance NFData LmodModule
instance NFData LmodInfo
instance NFData LmodLoad
instance NFData LmodCommand
instance NFData LmodParseResult
instance NFData LmodJobId
