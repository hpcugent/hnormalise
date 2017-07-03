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

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Torque.Internal where

--------------------------------------------------------------------------------
import           Data.Text
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------
data TorqueJobShortNode = TorqueJobShortNode
    { number :: !Int
    , ppn    :: !(Maybe Int)
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobFQNode = TorqueJobFQNode
    { name :: !Text
    , ppn  :: !Int
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobNode = TSN TorqueJobShortNode
                   | TFN [TorqueJobFQNode]
                   deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueExecHost = TorqueExecHost
    { name      :: !Text
    , cores     :: ![Int]
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueWalltime = TorqueWalltime
    { days    :: !Int
    , hours   :: !Int
    , minutes :: !Int
    , seconds :: !Int
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueResourceRequest = TorqueResourceRequest
    { mem           :: !(Maybe Integer)
    , advres        :: !(Maybe Text)
    , naccesspolicy :: !(Maybe Text)
    , ncpus         :: !(Maybe Int)
    , neednodes     :: !TorqueJobNode
    , nice          :: !(Maybe Int)
    , nodeCount     :: !Int
    , nodes         :: !TorqueJobNode
    , select        :: !(Maybe Text)
    , qos           :: !(Maybe Text)
    , pmem          :: !(Maybe Integer)
    , vmem          :: !(Maybe Integer)
    , pvmem         :: !(Maybe Integer)
    , walltime      :: !TorqueWalltime
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueResourceUsage = TorqueResourceUsage
    { cputime  :: !Integer
    , energy   :: !Integer
    , mem      :: !Integer
    , vmem     :: !Integer
    , walltime :: !TorqueWalltime
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobTime = TorqueJobTime
    { ctime     :: !Integer
    , qtime     :: !Integer
    , etime     :: !Integer
    , startTime :: !Integer
    , endTime   :: !Integer
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobExit = TorqueJobExit
    { name                :: !TorqueJobName
    , user                :: !Text
    , group               :: !Text
    , jobname             :: !Text
    , queue               :: !Text
    , startCount          :: !(Maybe Int)
    , owner               :: !Text
    , session             :: !Integer
    , times               :: !TorqueJobTime
    , execHost            :: ![TorqueExecHost]
    , resourceRequest     :: !TorqueResourceRequest
    , resourceUsage       :: !TorqueResourceUsage
    , totalExecutionSlots :: !Int
    , uniqueNodeCount     :: !Int
    , exitStatus          :: !Int
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobName = TorqueJobName
    { number   :: !Integer
    , array_id :: !(Maybe Integer)
    , master   :: !Text
    , cluster  :: !Text
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobQueue = TorqueJobQueue
    { name  :: !TorqueJobName
    , queue :: !Text
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobStart = TorqueJobStart
    { name                :: !TorqueJobName
    , user                :: !Text
    , group               :: !Text
    , jobname             :: !Text
    , queue               :: !Text
    , owner               :: !Text
    , times               :: !TorqueJobTime
    , execHost            :: ![TorqueExecHost]
    , resourceRequest     :: !TorqueResourceRequest
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueRequestor = TorqueRequestor
    { user     :: !Text
    , whence   :: !Text
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobDelete = TorqueJobDelete
    { name      :: !TorqueJobName
    , requestor :: !TorqueRequestor
    } deriving (Show, Eq, Generic)
