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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HNormalise.Torque.Internal where

--------------------------------------------------------------------------------
import           Control.DeepSeq  (NFData)
import           Data.Text
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- | `TorqueParseResult` encapsulates the results we get from parsing torque lines
data TorqueParseResult
    = TorqueQueue TorqueJobQueue
    | TorqueStart TorqueJobStart
    | TorqueDelete TorqueJobDelete
    | TorqueExit TorqueJobExit
    | TorqueAbort TorqueJobAbort
    | TorqueRerun TorqueJobRerun
    deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- | `TorqueEntryType` distinguishes between accounting data entry Types
data TorqueEntryType
    = TorqueQueueEntry
    | TorqueStartEntry
    | TorqueDeleteEntry
    | TorqueExitEntry
    | TorqueAbortEntry
    | TorqueRerunEntry
    deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobShortNode = TorqueJobShortNode
    { number :: !Int
    , ppn    :: !(Maybe Int)
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobFQNode = TorqueJobFQNode
    { name :: !Text
    , ppn  :: !(Maybe Int)
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobNode = TSN TorqueJobShortNode
                   | TFN [TorqueJobFQNode]
                   deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueExecHost = TorqueExecHost
    { name  :: !Text
    , cores :: ![Int]
    } deriving (Show, Eq, Generic)

instance Ord TorqueExecHost where
    compare (TorqueExecHost t1 _) (TorqueExecHost t2 _) = compare t1 t2

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
    , cputime       :: !(Maybe TorqueWalltime)
    , prologue      :: !(Maybe Text)
    , epilogue      :: !(Maybe Text)
    , neednodes     :: !(Maybe TorqueJobNode)
    , nice          :: !(Maybe Int)
    , nodeCount     :: !Int
    , nodes         :: !TorqueJobNode
    , select        :: !(Maybe Text)
    , qos           :: !(Maybe Text)
    , other         :: !(Maybe Text)
    , feature       :: !(Maybe Text)
    , host          :: !(Maybe Text)
    , procs         :: !(Maybe Text)
    , nodeset       :: !(Maybe Text)
    , tpn           :: !(Maybe Text)
    , pmem          :: !(Maybe Integer)
    , vmem          :: !(Maybe Integer)
    , pvmem         :: !(Maybe Integer)
    , mppmem        :: !(Maybe Integer)
    , walltime      :: !TorqueWalltime
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueResourceUsage = TorqueResourceUsage
    { cputime  :: !Integer
    , energy   :: !(Maybe Integer)
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
    , endTime   :: !(Maybe Integer)
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobExit = TorqueJobExit
    { torqueDatestamp     :: !Text
    , name                :: !TorqueJobName
    , user                :: !Text
    , group               :: !Text
    , account             :: !(Maybe Text)
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
    , torqueEntryType     :: TorqueEntryType
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobName = TorqueJobName
    { number  :: !Integer
    , arrayId :: !(Maybe Integer)
    , master  :: !Text
    , cluster :: !Text
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobQueue = TorqueJobQueue
    { torqueDatestamp :: !Text
    , name            :: !TorqueJobName
    , queue           :: !Text
    , torqueEntryType :: TorqueEntryType
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobStart = TorqueJobStart
    { torqueDatestamp :: !Text
    , name            :: !TorqueJobName
    , user            :: !Text
    , group           :: !Text
    , account         :: !(Maybe Text)
    , jobname         :: !Text
    , queue           :: !Text
    , owner           :: !Text
    , times           :: !TorqueJobTime
    , execHost        :: ![TorqueExecHost]
    , resourceRequest :: !TorqueResourceRequest
    , torqueEntryType :: TorqueEntryType
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueRequestor = TorqueRequestor
    { user   :: !Text
    , whence :: !Text
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobDelete = TorqueJobDelete
    { torqueDatestamp :: !Text
    , name            :: !TorqueJobName
    , requestor       :: !TorqueRequestor
    , torqueEntryType :: TorqueEntryType
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobAbort = TorqueJobAbort
    { torqueDatestamp :: !Text
    , name            :: !TorqueJobName
    , torqueEntryType :: TorqueEntryType
    } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data TorqueJobRerun = TorqueJobRerun
    { torqueDatestamp :: !Text
    , name            :: !TorqueJobName
    , torqueEntryType :: TorqueEntryType
    } deriving (Show, Eq, Generic)

instance NFData TorqueParseResult
instance NFData TorqueEntryType
instance NFData TorqueJobShortNode
instance NFData TorqueJobFQNode
instance NFData TorqueJobNode
instance NFData TorqueExecHost
instance NFData TorqueWalltime
instance NFData TorqueResourceRequest
instance NFData TorqueResourceUsage
instance NFData TorqueJobTime
instance NFData TorqueJobExit
instance NFData TorqueJobName
instance NFData TorqueJobQueue
instance NFData TorqueJobStart
instance NFData TorqueRequestor
instance NFData TorqueJobDelete
instance NFData TorqueJobAbort
instance NFData TorqueJobRerun
