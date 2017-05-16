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
    { number :: Int
    , ppn    :: Int
    } deriving (Show, Eq, Generic)

data TorqueJobFQNode = TorqueJobFQNode
    { name :: Text
    , ppn  :: Int
    } deriving (Show, Eq, Generic)

data TorqueExecHost = TorqueExecHost
    { name      :: Text
    , lowerCore :: Int
    , upperCore :: Int
    } deriving (Show, Eq, Generic)

data TorqueWalltime = TorqueWalltime
    { days    :: Int
    , hours   :: Int
    , minutes :: Int
    , seconds :: Int
    } deriving (Show, Eq, Generic)

data TorqueResourceRequest = TorqueResourceRequest
    { nodes     :: Either TorqueJobShortNode [TorqueJobFQNode]
    , walltime  :: TorqueWalltime
    , vmem      :: Integer
    , nodeCount :: Int
    , neednodes :: Either TorqueJobShortNode [TorqueJobFQNode]
    , nice      :: Maybe Int
    } deriving (Show, Eq, Generic)

data TorqueResourceUsage = TorqueResourceUsage
    { cputime  :: Integer
    , energy   :: Integer
    , mem      :: Integer
    , vmem     :: Integer
    , walltime :: TorqueWalltime
    } deriving (Show, Eq, Generic)

data TorqueJobTime = TorqueJobTime
    { ctime     :: Integer
    , qtime     :: Integer
    , etime     :: Integer
    , startTime :: Integer
    , endTime   :: Integer
    } deriving (Show, Eq, Generic)

data TorqueJobExit = TorqueJobExit
    { name                :: TorqueJobName
    , user                :: Text
    , group               :: Text
    , jobname             :: Text
    , queue               :: Text
    , startCount          :: Maybe Int
    , owner               :: Text
    , session             :: Integer
    , times               :: TorqueJobTime
    , resourceRequest     :: TorqueResourceRequest
    , resourceUsage       :: TorqueResourceUsage
    , totalExecutionSlots :: Int
    , uniqueNodeCount     :: Int
    , exitStatus          :: Int
    } deriving (Show, Eq, Generic)

data TorqueJobName = TorqueJobName
    { number  :: Integer
    , master  :: Text
    , cluster :: Text
    } deriving (Show, Eq, Generic)
