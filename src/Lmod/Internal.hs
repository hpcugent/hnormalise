{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lmod.Internal where

--------------------------------------------------------------------------------
import Data.Text
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------

-- username=vsc41480,
--cluster=delcatty,
--jobid=3230905.master15.delcatty.gent.vsc,
--userload=yes,
--module=GSL\/2.3-intel-2016b,
--fn=\/apps\/gent\/CO7\/sandybridge\/modules\/all\/GSL\/2.3-intel-2016b

data LmodModule = LmodModule
    { name :: Text
    , version :: Text
    } deriving (Show, Eq, Generic)

data LmodInfo = LmodInfo
    { username :: Text
    , cluster :: Text
    , jobid :: Text
    } deriving (Show, Eq, Generic)

data LmodLoad = LmodLoad
    { info :: LmodInfo
    , userload :: Bool
    , modul :: LmodModule
    , filename :: Text
    } deriving (Show, Eq, Generic)
