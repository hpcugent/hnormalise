{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Lmod.Internal where

--------------------------------------------------------------------------------
import           Data.Text
import           GHC.Generics (Generic)
--------------------------------------------------------------------------------

data LmodModule = LmodModule
    { name    :: !Text
    , version :: !Text
    } deriving (Show, Eq, Generic)

data LmodInfo = LmodInfo
    { username :: !Text
    , cluster  :: !Text
    , jobid    :: !Text
    } deriving (Show, Eq, Generic)

data LmodLoad = LmodLoad
    { info     :: !LmodInfo
    , userload :: !Bool
    , modul    :: !LmodModule
    , filename :: !Text
    } deriving (Show, Eq, Generic)
