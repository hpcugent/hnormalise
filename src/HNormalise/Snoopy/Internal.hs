{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Snoopy.Internal where


--------------------------------------------------------------------------------
import           Data.Aeson                 (FromJSON, ToJSON, toEncoding,
                                             toJSON)
import           Data.Text
import           GHC.Generics               (Generic)

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal
-- snoopy[27316]::  [uid:110 sid:9379 tty:(none) cwd:/ filename:/usr/lib64/nagios/plugins/hpc/check_ifutil.pl]: /usr/lib64/nagios/plugins/hpc/check_ifutil.pl -i em1.295 -w 90 -c 95 -p -b 10000m
data Snoopy = Snoopy
    { pid        :: !Int
    , uid        :: !Int
    , username   :: !(Maybe Text)
    , sid        :: !Int
    , tty        :: !Text
    , cwd        :: !Text
    , executable :: !Text
    , command    :: !Text
    } deriving (Show, Eq, Generic)
