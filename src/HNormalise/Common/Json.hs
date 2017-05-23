{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module HNormalise.Common.Json where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text
import           Data.Aeson.Encoding.Internal
import           Data.Monoid
import qualified Net.Types                   as NT
--------------------------------------------------------------------------------
import           HNormalise.Common.Internal

instance ToJSON NT.IPv6 where
    toJSON = String . pack . show

--------------------------------------------------------------------------------
instance ToJSON Host where
    toJSON (Hostname h) = toJSON h
    toJSON (IPv4 ip) = toJSON ip
    toJSON (IPv6 ip) = toJSON ip
