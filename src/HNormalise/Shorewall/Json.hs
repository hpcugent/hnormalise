{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Shorewall.Json where

--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.Monoid

--------------------------------------------------------------------------------
import           HNormalise.Shorewall.Internal

--------------------------------------------------------------------------------
instance ToJSON Shorewall where
    toEncoding = genericToEncoding defaultOptions
