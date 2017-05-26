{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Snoopy.Json where

--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.Monoid

--------------------------------------------------------------------------------
import           HNormalise.Common.Json
import           HNormalise.Snoopy.Internal

--------------------------------------------------------------------------------
instance ToJSON Snoopy where
    toEncoding = genericToEncoding defaultOptions
