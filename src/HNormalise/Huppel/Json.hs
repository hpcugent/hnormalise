{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Huppel.Json where

--------------------------------------------------------------------------------
import           Data.Aeson

--------------------------------------------------------------------------------

import           HNormalise.Huppel.Internal
--------------------------------------------------------------------------------


instance ToJSON Huppel where
    toEncoding = genericToEncoding defaultOptions
