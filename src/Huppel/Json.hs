{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Huppel.Json where

--------------------------------------------------------------------------------
import Data.Aeson

--------------------------------------------------------------------------------

import Huppel.Internal
--------------------------------------------------------------------------------


instance ToJSON Huppel where
    toJSON = genericToJSON defaultOptions
