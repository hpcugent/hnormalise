{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Lmod.Json where

import Data.Aeson

import Lmod.Internal


instance ToJSON LmodInfo where
    toJSON = genericToJSON defaultOptions
    
instance ToJSON LmodLoad where
    toJSON = genericToJSON defaultOptions

instance ToJSON LmodModule where
    toJSON = genericToJSON defaultOptions
