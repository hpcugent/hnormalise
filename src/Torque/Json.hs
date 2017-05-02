{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Torque.Json where

import Data.Aeson

import Torque.Internal

instance ToJSON TorqueJobShortNode where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueJobFQNode where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueExecHost where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueWalltime where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueResourceRequest where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueResourceUsage where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueJobTime where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueJobExit where
    toJSON = genericToJSON defaultOptions

instance ToJSON TorqueJobName where
    toJSON = genericToJSON defaultOptions
