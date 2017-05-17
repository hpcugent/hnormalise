{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module HNormalise.Torque.Json where

--------------------------------------------------------------------------------
import           Data.Aeson

--------------------------------------------------------------------------------
import           HNormalise.Torque.Internal

--------------------------------------------------------------------------------

instance ToJSON TorqueJobShortNode where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobFQNode where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueExecHost where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueWalltime where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueResourceRequest where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueResourceUsage where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobTime where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobExit where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TorqueJobName where
    toEncoding = genericToEncoding defaultOptions
