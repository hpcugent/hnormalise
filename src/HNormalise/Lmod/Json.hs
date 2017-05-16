{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module HNormalise.Lmod.Json where

--------------------------------------------------------------------------------
import           Data.Aeson
--------------------------------------------------------------------------------

import           HNormalise.Lmod.Internal
--------------------------------------------------------------------------------


instance ToJSON LmodInfo where
    toJSON = genericToJSON defaultOptions

instance ToJSON LmodLoad where
    toJSON = genericToJSON defaultOptions

instance ToJSON LmodModule where
    toJSON = genericToJSON defaultOptions
