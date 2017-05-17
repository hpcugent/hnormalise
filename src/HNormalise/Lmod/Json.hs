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
    toEncoding = genericToEncoding defaultOptions

instance ToJSON LmodLoad where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON LmodModule where
    toEncoding = genericToEncoding defaultOptions
