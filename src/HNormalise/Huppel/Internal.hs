{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Huppel.Internal where


--------------------------------------------------------------------------------
import           Data.Text
import           GHC.Generics (Generic)
--------------------------------------------------------------------------------



data Huppel = Huppel
    { id :: Int } deriving (Eq, Show, Generic)
