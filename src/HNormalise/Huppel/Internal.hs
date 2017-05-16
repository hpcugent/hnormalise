{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HNormalise.Huppel.Internal where


--------------------------------------------------------------------------------
import Data.Text
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------



data Huppel = Huppel
    { id :: Int } deriving (Eq, Show, Generic)
