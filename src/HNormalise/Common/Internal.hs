{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Common.Internal
    ( Host (..)
    ) where

--------------------------------------------------------------------------------
import           Data.Aeson                 (FromJSON, ToJSON, toEncoding,
                                             toJSON)
import           Data.Text
import           GHC.Generics               (Generic)
import qualified Net.Types                  as Net

--------------------------------------------------------------------------------
data Host = Hostname Text        -- hostname
          | IPv4 Net.IPv4
          | IPv6 Net.IPv6
          deriving (Show, Eq, Generic)
