{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module HNormalise.Lmod.Json where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Monoid
--------------------------------------------------------------------------------

import           HNormalise.Lmod.Internal
--------------------------------------------------------------------------------


instance ToJSON LmodInfo where
    --toEncoding = genericToEncoding defaultOptions
    toEncoding (LmodInfo username cluster jobid) =
        pairs
            (  "username" .= username
            <> "cluster" .= cluster
            <> "jobid" .= jobid
            )

instance ToJSON LmodLoad where
    --toEncoding = genericToEncoding defaultOptions
    toEncoding (LmodLoad info userload modul filename) =
        pairs
            (  "info" .= info
            <> "userload" .= userload
            <> "module" .= modul
            <> "filename" .= filename
            )

instance ToJSON LmodModule where
    --toEncoding = genericToEncoding defaultOptions
    toEncoding (LmodModule name version) =
        pairs
            (  "name" .= name
            <> "version" .= version
            )
