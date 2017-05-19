{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Lmod.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
--------------------------------------------------------------------------------

import           HNormalise.Common.Parser
import           HNormalise.Lmod.Internal
--------------------------------------------------------------------------------

parseLmodInfo :: Parser LmodInfo
parseLmodInfo = do
    username <- kvTextDelimParser "username" ","
    cluster <- char ',' *> skipSpace *> kvTextDelimParser "cluster" ","
    jobid <- char ',' *> skipSpace *> kvTextDelimParser "jobid" ","
    return LmodInfo
        { username = username
        , cluster = cluster
        , jobid = jobid
        }

parseLmodModule :: Parser LmodModule
parseLmodModule = do
    name <- kvTextDelimParser "module" "/"
    version <- char '/' *> Data.Attoparsec.Text.takeWhile (/= ',')
    return LmodModule
        { name = name
        , version = version
        }

parseLmodLoad :: Parser LmodLoad
parseLmodLoad = do
    string "lmod::"
    info <- skipSpace *> parseLmodInfo
    userload <- char ',' *> skipSpace *> kvYesNoParser "userload"
    m <- char ',' *> skipSpace *> parseLmodModule
    filename <- char ',' *> skipSpace *> kvTextParser "fn"
    return LmodLoad
        { info = info
        , userload = userload
        , modul = m
        , filename = filename
        }
