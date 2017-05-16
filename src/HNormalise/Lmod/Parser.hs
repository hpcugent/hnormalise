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
    cluster <- char ',' *> whitespace *> kvTextDelimParser "cluster" ","
    jobid <- char ',' *> whitespace *> kvTextDelimParser "jobid" ","
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
    info <- whitespace *> parseLmodInfo
    userload <- char ',' *> whitespace *> kvYesNoParser "userload"
    m <- char ',' *> whitespace *> parseLmodModule
    filename <- char ',' *> whitespace *> kvTextParser "fn"
    return LmodLoad
        { info = info
        , userload = userload
        , modul = m
        , filename = filename
        }
