{- hnormalise - a log normalisation library
 -
 - Copyright Ghent University (c) 2017
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - * Redistributions of source code must retain the above copyright
 - notice, this list of conditions and the following disclaimer.
 -
 - * Redistributions in binary form must reproduce the above
 - copyright notice, this list of conditions and the following
 - disclaimer in the documentation and/or other materials provided
 - with the distribution.
 -
 - * Neither the name of Author name here nor the names of other
 - contributors may be used to endorse or promote products derived
 - from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 - "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 - LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 - A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 - OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Lmod.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import           Data.Text                  (Text)
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

parseLmodLoad :: Parser (Text, LmodParseResult)
parseLmodLoad = do
    string "lmod::"
    info <- skipSpace *> parseLmodInfo
    userload <- char ',' *> skipSpace *> kvYesNoParser "userload"
    m <- char ',' *> skipSpace *> parseLmodModule
    filename <- char ',' *> skipSpace *> kvTextParser "fn"
    return ("lmod", LmodLoadParse LmodLoad
        { info = info
        , userload = userload
        , modul = m
        , filename = filename
        })

parseLmodCommand :: Parser (Text, LmodParseResult)
parseLmodCommand = do
    string "lmod::"
    info <- skipSpace *> parseLmodInfo
    command <- char ',' *> skipSpace *> kvTextDelimParser "cmd" ","
    arguments <- char ',' *> skipSpace *> kvTextParser "args"
    return ("lmod", LmodCommandParse LmodCommand
        { info = info
        , command = command
        , arguments = arguments
        })
