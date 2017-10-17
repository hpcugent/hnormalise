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

module HNormalise.Snoopy.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import           Data.Char                  (isSpace)
import           Data.Text                  (Text)

--------------------------------------------------------------------------------
import           HNormalise.Common.Parser
import           HNormalise.Snoopy.Internal

--------------------------------------------------------------------------------
-- snoopy[27316]::  [uid:110 sid:9379 tty:(none) cwd:/ filename:/usr/lib64/nagios/plugins/hpc/check_ifutil.pl]: /usr/lib64/nagios/plugins/hpc/check_ifutil.pl -i em1.295 -w 90 -c 95 -p -b 10000m
parseSnoopy :: Parser (Text, Snoopy)
parseSnoopy = do
    string "snoopy["
    pid <- decimal
    takeTill ( == '[') *> char '['
    uid <- string "uid:" *> decimal
    username <- maybeOption $ skipSpace *> string "username:" *> takeTill isSpace
    sid <- skipSpace *> string "sid:" *> decimal
    tty <- skipSpace *> string "tty:" *> takeTill isSpace
    cwd <- skipSpace *> string "cwd:" *> takeTill isSpace
    executable <- skipSpace *> "filename:" *> takeTill ( == ']')
    string "]:"
    command <- skipSpace *> takeText
    return $ ("snoopy", Snoopy
        { pid = pid
        , uid = uid
        , username = username
        , sid = sid
        , tty = tty
        , cwd = cwd
        , executable = executable
        , command = command
        })
