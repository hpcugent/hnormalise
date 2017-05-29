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
