{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Shorewall.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
--------------------------------------------------------------------------------

import           HNormalise.Common.Parser
import           HNormalise.Shorewall.Internal
--------------------------------------------------------------------------------

parseShorewallTCP = undefined
parseShorewallUDP = undefined
parseShorewallICMP = undefined

parseShorewall :: Parser Shorewall
parseShorewall =
        parseShorewallTCP
    <|> parseShorewallUDP
    <|> parseShorewallICMP
