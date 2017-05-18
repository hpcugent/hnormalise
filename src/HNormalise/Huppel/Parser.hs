{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Huppel.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
--------------------------------------------------------------------------------

import           HNormalise.Common.Parser
import           HNormalise.Huppel.Internal
--------------------------------------------------------------------------------


parseHuppel :: Parser Huppel
parseHuppel = do
    i <- string "huppel" *> skipSpace *> decimal
    return $ Huppel { id = i }
