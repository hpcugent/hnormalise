{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Common.Parser where

--------------------------------------------------------------------------------
import Control.Applicative ( (<|>) )
import Data.Attoparsec.Text
import Data.Text ( Text )
import qualified Data.Text as T
--------------------------------------------------------------------------------

whitespace = many' (char ' ')
keyParser k = string k *> char '='

kvTextParser :: Text -> Parser Text
kvTextParser key = kvTextDelimParser key " \n\t"

kvTextDelimParser :: Text -> String -> Parser Text
kvTextDelimParser key ds = keyParser key *> takeTill (`elem` ds)

kvNumParser :: Integral a => Text -> Parser a
kvNumParser key = keyParser key *> decimal

kvYesNoParser :: Text -> Parser Bool
kvYesNoParser key = do
    keyParser key
    yn <- asciiCI "yes" <|> asciiCI "no"
    return $ case T.toLower yn of
        "yes" -> True
        "no" -> False

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)
