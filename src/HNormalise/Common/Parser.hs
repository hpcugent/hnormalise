{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Common.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as T
--------------------------------------------------------------------------------

keyParser k = string k *> char '='
{-# INLINE keyParser #-}

kvTextParser :: Text -> Parser Text
kvTextParser key = kvTextDelimParser key " \n\t"
{-# INLINE kvTextParser #-}

kvTextDelimParser :: Text -> String -> Parser Text
kvTextDelimParser key ds = keyParser key *> takeTill (`elem` ds)
{-# INLINE kvTextDelimParser #-}

kvNumParser :: Integral a => Text -> Parser a
kvNumParser key = keyParser key *> decimal
{-# INLINE kvNumParser #-}

kvYesNoParser :: Text -> Parser Bool
kvYesNoParser key = do
    keyParser key
    yn <- asciiCI "yes" <|> asciiCI "no"
    return $ case T.toLower yn of
        "yes" -> True
        "no"  -> False
{-# INLINE kvYesNoParser #-}

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)
{-# INLINE maybeOption #-}
