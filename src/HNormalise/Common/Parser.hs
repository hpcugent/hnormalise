{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Common.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative   ((<|>))
import           Data.Attoparsec.Text
import           Data.Char             (isSpace)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Net.IPv4.Text         as IPv4
import qualified Net.IPv6.Text         as IPv6

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal

--------------------------------------------------------------------------------
hostnameParser :: Parser Text
hostnameParser = sepBy' (takeWhile1 (inClass "a-z0-9-")) (char '.') >>= \hns -> return $ T.intercalate "." hns
{-# INLINE hostnameParser #-}

--------------------------------------------------------------------------------
hostnameOrIPParser :: Parser Host
hostnameOrIPParser = choice
    [ IPv4.parser >>= \ip -> return $ IPv4 ip
    , IPv6.parser >>= \ip -> return $ IPv6 ip
    , hostnameParser >>= \h -> return $ Hostname h
    ]
{-# INLINE hostnameOrIPParser #-}

--------------------------------------------------------------------------------
keyParser k = string k *> char '='
{-# INLINE keyParser #-}

--------------------------------------------------------------------------------
kvParser :: Parser (Text, Text)
kvParser = do
    key <- takeTill (== '=')
    value <- char '=' *> takeTill isSpace
    return (key, value)
{-# INLINE kvParser #-}

--------------------------------------------------------------------------------
kvTextParser :: Text -> Parser Text
kvTextParser key = kvTextDelimParser key " \n\t"
{-# INLINE kvTextParser #-}

--------------------------------------------------------------------------------
kvTextDelimParser :: Text -> String -> Parser Text
kvTextDelimParser key ds = keyParser key *> takeTill (`elem` ds)
{-# INLINE kvTextDelimParser #-}

--------------------------------------------------------------------------------
kvNumParser :: Integral a => Text -> Parser a
kvNumParser key = keyParser key *> decimal
{-# INLINE kvNumParser #-}

--------------------------------------------------------------------------------
kvYesNoParser :: Text -> Parser Bool
kvYesNoParser key = do
    keyParser key
    yn <- asciiCI "yes" <|> asciiCI "no"
    return $ case T.toLower yn of
        "yes" -> True
        "no"  -> False
{-# INLINE kvYesNoParser #-}

--------------------------------------------------------------------------------
kvHostOrIPParser :: Text -> Parser Host
kvHostOrIPParser key = keyParser key *> hostnameOrIPParser

{-# INLINE kvHostOrIPParser #-}

--------------------------------------------------------------------------------
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)
{-# INLINE maybeOption #-}
