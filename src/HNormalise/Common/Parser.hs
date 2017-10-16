{- hnormalise - a log normalisation library
 -
 - Copyright Andy Georges (c) 2017
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
--    , IPv6.parser >>= \ip -> return $ IPv6 ip
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
kvSignedParser :: Integral a => Text -> Parser a
kvSignedParser key = keyParser key *> signed decimal
{-# INLINE kvSignedParser #-}

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
