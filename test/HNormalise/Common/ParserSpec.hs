{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Common.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Attoparsec.Text
import           Data.Text                 (Text)
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           HNormalise.Common.Parser

--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "kvTextParser" $ do
        it "parse a key value pair with given key" $ do
            ("foo=bar" :: Text) ~> (kvTextParser "foo") `shouldParse` ("bar" :: Text)

    describe "kvTextDelimParser" $ do
        it "parse a key value pair delimited by a given char" $ do
            ("foo=barD" :: Text) ~> (kvTextDelimParser "foo" "D") `shouldParse` ("bar" :: Text)

    describe "kvNumParser" $ do
        it "parse a key value pair with an integral value" $ do
            ("foo=42" :: Text) ~> (kvNumParser "foo") `shouldParse` 42
