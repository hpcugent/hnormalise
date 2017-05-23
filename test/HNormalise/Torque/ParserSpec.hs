{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Torque.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text.Read            as TR
import           Test.Hspec
import           Test.Hspec.Attoparsec

--------------------------------------------------------------------------------
import           HNormalise.Torque.Parser
import           HNormalise.Torque.Internal

--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "parseTorqueWalltime" $ do
        it "parse walltime given as SS" $ do
            let s = ("1234567" :: Text)
                Right (s', _) = TR.decimal s
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 0, hours = 0, minutes = 0, seconds = s' }

        it "parse walltime given as MM:SS" $ do
            let s = ("12:13") :: Text
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 0, hours = 0, minutes = 12, seconds = 13}

        it "parse walltime given as HH:MM:SS" $ do
            let s = ("11:12:13") :: Text
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 0, hours = 11, minutes = 12, seconds = 13 }

        it "parse walltime given as MM:SS" $ do
            let s = ("10:11:12:13") :: Text
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 10, hours = 11, minutes = 12, seconds = 13 }

    describe "parseTorqueMemory" $ do
        it "parse memory value in bytes (lowercase)" $ do
            let s = "123b" :: Text
            s ~> parseTorqueMemory `shouldParse` 123

        it "parse memory value in bytes (uppercase)" $ do
            let s = "123B" :: Text
            s ~> parseTorqueMemory `shouldParse` 123

        it "parse memory value in bytes (lowercase)" $ do
            let s = "123kb" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024)

        it "parse memory value in bytes (mixed case)" $ do
            let s = "123Kb" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024)

        it "parse memory value in bytes (mixed case)" $ do
            let s = "123kB" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024)

        it "parse memory value in bytes (uppercase)" $ do
            let s = "123KB" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024)

        it "parse memory value in bytes (lowercase)" $ do
            let s = "123mb" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024)

        it "parse memory value in bytes (mixed case)" $ do
            let s = "123Mb" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024)

        it "parse memory value in bytes (mixed case)" $ do
            let s = "123mB" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024)

        it "parse memory value in bytes (uppercase)" $ do
            let s = "123MB" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024)

        it "parse memory value in bytes (lowercase)" $ do
            let s = "123gb" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024 * 1024)

        it "parse memory value in bytes (mixed case)" $ do
            let s = "123Gb" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024 * 1024)

        it "parse memory value in bytes (mixed case)" $ do
            let s = "123gB" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024 * 1024)

        it "parse memory value in bytes (uppercase)" $ do
            let s = "123GB" :: Text
            s ~> parseTorqueMemory `shouldParse` (123 * 1024 * 1024 * 1024)
