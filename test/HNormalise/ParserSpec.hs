{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text.Read            as TR
import           Test.Hspec
import           Test.Hspec.Attoparsec
import qualified Net.IPv4                  as NT

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal
import           HNormalise.Lmod.Internal
import           HNormalise.Internal
import           HNormalise.Parser
--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "parse full rsyslog message" $ do
        it "lmod load" $ do
            let s = "<13>1 2016-06-07T17:50:22.658452+02:00 node2159 lmod: - lmod:: username=myuser, cluster=dmycluster, jobid=434.master.mycluster.mydomain, userload=yes, module=intel/2016a, fn=/apps/gent/SL6/sandybridge/modules/all/intel/2016" :: Text
            s ~> parseRsyslogLogstashString `shouldParse`
                "{\"message\":\"lmod:: username=myuser, cluster=dmycluster, jobid=434.master.mycluster.mydomain, userload=yes, module=intel/2016a, fn=/apps/gent/SL6/sandybridge/modules/all/intel/2016\",\"syslog_abspri\":\"\",\"program\":\"lmod\",\"lmod\":{\"info\":{\"username\":\"myuser\",\"cluster\":\"dmycluster\",\"jobid\":\"434.master.mycluster.mydomain\"},\"userload\":true,\"module\":{\"name\":\"intel\",\"version\":\"2016a\"},\"filename\":\"/apps/gent/SL6/sandybridge/modules/all/intel/2016\"}}"
