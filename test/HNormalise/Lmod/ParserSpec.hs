{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Lmod.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text.Read            as TR
import           Test.Hspec
import           Test.Hspec.Attoparsec

--------------------------------------------------------------------------------
import           HNormalise.Lmod.Parser
import           HNormalise.Lmod.Internal

--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "parseLmodInfo" $ do
        it "parse regular info" $ do
            let s = "username=someuser, cluster=myspace, jobid=myjobid" :: Text
            s ~> parseLmodInfo `shouldParse` LmodInfo { username = "someuser" , cluster = "myspace" , jobid = "myjobid" }

        it "parse module" $ do
            let s = "module=HNormalise/0.2.0.0-ghc-8.0.2," :: Text
            s ~> parseLmodModule `shouldParse` LmodModule { name = "HNormalise", version = "0.2.0.0-ghc-8.0.2" }

        it "parse module load" $ do
            let s = "lmod::  username=myuser, cluster=mycluster, jobid=3230905.master.mycluster.mydomain, userload=yes, module=GSL/2.3-intel-2016b, fn=/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b" :: Text
            s ~> parseLmodLoad `shouldParse` ("lmod", LmodLoad
                { info = LmodInfo
                    { username = "myuser"
                    , cluster = "mycluster"
                    , jobid = "3230905.master.mycluster.mydomain"
                    }
                , userload = True
                , modul = LmodModule
                    { name = "GSL"
                    , version = "2.3-intel-2016b"
                    }
                , filename = "/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b"
                })
