{- hnormalise - a log normalisation library
 -
 - Copyright Ghent University (c) 2017
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
            s ~> parseLmodLoad `shouldParse` ("lmod", LmodLoadParse LmodLoad
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

        it "parse command" $ do
            let s = "lmod::  username=myuser, cluster=mycluster, jobid=132.mymaster.mycluster.mydomain, cmd=load, args=cluster/othercluster" :: Text
            s ~> parseLmodCommand `shouldParse` ("lmod", LmodCommandParse LmodCommand
                { info = LmodInfo
                    { username = "myuser"
                    , cluster = "mycluster"
                    , jobid = "132.mymaster.mycluster.mydomain"
                    }
                , command = "load"
                , arguments = "cluster/othercluster"
                })
