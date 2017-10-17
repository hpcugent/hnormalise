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

module HNormalise.Snoopy.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text.Read            as TR
import           Test.Hspec
import           Test.Hspec.Attoparsec
import qualified Net.IPv4                  as NT

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal
import           HNormalise.Snoopy.Internal
import           HNormalise.Snoopy.Parser
--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "Snoopy" $ do
        it "regular entry" $ do
            let s = "snoopy[27316]::  [uid:110 sid:9379 tty:(none) cwd:/ filename:/usr/lib64/nagios/plugins/hpc/check_ifutil.pl]: /usr/lib64/nagios/plugins/hpc/check_ifutil.pl -i em1.295 -w 90 -c 95 -p -b 10000m" :: Text
            s ~> parseSnoopy `shouldParse` ("snoopy", Snoopy
                { pid = 27316
                , uid = 110
                , username = Nothing
                , sid = 9379
                , tty = "(none)"
                , cwd = "/"
                , executable = "/usr/lib64/nagios/plugins/hpc/check_ifutil.pl"
                , command = "/usr/lib64/nagios/plugins/hpc/check_ifutil.pl -i em1.295 -w 90 -c 95 -p -b 10000m"
                })

        it "regular entry" $ do
            let s = "snoopy[46513]:: [uid:2540337 sid:19403 tty:ERROR(ttyname_r->EUNKNOWN) cwd:/vscmnt/gent_vulpix/_/user/home/gent/vsc403/vsc40337/UCS_LABELLED_NEW/20000_to_30000 filename:/usr/bin/qsub]: qsub -l walltime=72:00:00 job7_21293_30000_doit" :: Text
            s ~> parseSnoopy `shouldParse` ("snoopy", Snoopy
                { pid = 46513
                , uid = 2540337
                , username = Nothing
                , sid = 19403
                , tty = "ERROR(ttyname_r->EUNKNOWN)"
                , cwd = "/vscmnt/gent_vulpix/_/user/home/gent/vsc403/vsc40337/UCS_LABELLED_NEW/20000_to_30000"
                , executable = "/usr/bin/qsub"
                , command = "qsub -l walltime=72:00:00 job7_21293_30000_doit"
                })

        it "failing" $ do
            let s = "snoopy[28949]::  [uid:992 username:nrpe sid:11542 tty:(none) cwd:/ filename:/usr/bin/which]: which python" :: Text
            s ~> parseSnoopy `shouldParse` ("snoopy", Snoopy
                { pid = 28949
                , uid = 992
                , username = Just "nrpe"
                , sid = 11542
                , tty = "(none)"
                , cwd = "/"
                , executable = "/usr/bin/which"
                , command = "which python"
                })
