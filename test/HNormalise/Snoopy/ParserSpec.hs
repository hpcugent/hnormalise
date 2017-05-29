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
