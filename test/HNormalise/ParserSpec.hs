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

module HNormalise.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                  (Text)
import qualified Data.Text.Read             as TR
import qualified Net.IPv4                   as NT
import           Test.Hspec
import           Test.Hspec.Attoparsec

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal
import           HNormalise.Internal
import           HNormalise.Lmod.Internal
import           HNormalise.Parser
import           HNormalise.Json
--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec =
    describe "parse full rsyslog message" $ do
        it "lmod load" $ do
            let s = "<13>1 2016-06-07T17:50:22.658452+02:00 node2159 lmod: - lmod:: username=myuser, cluster=dmycluster, jobid=434.master.mycluster.mydomain, userload=yes, module=intel/2016a, fn=/apps/gent/SL6/sandybridge/modules/all/intel/2016" :: Text
            s ~> (encodeNormalisedRsyslog `fmap` parseRsyslogLogstashString Nothing) `shouldParse`
                "{\"message\":\"lmod:: username=myuser, cluster=dmycluster, jobid=434.master.mycluster.mydomain, userload=yes, module=intel/2016a, fn=/apps/gent/SL6/sandybridge/modules/all/intel/2016\",\"syslog_abspri\":13,\"syslog_version\":1,\"program\":\"lmod\",\"@source_host\":\"node2159\",\"lmod\":{\"info\":{\"username\":\"myuser\",\"cluster\":\"dmycluster\",\"jobid\":{\"number\":434,\"arrayId\":null,\"master\":\"master\",\"cluster\":\"mycluster\"}},\"userload\":true,\"module\":{\"name\":\"intel\",\"version\":\"2016a\"},\"filename\":\"/apps/gent/SL6/sandybridge/modules/all/intel/2016\"}}"

        it "lmod command" $ do
            let s = "<13>1 2017-10-19T21:38:22.533439+02:00 node2801 lmod: - lmod::  username=myuser, cluster=mycluster, jobid=132.mymaster.mycluster.mydomain, cmd=load, args=cluster/othercluster" :: Text
            s ~> (encodeNormalisedRsyslog `fmap` parseRsyslogLogstashString Nothing) `shouldParse` 
                "{\"message\":\"lmod::  username=myuser, cluster=mycluster, jobid=132.mymaster.mycluster.mydomain, cmd=load, args=cluster/othercluster\",\"syslog_abspri\":13,\"syslog_version\":1,\"program\":\"lmod\",\"@source_host\":\"node2801\",\"lmod\":{\"info\":{\"username\":\"myuser\",\"cluster\":\"mycluster\",\"jobid\":{\"number\":132,\"arrayId\":null,\"master\":\"mymaster\",\"cluster\":\"mycluster\"}},\"command\":\"load\",\"arguments\":\"cluster/othercluster\"}}"

        it "imfile torque input" $ do
            let s = "<133>1 2017-05-24T18:01:53.367275+02:00 test2802 torque - torque: 01/25/2017 15:04:10;E;0.mymaster.somepokemon.mydomain;user=huppelde group=huppelde jobname=STDIN queue=short ctime=1485350399 qtime=1485350399 etime=1485350399 start=1485350407 owner=huppelde@mymachine.mydomain.com exec_host=node2801.somepokemon.mydomain/0 Resource_List.walltime=01:00:00 Resource_List.vmem=4224531456b Resource_List.nodect=1 Resource_List.nodes=1 Resource_List.neednodes=1 Resource_List.nice=0 session=22598 total_execution_slots=1 unique_node_count=1 end=1485353050 Exit_status=265 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=31032kb resources_used.vmem=1541612kb resources_used.walltime=00:44:04" :: Text
            s ~> (encodeNormalisedRsyslog `fmap` parseRsyslogLogstashString Nothing) `shouldParse` "{\"message\":\"torque: 01/25/2017 15:04:10;E;0.mymaster.somepokemon.mydomain;user=huppelde group=huppelde jobname=STDIN queue=short ctime=1485350399 qtime=1485350399 etime=1485350399 start=1485350407 owner=huppelde@mymachine.mydomain.com exec_host=node2801.somepokemon.mydomain/0 Resource_List.walltime=01:00:00 Resource_List.vmem=4224531456b Resource_List.nodect=1 Resource_List.nodes=1 Resource_List.neednodes=1 Resource_List.nice=0 session=22598 total_execution_slots=1 unique_node_count=1 end=1485353050 Exit_status=265 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=31032kb resources_used.vmem=1541612kb resources_used.walltime=00:44:04\",\"syslog_abspri\":133,\"syslog_version\":1,\"program\":\"torque\",\"@source_host\":\"test2802\",\"torque\":{\"torqueDatestamp\":\"01/25/2017 15:04:10\",\"name\":{\"number\":0,\"arrayId\":null,\"master\":\"mymaster\",\"cluster\":\"somepokemon\"},\"user\":\"huppelde\",\"group\":\"huppelde\",\"account\":null,\"jobname\":\"STDIN\",\"queue\":\"short\",\"startCount\":null,\"owner\":\"huppelde@mymachine.mydomain.com\",\"session\":22598,\"times\":{\"ctime\":1485350399,\"qtime\":1485350399,\"etime\":1485350399,\"startTime\":1485350407,\"endTime\":1485353050},\"execHost\":[{\"name\":\"node2801.somepokemon.mydomain\",\"cores\":[0]}],\"resourceRequest\":{\"mem\":null,\"advres\":null,\"naccesspolicy\":null,\"ncpus\":null,\"cputime\":null,\"prologue\":null,\"epilogue\":null,\"neednodes\":{\"number\":1,\"ppn\":null},\"nice\":0,\"nodeCount\":1,\"nodes\":{\"number\":1,\"ppn\":null},\"select\":null,\"qos\":null,\"other\":null,\"feature\":null,\"host\":null,\"procs\":null,\"nodeset\":null,\"tpn\":null,\"pmem\":null,\"vmem\":4224531456,\"pvmem\":null,\"mppmem\":null,\"walltime\":3600},\"resourceUsage\":{\"cputime\":0,\"energy\":0,\"mem\":31776768,\"vmem\":1578610688,\"walltime\":2644},\"totalExecutionSlots\":1,\"uniqueNodeCount\":1,\"exitStatus\":265,\"torqueEntryType\":\"TorqueExitEntry\"}}"

        it "imfile torque abort job input" $ do
            let s = "<133>1 2017-09-14T21:13:09.181048+02:00 master15 torque - torque: 10/17/2013 02:46:14;A;86264.master15.delcatty.gent.vsc;\n" :: Text
            s ~> (encodeNormalisedRsyslog `fmap` parseRsyslogLogstashString Nothing) `shouldParse` "{\"message\":\"torque: 10/17/2013 02:46:14;A;86264.master15.delcatty.gent.vsc;\",\"syslog_abspri\":133,\"syslog_version\":1,\"program\":\"torque\",\"@source_host\":\"master15\",\"torque\":{\"torqueDatestamp\":\"10/17/2013 02:46:14\",\"name\":{\"number\":86264,\"arrayId\":null,\"master\":\"master15\",\"cluster\":\"delcatty\"},\"torqueEntryType\":\"TorqueAbortEntry\"}}"

        it "snoopy with process ID" $ do
            let s = "<86>1 2017-05-29T16:40:48.275334+02:00 mymaster snoopy[28949]: - snoopy[28949]::  [uid:992 username:nrpe sid:11542 tty:(none) cwd:/ filename:/usr/bin/which]: which python" :: Text
            s ~> (encodeNormalisedRsyslog `fmap` parseRsyslogLogstashString Nothing) `shouldParse` "{\"message\":\"snoopy[28949]::  [uid:992 username:nrpe sid:11542 tty:(none) cwd:/ filename:/usr/bin/which]: which python\",\"syslog_abspri\":86,\"syslog_version\":1,\"program\":\"snoopy\",\"@source_host\":\"mymaster\",\"snoopy\":{\"pid\":28949,\"uid\":992,\"username\":\"nrpe\",\"sid\":11542,\"tty\":\"(none)\",\"cwd\":\"/\",\"executable\":\"/usr/bin/which\",\"command\":\"which python\"}}"

        it "snoopy with process ID; only the @source_host" $ do
            let s = "<86>1 2017-05-29T16:40:48.275334+02:00 mymaster snoopy[28949]: - snoopy[28949]::  [uid:992 username:nrpe sid:11542 tty:(none) cwd:/ filename:/usr/bin/which]: which python" :: Text
            s ~> (encodeNormalisedRsyslog `fmap` (parseRsyslogLogstashString $ Just [("@source_host", "hostname")])) `shouldParse` "{\"snoopy\":{\"pid\":28949,\"uid\":992,\"username\":\"nrpe\",\"sid\":11542,\"tty\":\"(none)\",\"cwd\":\"/\",\"executable\":\"/usr/bin/which\",\"command\":\"which python\"},\"@source_host\":\"mymaster\"}"
