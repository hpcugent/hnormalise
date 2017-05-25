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

        it "imfile torque input" $ do
            let s = "<133>1 2017-05-24T18:01:53.367275+02:00 test2802 torque - torque: 01/25/2017 15:04:10;E;0.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1485350399 qtime=1485350399 etime=1485350399 start=1485350407 owner=vsc40075@gligar03.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0 Resource_List.walltime=01:00:00 Resource_List.vmem=4224531456b Resource_List.nodect=1 Resource_List.nodes=1 Resource_List.neednodes=1 Resource_List.nice=0 session=22598 total_execution_slots=1 unique_node_count=1 end=1485353050 Exit_status=265 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=31032kb resources_used.vmem=1541612kb resources_used.walltime=00:44:04" :: Text
            s ~> parseRsyslogLogstashString `shouldParse` "{\"message\":\"torque: 01/25/2017 15:04:10;E;0.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1485350399 qtime=1485350399 etime=1485350399 start=1485350407 owner=vsc40075@gligar03.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0 Resource_List.walltime=01:00:00 Resource_List.vmem=4224531456b Resource_List.nodect=1 Resource_List.nodes=1 Resource_List.neednodes=1 Resource_List.nice=0 session=22598 total_execution_slots=1 unique_node_count=1 end=1485353050 Exit_status=265 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=31032kb resources_used.vmem=1541612kb resources_used.walltime=00:44:04\",\"syslog_abspri\":\"\",\"program\":\"torque\",\"torque\":{\"name\":{\"number\":0,\"array_id\":null,\"master\":\"master23\",\"cluster\":\"banette\"},\"user\":\"vsc40075\",\"group\":\"vsc40075\",\"jobname\":\"STDIN\",\"queue\":\"short\",\"startCount\":null,\"owner\":\"vsc40075@gligar03.gligar.gent.vsc\",\"session\":22598,\"times\":{\"ctime\":1485350399,\"qtime\":1485350399,\"etime\":1485350399,\"startTime\":1485350407,\"endTime\":1485353050},\"resourceRequest\":{\"mem\":null,\"advres\":null,\"naccesspolicy\":null,\"ncpus\":null,\"neednodes\":{\"Left\":{\"number\":1,\"ppn\":null}},\"nice\":0,\"nodeCount\":1,\"nodes\":{\"Left\":{\"number\":1,\"ppn\":null}},\"select\":null,\"qos\":null,\"pmem\":null,\"vmem\":4224531456,\"pvmem\":null,\"walltime\":{\"days\":0,\"hours\":1,\"minutes\":0,\"seconds\":0}},\"resourceUsage\":{\"cputime\":0,\"energy\":0,\"mem\":31776768,\"vmem\":1578610688,\"walltime\":{\"days\":0,\"hours\":0,\"minutes\":44,\"seconds\":4}},\"totalExecutionSlots\":1,\"uniqueNodeCount\":1,\"exitStatus\":265}}"
