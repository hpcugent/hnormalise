{-# LANGUAGE DuplicateRecordFields #-}
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

    describe "parseTorqueJobName" $ do
        it "parse regular torque job name" $ do
            let s = "123456789.master.mycluster.mydomain;" :: Text
            s ~> parseTorqueJobName `shouldParse` TorqueJobName { number = 123456789, array_id = Nothing, master = "master", cluster = "mycluster" }

        it "parse array torque job name" $ do
            let s = "123456[789].master.mycluster.mydomain;" :: Text
            s ~> parseTorqueJobName `shouldParse` TorqueJobName { number = 123456, array_id = Just 789, master = "master", cluster = "mycluster" }

    describe "parseTorqueResourceRequest" $ do
        it "parse mandatory fields in expected order" $ do
            let s = "Resource_List.neednodes=1:ppn=1 Resource_List.nodect=1 Resource_List.nodes=1:ppn=1 Resource_List.walltime=01:00:00" :: Text
            s ~> parseTorqueResourceRequest `shouldParse` TorqueResourceRequest
                { mem           = Nothing
                , advres        = Nothing
                , naccesspolicy = Nothing
                , ncpus         = Nothing
                , neednodes     = Left TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = Left TorqueJobShortNode { number = 1, ppn = Just 1 }
                , select        = Nothing
                , qos           = Nothing
                , pmem          = Nothing
                , vmem          = Nothing
                , pvmem         = Nothing
                , walltime      = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0 }
                }

        it "parse mandatory fields in reverse order" $ do
            let s = "Resource_List.walltime=01:00:00 Resource_List.nodes=1:ppn=1 Resource_List.nodect=1 Resource_List.neednodes=1:ppn=1" :: Text
            s ~> parseTorqueResourceRequest `shouldParse` TorqueResourceRequest
                { mem           = Nothing
                , advres        = Nothing
                , naccesspolicy = Nothing
                , ncpus         = Nothing
                , neednodes     = Left TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = Left TorqueJobShortNode { number = 1, ppn = Just 1 }
                , select        = Nothing
                , qos           = Nothing
                , pmem          = Nothing
                , vmem          = Nothing
                , pvmem         = Nothing
                , walltime      = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0 }
                }








    describe "parseTorqueExit" $ do
        it "parse job exit log line" $ do
            let s = "torque: 04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44" :: Text
            s ~> parseTorqueExit `shouldParse` ("torque", TorqueJobExit
                { name = TorqueJobName { number = 45, array_id = Nothing, master = "master23", cluster = "banette" }
                , user = "vsc40075"
                , group = "vsc40075"
                , jobname = "STDIN"
                , queue = "short"
                , startCount = Nothing
                , owner = "vsc40075@gligar01.gligar.gent.vsc"
                , session = 15273
                , times = TorqueJobTime
                    { ctime = 1491390300
                    , qtime = 1491390300
                    , etime = 1491390300
                    , startTime = 1491390307
                    , endTime = 1491390413
                    }
                , resourceRequest = TorqueResourceRequest
                    { mem           = Nothing
                    , advres        = Nothing
                    , naccesspolicy = Nothing
                    , ncpus         = Nothing
                    , neednodes = Right
                        [ TorqueJobFQNode
                            { name = "node2801.banette.gent.vsc"
                            , ppn  = 2
                            }
                        , TorqueJobFQNode
                            { name = "node2803.banette.gent.vsc"
                            , ppn  = 2
                            }
                        ]
                    , nice      = Just 0
                    , nodeCount = 2
                    , nodes = Right
                        [ TorqueJobFQNode
                            { name = "node2801.banette.gent.vsc"
                            , ppn  = 2
                            }
                        , TorqueJobFQNode
                            { name = "node2803.banette.gent.vsc"
                            , ppn  = 2
                            }
                        ]
                    , select        = Nothing
                    , qos           = Nothing
                    , vmem = Just $ 1 * 1024 * 1024 * 1024
                    , pmem = Nothing
                    , pvmem = Nothing
                    , walltime  = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0}
                    }
                , resourceUsage = TorqueResourceUsage
                    { cputime = 0
                    , energy = 0
                    , mem = 55048 * 1024
                    , vmem = 92488 * 1024
                    , walltime = TorqueWalltime { days = 0, hours = 0, minutes = 1, seconds = 44 }
                    }
                , totalExecutionSlots = 4
                , uniqueNodeCount = 2
                , exitStatus = 0
                })
