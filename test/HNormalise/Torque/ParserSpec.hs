{- hnormalise - a log normalisation library
 -
 - Copyright Andy Georges (c) 2017
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
            let s = "1234567" :: Text
                Right (s', _) = TR.decimal s
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 0, hours = 0, minutes = 0, seconds = s' }

        it "parse walltime given as MM:SS" $ do
            let s = "12:13" :: Text
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 0, hours = 0, minutes = 12, seconds = 13}

        it "parse walltime given as HH:MM:SS" $ do
            let s = "11:12:13" :: Text
            s ~> parseTorqueWalltime `shouldParse` TorqueWalltime { days = 0, hours = 11, minutes = 12, seconds = 13 }

        it "parse walltime given as MM:SS" $ do
            let s = "10:11:12:13" :: Text
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
                , neednodes     = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
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
                , neednodes     = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , select        = Nothing
                , qos           = Nothing
                , pmem          = Nothing
                , vmem          = Nothing
                , pvmem         = Nothing
                , walltime      = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0 }
                }

        it "parse mandatory fields plus mem fields" $ do
            let s = "Resource_List.vmem=1mb Resource_List.walltime=01:00:00 Resource_List.pvmem=400kb Resource_List.pmem=200kB Resource_List.nodes=1:ppn=1 Resource_List.nodect=1 Resource_List.neednodes=1:ppn=1" :: Text
            s ~> parseTorqueResourceRequest `shouldParse` TorqueResourceRequest
                { mem           = Nothing
                , advres        = Nothing
                , naccesspolicy = Nothing
                , ncpus         = Nothing
                , neednodes     = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , select        = Nothing
                , qos           = Nothing
                , pmem          = Just $ 200 * 1024
                , vmem          = Just $ 1 * 1024 * 1024
                , pvmem         = Just $ 400 * 1024
                , walltime      = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0 }
                }

        it "parse mandatory fields plus reservation field" $ do
            let s = "Resource_List.walltime=01:00:00 Resource_List.advres=myreservation.1 Resource_List.nodes=1:ppn=1 Resource_List.nodect=1 Resource_List.neednodes=1:ppn=1" :: Text
            s ~> parseTorqueResourceRequest `shouldParse` TorqueResourceRequest
                { mem           = Nothing
                , advres        = Just "myreservation.1"
                , naccesspolicy = Nothing
                , ncpus         = Nothing
                , neednodes     = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , select        = Nothing
                , qos           = Nothing
                , pmem          = Nothing
                , vmem          = Nothing
                , pvmem         = Nothing
                , walltime      = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0 }
                }

        it "parse mandatory fields plus qos field" $ do
            let s = "Resource_List.walltime=01:00:00 Resource_List.nodes=1:ppn=1 Resource_List.qos=someqos Resource_List.nodect=1 Resource_List.neednodes=1:ppn=1" :: Text
            s ~> parseTorqueResourceRequest `shouldParse` TorqueResourceRequest
                { mem           = Nothing
                , advres        = Nothing
                , naccesspolicy = Nothing
                , ncpus         = Nothing
                , neednodes     = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , nice          = Nothing
                , nodeCount     = 1
                , nodes         = TSN TorqueJobShortNode { number = 1, ppn = Just 1 }
                , select        = Nothing
                , qos           = Just "someqos"
                , pmem          = Nothing
                , vmem          = Nothing
                , pvmem         = Nothing
                , walltime      = TorqueWalltime { days = 0, hours = 1, minutes = 0, seconds = 0 }
                }

        it "parse 2014 resource List" $ do
            let s = "Resource_List.neednodes=1:ppn=16 Resource_List.nice=0 Resource_List.nodect=1 Resource_List.nodes=1:ppn=16 Resource_List.vmem=74737mb Resource_List.walltime=05:00:00" :: Text
            s ~> parseTorqueResourceRequest `shouldParse` TorqueResourceRequest
                { mem           = Nothing
                , advres        = Nothing
                , naccesspolicy = Nothing
                , ncpus         = Nothing
                , neednodes     = TSN TorqueJobShortNode { number = 1, ppn = Just 16 }
                , nice          = Just 0
                , nodeCount     = 1
                , nodes         = TSN TorqueJobShortNode { number = 1, ppn = Just 16 }
                , select        = Nothing
                , qos           = Nothing
                , pmem          = Nothing
                , vmem          = Just $ 74737 * 1024 * 1024
                , pvmem         = Nothing
                , walltime      = TorqueWalltime { days = 0, hours = 5, minutes = 0, seconds = 0 }
                }

    describe "parseTorqueHostList" $ do
        it "parse comma separated list of single cores" $ do
            let s = "exec_host=node1001.mycluster.mydomain/1,3,5,7" :: Text
            s ~> parseTorqueHostList `shouldParse` [ TorqueExecHost
                { name = "node1001.mycluster.mydomain"
                , cores = [1,3,5,7]
                }]
        it "parse comma separated list of core rangess" $ do
            let s = "exec_host=node1001.mycluster.mydomain/1-3,5-7" :: Text
            s ~> parseTorqueHostList `shouldParse` [ TorqueExecHost
                { name = "node1001.mycluster.mydomain"
                , cores = [1,2,3,5,6,7]
                }]
        it "parse comma separated list of single cores and core ranges" $ do
            let s = "exec_host=node1001.mycluster.mydomain/1,3,5-7,9,12-14" :: Text
            s ~> parseTorqueHostList `shouldParse` [ TorqueExecHost
                { name = "node1001.mycluster.mydomain"
                , cores = [1,3,5,6,7,9,12,13,14]
                }]
        it "parse multiple nodes with one comma separated list of single cores and one core range" $ do
            let s = "exec_host=node1001.mycluster.mydomain/1,3,5,7+node1002.mycluster.mydomain/4-6" :: Text
            s ~> parseTorqueHostList `shouldParse` [
                TorqueExecHost
                    { name = "node1001.mycluster.mydomain"
                    , cores = [1,3,5,7]
                    },
                TorqueExecHost
                    { name = "node1002.mycluster.mydomain"
                    , cores = [4,5,6]
                    }
                ]

    describe "parseTorqueExit" $ do
        it "parse torque 6.0 job exit log line" $ do
            let s = "torque: 04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44" :: Text
            s ~> parseTorqueExit `shouldParse` ("torque", TorqueExit TorqueJobExit
                { torqueDatestamp = "04/05/2017 13:06:53"
                , name = TorqueJobName { number = 45, array_id = Nothing, master = "master23", cluster = "banette" }
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
                    , endTime = Just 1491390413
                    }
                , execHost =
                    [ TorqueExecHost
                        { name = "node2801.banette.gent.vsc"
                        , cores = [0,1]
                        }
                    , TorqueExecHost
                        { name = "node2803.banette.gent.vsc"
                        , cores = [0,1]
                        }
                    ]
                , resourceRequest = TorqueResourceRequest
                    { mem           = Nothing
                    , advres        = Nothing
                    , naccesspolicy = Nothing
                    , ncpus         = Nothing
                    , neednodes = TFN
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
                    , nodes = TFN
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
                    , energy = Just 0
                    , mem = 55048 * 1024
                    , vmem = 92488 * 1024
                    , walltime = TorqueWalltime { days = 0, hours = 0, minutes = 1, seconds = 44 }
                    }
                , totalExecutionSlots = Just 4
                , uniqueNodeCount = Just 2
                , exitStatus = 0
                , torqueEntryType = TorqueExitEntry
                })
        it "parse 2014 torque job exit line" $ do
            let s = "torque: 01/12/2014 23:57:07;E;161299[389].master15.delcatty.gent.vsc;user=vsc40909 group=vsc40909 jobname=30by40XconChoicesResults-389 queue=short ctime=1389546423 qtime=1389546423 etime=1389546423 start=1389567229 owner=vsc40909@gligar02.gligar.gent.vsc exec_host=node2135.delcatty.gent.vsc/0+node2135.delcatty.gent.vsc/1+node2135.delcatty.gent.vsc/2+node2135.delcatty.gent.vsc/3+node2135.delcatty.gent.vsc/4+node2135.delcatty.gent.vsc/5+node2135.delcatty.gent.vsc/6+node2135.delcatty.gent.vsc/7+node2135.delcatty.gent.vsc/8+node2135.delcatty.gent.vsc/9+node2135.delcatty.gent.vsc/10+node2135.delcatty.gent.vsc/11+node2135.delcatty.gent.vsc/12+node2135.delcatty.gent.vsc/13+node2135.delcatty.gent.vsc/14+node2135.delcatty.gent.vsc/15 Resource_List.neednodes=1:ppn=16 Resource_List.nice=0 Resource_List.nodect=1 Resource_List.nodes=1:ppn=16 Resource_List.vmem=74737mb Resource_List.walltime=05:00:00 session=32698 end=1389567427 Exit_status=0 resources_used.cput=00:48:40 resources_used.mem=307504kb resources_used.vmem=1985904kb resources_used.walltime=00:03:21" :: Text
            s ~> parseTorqueExit `shouldParse` ("torque", TorqueExit TorqueJobExit
                { torqueDatestamp = "01/12/2014 23:57:07"
                , name = TorqueJobName { number = 161299, array_id = Just 389, master = "master15", cluster = "delcatty" }
                , user = "vsc40909"
                , group = "vsc40909"
                , jobname = "30by40XconChoicesResults-389"
                , queue = "short"
                , startCount = Nothing
                , owner = "vsc40909@gligar02.gligar.gent.vsc"
                , session = 32698
                , times = TorqueJobTime
                    { ctime = 1389546423
                    , qtime = 1389546423
                    , etime = 1389546423
                    , startTime = 1389567229
                    , endTime = Just 1389567427
                    }
                , execHost =
                    [ TorqueExecHost
                        { name = "node2135.delcatty.gent.vsc"
                        , cores = [0 .. 15]
                        }
                    ]
                , resourceRequest = TorqueResourceRequest
                    { mem = Nothing
                    , advres = Nothing
                    , naccesspolicy = Nothing
                    , ncpus = Nothing
                    , neednodes = TSN TorqueJobShortNode
                        { number = 1
                        , ppn = Just 16
                        }
                    , nice = Just 0
                    , nodeCount = 1
                    , nodes = TSN TorqueJobShortNode
                        { number = 1
                        , ppn = Just 16
                        }
                    , select = Nothing
                    , qos = Nothing
                    , pmem = Nothing
                    , vmem = Just 78367424512
                    , pvmem = Nothing
                    , walltime = TorqueWalltime { days = 0, hours = 5, minutes = 0, seconds = 0}
                    }
                , resourceUsage = TorqueResourceUsage
                    { cputime = 2920
                    , energy = Nothing
                    , mem = 314884096
                    , vmem = 2033565696
                    , walltime = TorqueWalltime { days = 0, hours = 0, minutes = 3, seconds = 21}
                    }
                , totalExecutionSlots = Nothing
                , uniqueNodeCount = Nothing
                , exitStatus = 0
                , torqueEntryType = TorqueExitEntry
            })

    describe "parseTorqueQueue" $ do
        it "parse job queue entry" $ do
            let s = "torque: 06/28/2017 14:31:09;Q;80.master23.banette.gent.vsc;queue=default" :: Text
            s ~> parseTorqueQueue `shouldParse` ("torque", TorqueQueue TorqueJobQueue
                { torqueDatestamp = "06/28/2017 14:31:09"
                , name = TorqueJobName { number = 80, array_id = Nothing, master = "master23", cluster = "banette" }
                , queue = "default"
                , torqueEntryType = TorqueQueueEntry
                })

        it "parse job queue entry master24 - torque 6.0" $ do
            let s = "torque: 07/27/2017 14:17:41;Q;5.master24.banette.gent.vsc;queue=default" :: Text
            s ~> parseTorqueQueue `shouldParse` ("torque", TorqueQueue TorqueJobQueue
                { torqueDatestamp = "07/27/2017 14:17:41"
                , name = TorqueJobName { number = 5, array_id = Nothing, master = "master24" , cluster = "banette" }
                , queue = "default"
                , torqueEntryType = TorqueQueueEntry
                })

    describe "parseTorqueDelete" $
        it "parse job delete entry" $ do
            let s = "torque: 06/28/2017 15:44:02;D;81.master23.banette.gent.vsc;requestor=vsc40075@gligar02.gligar.gent.vsc" :: Text
            s ~> parseTorqueDelete `shouldParse` ("torque", TorqueDelete TorqueJobDelete
                { torqueDatestamp = "06/28/2017 15:44:02"
                , name = TorqueJobName { number = 81, array_id = Nothing, master = "master23", cluster = "banette" }
                , requestor = TorqueRequestor { user = "vsc40075", whence = "gligar02.gligar.gent.vsc" }
                , torqueEntryType = TorqueDeleteEntry
                })

    describe "parseTorqueAbort" $
        it "parse job abort entry" $ do
            let s = "torque: 09/02/2013 17:34:26;A;34106.master15.delcatty.gent.vsc;" :: Text
            s ~> parseTorqueAbort `shouldParse` ("torque", TorqueAbort TorqueJobAbort
                { torqueDatestamp = "09/02/2013 17:34:26"
                , name = TorqueJobName { number = 34106, array_id = Nothing, master = "master15", cluster = "delcatty" }
                , torqueEntryType = TorqueAbortEntry
                })

    describe "parseTorqueStart" $
        it "parse job start" $ do
            let s = "torque: 06/20/2017 11:24:49;S;63.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1497950675 qtime=1497950675 etime=1497950675 start=1497950689 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0 Resource_List.vmem=4224531456b Resource_List.nodes=1:ppn=1 Resource_List.walltime=00:10:00 Resource_List.nodect=1 Resource_List.neednodes=1:ppn=1 Resource_List.nice=0" :: Text
            s ~> parseTorqueStart `shouldParse` ("torque", TorqueStart TorqueJobStart
                { torqueDatestamp = "06/20/2017 11:24:49"
                , name = TorqueJobName { number = 63, array_id = Nothing, master = "master23", cluster = "banette" }
                , user = "vsc40075"
                , group = "vsc40075"
                , jobname = "STDIN"
                , queue = "short"
                , owner = "vsc40075@gligar01.gligar.gent.vsc"
                , times = TorqueJobTime
                    { ctime = 1497950675
                    , qtime = 1497950675
                    , etime = 1497950675
                    , startTime = 1497950689
                    , endTime = Nothing
                    }
                , execHost =
                    [ TorqueExecHost
                        { name = "node2801.banette.gent.vsc"
                        , cores = [0]
                        }
                    ]
                , resourceRequest = TorqueResourceRequest
                    { mem           = Nothing
                    , advres        = Nothing
                    , naccesspolicy = Nothing
                    , ncpus         = Nothing
                    , neednodes = TSN
                        TorqueJobShortNode
                            { number = 1
                            , ppn  = Just 1
                            }
                    , nice      = Just 0
                    , nodeCount = 1
                    , nodes = TSN
                        TorqueJobShortNode
                            { number = 1
                            , ppn  = Just 1
                            }
                    , select        = Nothing
                    , qos           = Nothing
                    , vmem = Just 4224531456
                    , pmem = Nothing
                    , pvmem = Nothing
                    , walltime  = TorqueWalltime { days = 0, hours = 0, minutes = 10, seconds = 0}
                    }
                , torqueEntryType = TorqueStartEntry
                })
