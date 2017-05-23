{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import Data.Attoparsec.Text
import Test.Hspec

--------------------------------------------------------------------------------
import Lmod.Parser
import Lmod.Internal
--------------------------------------------------------------------------------
{-exitTestLine1 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44\n"
exitTestLine2 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=2:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=2:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44\n"
lmodload1 = "lmod::  username=vsc41480, cluster=delcatty, jobid=3230905.master15.delcatty.gent.vsc, userload=yes, module=GSL/2.3-intel-2016b, fn=/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b\n"
lmodload2 = "lmod::  username=vsc42163, cluster=raichu, jobid=, userload=yes, module=cluster/raichu, fn=/etc/modulefiles/vsc/cluster/raichu.lua\n"

doTest _ = do
    let t = parse parseTorqueExit exitTestLine1
    case t of
        Done _ te -> BS.putStrLn $ encodePretty te
        _ -> putStrLn "Failed"
    let t = parse parseTorqueExit exitTestLine2
    case t of
        Done _ te -> BS.putStrLn $ encodePretty te
        _ -> putStrLn "Failed"
    let t = parse parseLmodLoad lmodload2
    case t of
        Done _ te -> BS.putStrLn $ encodePretty te
        x -> putStr "Failed: " >> print x
-}



exitTestLine1 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44\n"
exitTestLine2 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=2:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=2:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44\n"
lmodload1 = "lmod::  username=vsc40075, cluster=delcatty, jobid=3230905.master15.delcatty.gent.vsc, userload=yes, module=GSL/2.3-intel-2016b, fn=/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b\n"
lmodload2 = "lmod::  username=vsc40075, cluster=raichu, jobid=, userload=yes, module=cluster/raichu, fn=/etc/modulefiles/vsc/cluster/raichu.lua\n"


main :: IO ()
main = hspec $ do
    describe "lmod load parsing" $ do
        it "skips over empty fields" $
            case parse parseLmodLoad lmodload1 of
                Done _ r -> r `shouldBe` (Done "" $
                                LmodLoad
                                    { info = LmodInfo
                                        { username = "vsc40075"
                                        , cluster = "delcatty"
                                        , jobid = "3230905.master15.delcatty.gent.vsc"
                                        }
                                    , modul = LmodModule
                                        { name = "GSL"
                                        , version = "2.3-intel-2016b"
                                        }
                                    , filename = "/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b"
                                    }
                                )
                _ ->
