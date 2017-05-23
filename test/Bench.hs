{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import           Criterion.Main
import qualified Data.Attoparsec.Text      as AT
import           Data.Text                 (Text)
--------------------------------------------------------------------------------
import qualified HNormalise.Torque.Parser  as TorqueP


--------------------------------------------------------------------------------
torqueJobExitInput1 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44" :: Text
torqueJobExitInput2 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44" :: Text

torqueJobExitFailInput1 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short HUPPEL"

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ bgroup "torque"
        [ bench "jobexit full resource node list"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitInput1
        , bench "jobexit short resource node number"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitInput1
        , bench "jobexit borked input"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitFailInput1
        ]
    ]
