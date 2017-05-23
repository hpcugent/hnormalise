{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import           Criterion.Main
import qualified Data.Attoparsec.Text      as AT
import           Data.Text                 (Text)
--------------------------------------------------------------------------------
import qualified HNormalise.Lmod.Parser  as LmodP
import qualified HNormalise.Shorewall.Parser  as ShorewallP
import qualified HNormalise.Torque.Parser  as TorqueP


--------------------------------------------------------------------------------
torqueJobExitInput1 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44" :: Text
torqueJobExitInput2 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44" :: Text

torqueJobExitFailInput1 = "04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short HUPPEL"

lmodLoadInput1 = "lmod::  username=myuser, cluster=mycluster, jobid=3230905.master.mycluster.mydomain, userload=yes, module=GSL/2.3-intel-2016b, fn=/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b" :: Text

shorewallTCPInput = " - kernel:: Shorewall:ext2fw:REJECT:IN=em3 OUT= MAC=aa:aa:bb:ff:88:bc:bc:15:80:8b:f8:f8:80:00 SRC=78.0.0.1 DST=150.0.0.1 LEN=52 TOS=0x00 PREC=0x00 TTL=117 ID=7564 DF PROTO=TCP SPT=60048 DPT=22 WINDOW=65535 RES=0x00 SYN URGP=0"
shorewallUDPInput = " - kernel:: Shorewall:ipmi2int:REJECT:IN=em4 OUT=em1 SRC=10.0.0.2 DST=10.0.0.1 LEN=57 TOS=0x00 PREC=0x00 TTL=63 ID=62392 PROTO=UDP SPT=57002 DPT=53 LEN=37"
shorewallICMPInput = " - kernel:: Shorewall:ipmi2ext:REJECT:IN=em4 OUT=em3 SRC=10.0.0.2 DST=10.0.0.1 LEN=28 TOS=0x00 PREC=0x00 TTL=63 ID=36216 PROTO=ICMP TYPE=8 CODE=0 ID=0 SEQ=1421"

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ bgroup "torque"
        [ bench "jobexit full resource node list"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitInput1
        , bench "jobexit short resource node number"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitInput1
        , bench "jobexit borked input"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitFailInput1
        ]
    , bgroup "lmod"
        [ bench "lmod successfull module load parse" $ whnf (AT.parse LmodP.parseLmodLoad) lmodLoadInput1
        ]
    , bgroup "shorewall"
        [ bench "tcp successfull input" $ whnf (AT.parse ShorewallP.parseShorewallTCP) shorewallTCPInput
        , bench "udp successfull input" $ whnf (AT.parse ShorewallP.parseShorewallUDP) shorewallUDPInput
        , bench "icmp successfull input" $ whnf (AT.parse ShorewallP.parseShorewallICMP) shorewallICMPInput
        ]
    ]
