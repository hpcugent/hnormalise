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

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import           Control.DeepSeq
import           Criterion.Main
import qualified Data.Aeson                   as Aeson
import qualified Data.Attoparsec.Text         as AT
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)

--------------------------------------------------------------------------------
import qualified HNormalise.Lmod.Parser       as LmodP
import qualified HNormalise.Shorewall.Parser  as ShorewallP
import qualified HNormalise.Snoopy.Parser     as SnoopyP
import qualified HNormalise.Torque.Parser     as TorqueP
import qualified HNormalise                   as H

--------------------------------------------------------------------------------
torqueJobExitInput1 = "torque: 04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=node2801.banette.gent.vsc:ppn=2+node2803.banette.gent.vsc:ppn=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44"
torqueJobExitInput2 = "torque: 04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1491390300 qtime=1491390300 etime=1491390300 start=1491390307 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0-1+node2803.banette.gent.vsc/0-1 Resource_List.nodes=2 Resource_List.vmem=1gb Resource_List.nodect=2 Resource_List.neednodes=2 Resource_List.nice=0 Resource_List.walltime=01:00:00 session=15273 total_execution_slots=4 unique_node_count=2 end=1491390413 Exit_status=0 resources_used.cput=0 resources_used.energy_used=0 resources_used.mem=55048kb resources_used.vmem=92488kb resources_used.walltime=00:01:44"

torqueJobExitFailInput1 = "torque: 04/05/2017 13:06:53;E;45.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short HUPPEL"

torqueJobQueueInput = "torque: 06/28/2017 14:31:09;Q;80.master23.banette.gent.vsc;queue=default"
torqueJobDeleteInput = "torque: 06/28/2017 15:44:02;D;81.master23.banette.gent.vsc;requestor=vsc40075@gligar02.gligar.gent.vsc"
torqueJobStartInput = "torque: 06/20/2017 11:24:49;S;63.master23.banette.gent.vsc;user=vsc40075 group=vsc40075 jobname=STDIN queue=short ctime=1497950675 qtime=1497950675 etime=1497950675 start=1497950689 owner=vsc40075@gligar01.gligar.gent.vsc exec_host=node2801.banette.gent.vsc/0 Resource_List.vmem=4224531456b Resource_List.nodes=1:ppn=1 Resource_List.walltime=00:10:00 Resource_List.nodect=1 Resource_List.neednodes=1:ppn=1 Resource_List.nice=0"

lmodLoadInput1 = "lmod::  username=myuser, cluster=mycluster, jobid=3230905.master.mycluster.mydomain, userload=yes, module=GSL/2.3-intel-2016b, fn=/apps/gent/CO7/sandybridge/modules/all/GSL/2.3-intel-2016b"
fullLmodInput = encodeUtf8 "<13>1 2016-06-07T17:50:22.495571+02:00 node2159 lmod: - lmod:: username=vsc40307, cluster=delcatty, jobid=434.master16.delcatty.gent.vsc, userload=no, module=binutils/2.25-GCCcore-4.9.3, fn=/apps/gent/SL6/sandybridge/modules/all/binutils/2.25-GCCcore-4.9.3"

shorewallTCPInput = "kernel:: Shorewall:ext2fw:REJECT:IN=em3 OUT= MAC=aa:aa:bb:ff:88:bc:bc:15:80:8b:f8:f8:80:00 SRC=78.0.0.1 DST=150.0.0.1 LEN=52 TOS=0x00 PREC=0x00 TTL=117 ID=7564 DF PROTO=TCP SPT=60048 DPT=22 WINDOW=65535 RES=0x00 SYN URGP=0"
shorewallUDPInput = "kernel:: Shorewall:ipmi2int:REJECT:IN=em4 OUT=em1 SRC=10.0.0.2 DST=10.0.0.1 LEN=57 TOS=0x00 PREC=0x00 TTL=63 ID=62392 PROTO=UDP SPT=57002 DPT=53 LEN=37"
shorewallICMPInput = "kernel:: Shorewall:ipmi2ext:REJECT:IN=em4 OUT=em3 SRC=10.0.0.2 DST=10.0.0.1 LEN=28 TOS=0x00 PREC=0x00 TTL=63 ID=36216 PROTO=ICMP TYPE=8 CODE=0 ID=0 SEQ=1421"

snoopyInput = "snoopy[27316]::  [uid:110 sid:9379 tty:(none) cwd:/ filename:/usr/lib64/nagios/plugins/hpc/check_ifutil.pl]: /usr/lib64/nagios/plugins/hpc/check_ifutil.pl -i em1.295 -w 90 -c 95 -p -b 10000m"
fullSnoopyInput = encodeUtf8 "<86>1 2015-12-20T09:59:40.844711+01:00 gligar03 snoopy[46513]: - snoopy[46513]:: [uid:2540337 sid:19403 tty:ERROR(ttyname_r->EUNKNOWN) cwd:/vscmnt/gent_vulpix/_/user/home/gent/vsc403/vsc40337/UCS_LABELLED_NEW/20000_to_30000 filename:/usr/bin/qsub]: qsub -l walltime=72:00:00 job7_21293_30000_doit"


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ bgroup "parse torque"
        [ bench "jobexit full resource node list"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitInput1
        , bench "jobexit short resource node number"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitInput1
        , bench "jobexit borked input"  $ whnf (AT.parse TorqueP.parseTorqueExit) torqueJobExitFailInput1
        , bench "jobqueue input" $ whnf (AT.parse TorqueP.parseTorqueQueue) torqueJobQueueInput
        , bench "jobdelete input" $ whnf (AT.parse TorqueP.parseTorqueDelete) torqueJobDeleteInput
        , bench "jobstart input" $ whnf (AT.parse TorqueP.parseTorqueStart) torqueJobStartInput
        ]
    , bgroup "parse lmod"
        [ bench "lmod successfull module load parse" $ whnf (AT.parse LmodP.parseLmodLoad) lmodLoadInput1]
    , bgroup "parse shorewall"
        [ bench "tcp successfull input" $ whnf (AT.parse ShorewallP.parseShorewallTCP) shorewallTCPInput
        , bench "udp successfull input" $ whnf (AT.parse ShorewallP.parseShorewallUDP) shorewallUDPInput
        , bench "icmp successfull input" $ whnf (AT.parse ShorewallP.parseShorewallICMP) shorewallICMPInput
        ]
    , bgroup "parse snoopy"
        [ bench "successfull input" $ whnf (AT.parse SnoopyP.parseSnoopy) snoopyInput]
    , bgroup "normaliseText"
        [ bench "snoopy" $ nf (H.normaliseText Nothing) fullSnoopyInput
        , bench "lmod" $ nf (H.normaliseText Nothing) fullLmodInput
        ]
    ]
