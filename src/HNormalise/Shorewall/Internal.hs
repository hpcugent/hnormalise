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

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Shorewall.Internal where

{-
SHOREWALL_TCP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=\s*MAC\=%{DATA:fwmac} SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} SPT\=%{INT:fwspt:int} DPT\=%{INT:fwdpt:int} .*?
SHOREWALL_UDP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=%{WORD:fwout}.*?SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} SPT\=%{INT:fwspt:int} DPT\=%{INT:fwdpt:int} .*?
SHOREWALL_ICMP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=%{WORD:fwout} SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} .*?

SHOREWALL_MSG (?:%{SHOREWALL_TCP}|%{SHOREWALL_UDP}|%{SHOREWALL_ICMP})

    "raw" : "2016-04-07T09:27:26.729986+02:00 gastly kernel: - kernel:: Shorewall:ipmi2ext:REJECT:IN=em4 OUT=em3 SRC=10.0.0.2 DST=10.0.0.1 LEN=28 TOS=0x00 PREC=0x00 TTL=63 ID=36216 PROTO=ICMP TYPE=8 CODE=0 ID=0 SEQ=1421",
    "raw" : "2016-03-31T23:45:27.615225+02:00 gastly kernel: - kernel:: Shorewall:ipmi2int:REJECT:IN=em4 OUT=em1 SRC=10.0.0.2 DST=10.0.0.1 LEN=57 TOS=0x00 PREC=0x00 TTL=63 ID=62392 PROTO=UDP SPT=57002 DPT=53 LEN=37",
    "raw" : "2016-03-29T16:10:49.386951+02:00 gligar03 kernel: - kernel:: Shorewall:ext2fw:REJECT:IN=em3 OUT= MAC=aa:aa:bb:ff:88:bc:bc:15:80:8b:f8:f8:80:00 SRC=78.0.0.1 DST=150.0.0.1 LEN=52 TOS=0x00 PREC=0x00 TTL=117 ID=7564 DF PROTO=TCP SPT=60048 DPT=22 WINDOW=65535 RES=0x00 SYN URGP=0",
-}
--------------------------------------------------------------------------------
import           Data.Aeson                 (FromJSON, ToJSON, toEncoding,
                                             toJSON)
import           Data.Text
import           GHC.Generics               (Generic)

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal

--------------------------------------------------------------------------------
data ShorewallProtocol = TCP | UDP | ICMP deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
data Shorewall = Shorewall
    { fwrule :: !Text
    , fwtarget :: !Text
    , fwin :: !Text
    , fwout :: !(Maybe Text)
    , fwmac :: !(Maybe Text)
    , fwsrc :: !Host
    , fwdst :: !Host
    , fwproto :: !ShorewallProtocol
    , fwspt :: !(Maybe Integer)
    , fwdpt :: !(Maybe Integer)
    } deriving (Show, Eq, Generic)
