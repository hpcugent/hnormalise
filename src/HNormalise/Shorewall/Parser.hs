{- hnormalise - a log normalisation library
 -
 - Copyright Ghent University (c) 2017-2019
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

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Shorewall.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import           Data.Text                  (Text)

--------------------------------------------------------------------------------
import           HNormalise.Common.Parser
import           HNormalise.Shorewall.Internal

--------------------------------------------------------------------------------
parseShorewallTCP :: Parser Shorewall
parseShorewallTCP = do
    string "kernel:: Shorewall:"
    fwrule <- takeTill (== ':')
    fwtarget <- char ':' *> takeTill (== ':')
    fwin <- char ':' *> kvTextParser "IN"
    fwmac <- skipSpace *> string "OUT=" *> skipSpace *> kvParser >>= return . snd           -- FIXME: ideally this parses a MAC address
    fwsrc <- skipSpace *> kvHostOrIPParser "SRC"
    fwdst <- skipSpace *> kvHostOrIPParser "DST"
    manyTill anyChar (lookAhead $ string " PROTO=")
    string " PROTO=TCP"
    fwspt <- skipSpace *> kvNumParser "SPT"
    fwdpt <- skipSpace *> kvNumParser "DPT"
    takeText
    return $ Shorewall
        { fwrule = fwrule
        , fwtarget = fwtarget
        , fwin = fwin
        , fwout = Nothing
        , fwmac = Just fwmac
        , fwsrc = fwsrc
        , fwdst = fwdst
        , fwproto = TCP
        , fwspt = Just fwspt
        , fwdpt = Just fwdpt
        }

        {-
        SHOREWALL_UDP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=%{WORD:fwout}.*?SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} SPT\=%{INT:fwspt:int} DPT\=%{INT:fwdpt:int} .*?
        SHOREWALL_MSG (?:%{SHOREWALL_TCP}|%{SHOREWALL_UDP}|%{SHOREWALL_ICMP})
            "raw" : "2016-03-31T23:45:27.615225+02:00 gastly kernel: - kernel:: Shorewall:ipmi2int:REJECT:IN=em4 OUT=em1 SRC=10.0.0.2 DST=10.0.0.1 LEN=57 TOS=0x00 PREC=0x00 TTL=63 ID=62392 PROTO=UDP SPT=57002 DPT=53 LEN=37",
        -}

--------------------------------------------------------------------------------
parseShorewallUDP :: Parser Shorewall
parseShorewallUDP = do
    string "kernel:: Shorewall:"
    fwrule <- takeTill (== ':')
    fwtarget <- char ':' *> takeTill (== ':')
    fwin <- char ':' *> kvTextParser "IN"
    fwout <- skipSpace *> kvTextParser "OUT"
    fwsrc <- skipSpace *> kvHostOrIPParser "SRC"
    fwdst <- skipSpace *> kvHostOrIPParser "DST"
    manyTill anyChar (lookAhead $ string "PROTO=")
    string "PROTO=UDP"
    fwspt <- skipSpace *> kvNumParser "SPT"
    fwdpt <- skipSpace *> kvNumParser "DPT"
    takeText
    return $ Shorewall
        { fwrule = fwrule
        , fwtarget = fwtarget
        , fwin = fwin
        , fwout = Just fwout
        , fwmac = Nothing
        , fwsrc = fwsrc
        , fwdst = fwdst
        , fwproto = UDP
        , fwspt = Just fwspt
        , fwdpt = Just fwdpt
        }


        {-
        SHOREWALL_ICMP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=%{WORD:fwout} SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} .*?
        SHOREWALL_MSG (?:%{SHOREWALL_TCP}|%{SHOREWALL_UDP}|%{SHOREWALL_ICMP})
            "raw" : "2016-04-07T09:27:26.729986+02:00 gastly kernel: - kernel:: Shorewall:ipmi2ext:REJECT:IN=em4 OUT=em3 SRC=10.0.0.2 DST=10.0.0.1 LEN=28 TOS=0x00 PREC=0x00 TTL=63 ID=36216 PROTO=ICMP TYPE=8 CODE=0 ID=0 SEQ=1421",
        -}

--------------------------------------------------------------------------------
parseShorewallICMP :: Parser Shorewall
parseShorewallICMP = do
    string "kernel:: Shorewall:"
    fwrule <- takeTill (== ':')
    fwtarget <- char ':' *> takeTill (== ':')
    fwin <- char ':' *> kvTextParser "IN"
    fwout <- skipSpace *> kvTextParser "OUT"
    fwsrc <- skipSpace *> kvHostOrIPParser "SRC"
    fwdst <- skipSpace *> kvHostOrIPParser "DST"
    manyTill anyChar (lookAhead $ string " PROTO=")
    string " PROTO=ICMP"
    takeText
    return $ Shorewall
        { fwrule = fwrule
        , fwtarget = fwtarget
        , fwin = fwin
        , fwout = Just fwout
        , fwmac = Nothing
        , fwsrc = fwsrc
        , fwdst = fwdst
        , fwproto = ICMP
        , fwspt = Nothing
        , fwdpt = Nothing
        }

parseShorewall :: Parser (Text, Shorewall)
parseShorewall = do
    s <-    parseShorewallTCP
        <|> parseShorewallUDP
        <|> parseShorewallICMP
    return ("kernel", s)
