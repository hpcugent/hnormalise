{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Shorewall.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import qualified Data.Text                  as T
--------------------------------------------------------------------------------

import           HNormalise.Common.Parser
import           HNormalise.Shorewall.Internal
--------------------------------------------------------------------------------

{-
SHOREWALL_TCP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=\s*MAC\=%{DATA:fwmac} SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} SPT\=%{INT:fwspt:int} DPT\=%{INT:fwdpt:int} .*?
SHOREWALL_UDP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=%{WORD:fwout}.*?SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} SPT\=%{INT:fwspt:int} DPT\=%{INT:fwdpt:int} .*?
SHOREWALL_ICMP .*?%{WORD:fwrule}\:%{WORD:fwtarget}\:IN\=%{WORD:fwin} OUT\=%{WORD:fwout} SRC\=%{IPORHOST:fwsrc} DST\=%{IPORHOST:fwdst} .*? PROTO\=%{WORD:fwproto} .*?

SHOREWALL_MSG (?:%{SHOREWALL_TCP}|%{SHOREWALL_UDP}|%{SHOREWALL_ICMP})

    "raw" : "2016-04-07T09:27:26.729986+02:00 gastly kernel: - kernel:: Shorewall:ipmi2ext:REJECT:IN=em4 OUT=em3 SRC=10.0.0.2 DST=10.0.0.1 LEN=28 TOS=0x00 PREC=0x00 TTL=63 ID=36216 PROTO=ICMP TYPE=8 CODE=0 ID=0 SEQ=1421",
    "raw" : "2016-03-31T23:45:27.615225+02:00 gastly kernel: - kernel:: Shorewall:ipmi2int:REJECT:IN=em4 OUT=em1 SRC=10.0.0.2 DST=10.0.0.1 LEN=57 TOS=0x00 PREC=0x00 TTL=63 ID=62392 PROTO=UDP SPT=57002 DPT=53 LEN=37",
    "raw" : "2016-03-29T16:10:49.386951+02:00 gligar03 kernel: - kernel:: Shorewall:ext2fw:REJECT:IN=em3 OUT= MAC=aa:aa:bb:ff:88:bc:bc:15:80:8b:f8:f8:80:00 SRC=78.0.0.1 DST=150.0.0.1 LEN=52 TOS=0x00 PREC=0x00 TTL=117 ID=7564 DF PROTO=TCP SPT=60048 DPT=22 WINDOW=65535 RES=0x00 SYN URGP=0",
-}

parseShorewallTCP :: Parser Shorewall
parseShorewallTCP = do
    string "kernel:: Shorewall:"
    fwrule <- takeTill (== ':')
    fwtarget <- char ':' *> takeTill (== ':')
    fwin <- char ':' *> kvTextParser "IN"
    fwmac <- skipSpace *> string "OUT=" *> skipSpace *> kvParser >>= return . snd           -- FIXME: ideally this parses a MAC address
    fwsrc <- skipSpace *> kvHostOrIPParser "SRC"
    fwdst <- skipSpace *> kvHostOrIPParser "DST"
    manyTill anyChar (lookAhead $ string " PROTO")
    fwproto <- skipSpace *> kvTextParser "PROTO"
    fwspt <- skipSpace *> kvNumParser "SPT"
    fwdpt <- skipSpace *> kvNumParser "DPT"
    takeText
    return $ Shorewall
        { fwtype = TCP
        , fwrule = fwrule
        , fwtarget = fwtarget
        , fwin = fwin
        , fwout = Nothing
        , fwmac = Just fwmac
        , fwsrc = fwsrc
        , fwdst = fwdst
        , fwproto = T.empty --fwproto
        , fwspt = Nothing -- Just fwspt
        , fwdpt = Nothing -- Just fwdpt
        }



parseShorewallUDP = undefined
parseShorewallICMP = undefined

parseShorewall :: Parser Shorewall
parseShorewall =
        parseShorewallTCP
    <|> parseShorewallUDP
    <|> parseShorewallICMP
