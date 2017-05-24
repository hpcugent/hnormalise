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
    string " - kernel:: Shorewall:"
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
    string " - kernel:: Shorewall:"
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

parseShorewall :: Parser Shorewall
parseShorewall =
        parseShorewallTCP
    <|> parseShorewallUDP
    <|> parseShorewallICMP
