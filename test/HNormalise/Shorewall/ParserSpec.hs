{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HNormalise.Shorewall.ParserSpec (main, spec) where

--------------------------------------------------------------------------------
import           Data.Text                 (Text)
import qualified Data.Text.Read            as TR
import           Test.Hspec
import           Test.Hspec.Attoparsec
import qualified Net.IPv4                  as NT

--------------------------------------------------------------------------------
import           HNormalise.Common.Internal
import           HNormalise.Shorewall.Parser
import           HNormalise.Shorewall.Internal
--------------------------------------------------------------------------------
main :: IO ()
main = hspec spec

--------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "parseShorewall" $ do
        it "Shorewall UDP message" $ do
            let s = " - kernel:: Shorewall:ipmi2int:REJECT:IN=em4 OUT=em1 SRC=10.0.0.2 DST=10.0.0.1 LEN=57 TOS=0x00 PREC=0x00 TTL=63 ID=62392 PROTO=UDP SPT=57002 DPT=53 LEN=37" :: Text
            s ~> parseShorewallUDP `shouldParse` Shorewall
                { fwrule = "ipmi2int"
                , fwtarget = "REJECT"
                , fwin = "em4"
                , fwout = Just "em1"
                , fwmac = Nothing
                , fwsrc = IPv4 $ NT.fromOctets 10 0 0 2
                , fwdst = IPv4 $ NT.fromOctets 10 0 0 1
                , fwproto = UDP
                , fwspt = Just 57002
                , fwdpt = Just 53
                }

        it "Shorewall TCP message" $ do
            let s = " - kernel:: Shorewall:ext2fw:REJECT:IN=em3 OUT= MAC=aa:aa:bb:ff:88:bc:bc:15:80:8b:f8:f8:80:00 SRC=78.0.0.1 DST=150.0.0.1 LEN=52 TOS=0x00 PREC=0x00 TTL=117 ID=7564 DF PROTO=TCP SPT=60048 DPT=22 WINDOW=65535 RES=0x00 SYN URGP=0" :: Text
            s ~> parseShorewallTCP `shouldParse` Shorewall
                { fwrule = "ext2fw"
                , fwtarget = "REJECT"
                , fwin = "em3"
                , fwout = Nothing
                , fwmac = Just "aa:aa:bb:ff:88:bc:bc:15:80:8b:f8:f8:80:00"
                , fwsrc = IPv4 $ NT.fromOctets 78 0 0 1
                , fwdst = IPv4 $ NT.fromOctets 150 0 0 1
                , fwproto = TCP
                , fwspt = Just 60048
                , fwdpt = Just 22
                }

        it "Shorewall ICMP message" $ do
            let s = " - kernel:: Shorewall:ipmi2ext:REJECT:IN=em4 OUT=em3 SRC=10.0.0.2 DST=10.0.0.1 LEN=28 TOS=0x00 PREC=0x00 TTL=63 ID=36216 PROTO=ICMP TYPE=8 CODE=0 ID=0 SEQ=1421" :: Text
            s ~> parseShorewallICMP `shouldParse` Shorewall
                { fwrule = "ipmi2ext"
                , fwtarget = "REJECT"
                , fwin = "em4"
                , fwout = Just "em3"
                , fwmac = Nothing
                , fwsrc = IPv4 $ NT.fromOctets 10 0 0 2
                , fwdst = IPv4 $ NT.fromOctets 10 0 0 1
                , fwproto = ICMP
                , fwspt = Nothing
                , fwdpt = Nothing
                }
