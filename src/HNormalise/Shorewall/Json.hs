{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Shorewall.Json where

--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.Monoid

--------------------------------------------------------------------------------
import           HNormalise.Common.Json
import           HNormalise.Shorewall.Internal

--------------------------------------------------------------------------------
instance ToJSON ShorewallProtocol where
    toJSON TCP = String "TCP"
    toJSON UDP = String "UDP"
    toJSON ICMP = String "ICMP"
--------------------------------------------------------------------------------
instance ToJSON Shorewall where
    toEncoding (Shorewall fwrule fwtarget fwin fwout fwmac fwsrc fwdst fwproto fwspt fwdpt) =
        pairs
            (  "fwrule" .= fwrule
            <> "fwtarget" .= fwtarget
            <> "fwin" .= fwin
            <> case fwout of
                    Nothing -> mempty
                    Just f  -> "fwout" .= f
            <> case fwmac of
                    Nothing -> mempty
                    Just m  -> "fwmac" .= m
            <> "fwsrc" .= fwsrc
            <> "fwdst" .= fwdst
            <> "fwproto" .= fwproto
            <> case fwspt of
                    Nothing -> mempty
                    Just p  -> "fwspt" .= p
            <> case fwdpt of
                    Nothing -> mempty
                    Just p  -> "fwdst" .= p
            )
