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
