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

{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Json where

--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8       as SBS
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.HashMap.Lazy   as M
import           Data.Monoid

--------------------------------------------------------------------------------
import           HNormalise.Internal

--------------------------------------------------------------------------------
instance FromJSON Rsyslog where
    parseJSON = withObject "Rsyslog" $ \v ->
        Rsyslog <$>
            (v .:  "msg")              <*>
    --        (v .:  "rawmsg")           <*>
            (v .:  "timereported")     <*>
            (v .:  "hostname")         <*>
            (v .:  "syslogtag")        <*>
            (v .:  "inputname")        <*>
            (v .:  "fromhost")         <*>
            (v .:  "fromhost-ip")      <*>
            (v .:  "pri")              <*>
            (v .:  "version")          <*>               -- no idea with what the version matches in the JSON message format, see also http://www.rsyslog.com/doc/master/configuration/properties.html
            (v .:  "syslogfacility")   <*>
            (v .:  "syslogseverity")   <*>
            (v .:  "timegenerated")    <*>
            (v .:  "programname")      <*>
            (v .:  "protocol-version") <*>
    --        (v .:  "structured-data")  <*>
            (v .:  "app-name")         <*>
            (v .:  "procid")
    --        (v .:  "msgid")            <*>
    --        (v .:? "uuid")             <*>
    --        (v .:? "$!")

--------------------------------------------------------------------------------
instance ToJSON Rsyslog where
    toEncoding = genericToEncoding defaultOptions
    toJSON = genericToJSON defaultOptions

--------------------------------------------------------------------------------
instance ToJSON NormalisedRsyslog where
    toEncoding (NRsyslog r n k fs) =
        case fs of
            -- this is the default
            Nothing -> pairs
                        (  "message" .= msg r
                        <> "syslog_abspri" .= pri r
                        <> "syslog_version" .= version r
                        <> "program" .= app_name r
                        <> "@source_host" .= hostname r          -- the host in ES will likely be set to the machine sending the data to logstash
                        <> k .= n
                        )
            Just fs' -> case toJSON r of
                            Object syslog -> pairs $ foldl (\v (key, fieldname) -> v <> key .= M.lookupDefault Null fieldname syslog) (k .= n) fs'
                            _ -> pairs (k .= n) -- FIXME: this should never happen

encodeNormalisedRsyslog :: NormalisedRsyslog
                        -> SBS.ByteString
encodeNormalisedRsyslog = BS.toStrict . encode
