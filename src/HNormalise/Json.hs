{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Json where

--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy     as M
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
        if null fs then
            -- default
            pairs
                (  "message" .= msg r
                <> "syslog_abspri" .= pri r
                <> "syslog_version" .= version r
                <> "program" .= app_name r
                <> "@source_host" .= hostname r          -- the host in ES will likely be set to the machine sending the data to logstash
                <> k .= n
                )
        else
            case toJSON r of
                Object syslog -> pairs $ foldl (\v (key, fieldname) -> v <> key .= (M.lookupDefault Null fieldname syslog)) (k .= n) fs
                _ -> pairs (k .= n)
