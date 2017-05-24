{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module HNormalise.Json where

--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
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

--------------------------------------------------------------------------------
instance ToJSON NormalisedRsyslog where
    toEncoding (NRsyslog r n k) =
        pairs
            (  "message" .= msg r
            <> "syslog_abspri" .= syslogseverity r
            <> "program" .= app_name r
            <> k .= n
            )
