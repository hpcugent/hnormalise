{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HNormalise.Rsyslog.Json where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Aeson

--------------------------------------------------------------------------------

import HNormalise.Rsyslog.Internal
--------------------------------------------------------------------------------


instance FromJSON Rsyslog where
    parseJSON = withObject "Rsyslog" $ \v -> Rsyslog <$>
            (v .: "msg") <*>
            (v .: "rawmsg") <*>
            (v .: "timereported") <*>
            (v .: "hostname") <*>
            (v .: "syslogtag") <*>
            (v .: "inputname") <*>
            (v .: "fromhost") <*>
            (v .: "fromhost-ip") <*>
            (v .: "pri") <*>
            (v .: "syslogfacility") <*>
            (v .: "syslogseverity") <*>
            (v .: "timegenerated") <*>
            (v .: "programname") <*>
            (v .: "protocol-version") <*>
            (v .: "structured-data") <*>
            (v .: "app-name") <*>
            (v .: "procid") <*>
            (v .: "msgid") <*>
            (v .: "uuid") <*>
            (v .: "$!")

instance ToJSON Rsyslog where
    toJSON = genericToJSON defaultOptions
