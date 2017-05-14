{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Rsyslog.Internal where


--------------------------------------------------------------------------------
import Data.Text
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------
{-

{ "msg": "huppel 9994",
"rawmsg": "huppel 9994",
"timereported": "2017-05-13T00:40:03.983699+02:00",
"hostname": "test2802",
"syslogtag": "hnormalise",
"inputname": "imfile",
"fromhost": "",
"fromhost-ip": "",
"pri": "133",
"syslogfacility": "16",
"syslogseverity": "5",
"timegenerated": "2017-05-13T00:40:03.983699+02:00",
"programname": "hnormalise",
"protocol-version": "0",
"structured-data": "-",
"app-name": "hnormalise",
"procid": "-",
"msgid": "-",
"uuid": null,
"$!": null }

-}

data Rsyslog = Rsyslog
    { msg:: Text
    , rawmsg:: Text
    , timereported:: Text
    , hostname:: Text
    , syslogtag :: Text  -- Could be a list?
    , inputname :: Text
    , fromhost :: Text
    , fromhost_ip :: Text
    , pri :: Text
    , syslogfacility :: Text
    , syslogseverity :: Text
    , timegenerated :: Text
    , programname :: Text
    , protocol_version :: Text
    , structured_data :: Text
    , app_name :: Text
    , procid :: Text
    , msgid :: Text
    , uuid :: Text
    , all_json :: Text
    } deriving (Eq, Show, Generic)
