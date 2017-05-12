{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Huppel.Json where

--------------------------------------------------------------------------------
import Data.Aeson

--------------------------------------------------------------------------------

import Rsyslog.Internal
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
