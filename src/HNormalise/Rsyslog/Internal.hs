{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}

module HNormalise.Rsyslog.Internal where

--------------------------------------------------------------------------------
import Data.Aeson             (FromJSON, ToJSON)
import Data.Text
import GHC.Generics           (Generic)

--------------------------------------------------------------------------------
data Rsyslog = Rsyslog
    { msg              :: Text
    , rawmsg           :: Text
    , timereported     :: Text
    , hostname         :: Text
    , syslogtag        :: Text  -- Could be a list?
    , inputname        :: Text
    , fromhost         :: Text
    , fromhost_ip      :: Text
    , pri              :: Text
    , syslogfacility   :: Text
    , syslogseverity   :: Text
    , timegenerated    :: Text
    , programname      :: Text
    , protocol_version :: Text
    , structured_data  :: Text
    , app_name         :: Text
    , procid           :: Text
    , msgid            :: Text
    , uuid             :: Maybe Text
    , all_json         :: Maybe Text
    } deriving (Eq, Show, Generic)

data NormalisedRsyslog a = NRsyslog
    { rsyslog          :: Rsyslog
    , normalised       :: a
    } deriving (Generic)
