{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE OverloadedStrings          #-}

module HNormalise.Internal where

--------------------------------------------------------------------------------
import Data.Aeson             (FromJSON, ToJSON, toJSON, toEncoding)
import Data.Text
import GHC.Generics           (Generic)

--------------------------------------------------------------------------------
import HNormalise.Huppel.Internal (Huppel)
import HNormalise.Huppel.Json
import HNormalise.Lmod.Internal   (LmodLoad)
import HNormalise.Lmod.Json
import HNormalise.Torque.Internal (TorqueJobExit)
import HNormalise.Torque.Json

--------------------------------------------------------------------------------
{-data GetJsonKey a = GetJsonKey
    { f :: (a -> Text)
    }

-- We really do not use this, but something should satify the ToJSON constraint of the NRSyslog type as well as
-- make sure we can avoid circular imports to get the JSON key for storing the parsed data.
instance (ToJSON a) => ToJSON (GetJsonKey a) where
    toJSON (GetJsonKey _) = "{}"
-}
--------------------------------------------------------------------------------
data ParseResult = PR_H Huppel
                 | PR_L LmodLoad
                 | PR_T TorqueJobExit
                 deriving  (Show, Eq, Generic)

--------------------------------------------------------------------------------
instance ToJSON ParseResult where
    toEncoding (PR_H v) = toEncoding v
    toEncoding (PR_L v) = toEncoding v
    toEncoding (PR_T v) = toEncoding v

--------------------------------------------------------------------------------
data Rsyslog = Rsyslog
    { msg              :: !Text
    --, rawmsg           :: !Text
    , timereported     :: !Text
    , hostname         :: !Text
    , syslogtag        :: !Text  -- Could be a list?
    , inputname        :: !Text
    , fromhost         :: !Text
    , fromhost_ip      :: !Text
    , pri              :: !Text
    , syslogfacility   :: !Text
    , syslogseverity   :: !Text
    , timegenerated    :: !Text
    , programname      :: !Text
    , protocol_version :: !Text
    --, structured_data  :: !Text
    , app_name         :: !Text
    , procid           :: !Text
    --, msgid            :: !Text
    --, uuid             :: !(Maybe Text)
    --, all_json         :: !(Maybe Text)
    } deriving (Eq, Show, Generic)


--------------------------------------------------------------------------------
data NormalisedRsyslog = NRsyslog
    { rsyslog          :: Rsyslog
    , normalised       :: ParseResult
    , jsonkey          :: Text
    } deriving (Eq, Show, Generic)
