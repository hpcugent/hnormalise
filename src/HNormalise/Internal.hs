{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Internal where

--------------------------------------------------------------------------------
import           Data.Aeson                    (FromJSON, ToJSON, toEncoding,
                                                toJSON)
import           Data.Text                     (Text)
import           Data.Time.LocalTime           (ZonedTime)
import           GHC.Generics                  (Generic)

--------------------------------------------------------------------------------
import           HNormalise.Huppel.Internal    (Huppel)
import           HNormalise.Huppel.Json
import           HNormalise.Lmod.Internal      (LmodLoad)
import           HNormalise.Lmod.Json
import           HNormalise.Shorewall.Internal (Shorewall)
import           HNormalise.Shorewall.Json
import           HNormalise.Snoopy.Internal    (Snoopy)
import           HNormalise.Snoopy.Json
import           HNormalise.Torque.Internal    (TorqueJobExit)
import           HNormalise.Torque.Json

--------------------------------------------------------------------------------
data ParseResult
    -- | 'Huppel' Result for testing purposes, should you want to check the pipeline works without pushing in actual data
    = PR_Huppel Huppel
    -- | Represents a parsed 'LmodLoad' message
    | PR_Lmod LmodLoad
    -- | Represents a parsed 'Shorewall' message
    | PR_Shorewall Shorewall
    -- | Represents a parsed 'Snoopy' message
    | PR_Snoopy Snoopy
    -- | Represents a parsed 'TorqueJobExit' message
    | PR_Torque TorqueJobExit
    deriving  (Show, Eq, Generic)

--------------------------------------------------------------------------------
instance ToJSON ParseResult where
    toEncoding (PR_Huppel v) = toEncoding v
    toEncoding (PR_Lmod v) = toEncoding v
    toEncoding (PR_Shorewall v) = toEncoding v
    toEncoding (PR_Snoopy v) = toEncoding v
    toEncoding (PR_Torque v) = toEncoding v

--------------------------------------------------------------------------------
data Rsyslog = Rsyslog
    { msg              :: !Text
    --, rawmsg           :: !Text
    , timereported     :: !ZonedTime
    , hostname         :: !Text
    , syslogtag        :: !Text  -- Could be a list?
    , inputname        :: !Text
    , fromhost         :: !Text
    , fromhost_ip      :: !Text
    , pri              :: !(Maybe Int)
    , version          :: !(Maybe Int)
    , syslogfacility   :: !Text
    , syslogseverity   :: !Text
    , timegenerated    :: !(Maybe ZonedTime)
    , programname      :: !Text
    , protocol_version :: !Text
    --, structured_data  :: !Text
    , app_name         :: !Text
    , procid           :: !Text
    --, msgid            :: !Text
    --, uuid             :: !(Maybe Text)
    --, all_json         :: !(Maybe Text)
    } deriving (Show, Generic)

--------------------------------------------------------------------------------
data NormalisedRsyslog = NRsyslog
    { rsyslog    :: Rsyslog            -- ^ The original rsyslog message in a parsed form
    , normalised :: ParseResult        -- ^ The normalised message
    , jsonkey    :: Text               -- ^ The key under which the normalised info will appear in the JSON result
    , fields     :: [(Text, Text)]     -- ^ The fields we need to output when creating the JSON encoding as (key, fieldname)
    } deriving (Show, Generic)
