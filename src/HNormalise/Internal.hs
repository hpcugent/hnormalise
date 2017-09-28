{- hnormalise - a log normalisation library
 -
 - Copyright Andy Georges (c) 2017
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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

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
import           HNormalise.Torque.Internal    (TorqueParseResult)
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
    -- | Represents a parsed 'Torque' message
    | PR_Torque TorqueParseResult
    deriving  (Show, Eq, Generic)

--------------------------------------------------------------------------------
instance ToJSON ParseResult where
    toEncoding (PR_Huppel v)    = toEncoding v
    toEncoding (PR_Lmod v)      = toEncoding v
    toEncoding (PR_Shorewall v) = toEncoding v
    toEncoding (PR_Snoopy v)    = toEncoding v
    toEncoding (PR_Torque v)    = toEncoding v

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
    , fields     :: Maybe [(Text, Text)]     -- ^ The fields we need to output when creating the JSON encoding as (key, fieldname)
    } deriving (Show, Generic)
