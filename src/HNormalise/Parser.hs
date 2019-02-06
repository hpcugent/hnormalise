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

{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative         ((<|>))
import           Data.Aeson                  (encode)
import           Data.Attoparsec.Text
import           Data.Attoparsec.Time
import qualified Data.ByteString.Char8       as SBS
import qualified Data.ByteString.Lazy.Char8  as BS
import           Data.Char
import           Data.Text                   (Text, empty)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
--------------------------------------------------------------------------------
import           HNormalise.Common.Parser
import           HNormalise.Huppel.Parser
import           HNormalise.Internal
import           HNormalise.Json
import           HNormalise.Lmod.Parser
import           HNormalise.Shorewall.Parser
import           HNormalise.Snoopy.Parser
import           HNormalise.Torque.Parser
--------------------------------------------------------------------------------
rsyslogLogstashTemplate = "<%PRI%>1 %timegenerated:::date-rfc3339% %HOSTNAME% %syslogtag% - %APP-NAME%: %msg:::drop-last-lf%\n"

--------------------------------------------------------------------------------
-- | The 'parseMessage' function will try and use each configured parser to normalise the input it's given
parseMessage :: Parser (Text, ParseResult)
parseMessage =
    let pm parser target = (parser >>= (\(a, v) -> return (a, target v)))
    in     pm parseLmodLoad PRLmod
       <|> pm parseLmodCommand PRLmod
       <|> pm parseShorewall PRShorewall
       <|> pm parseSnoopy PRSnoopy
       <|> pm parseTorqueQueue PRTorque
       <|> pm parseTorqueStart PRTorque
       <|> pm parseTorqueDelete PRTorque
       <|> pm parseTorqueExit PRTorque
       <|> pm parseTorqueAbort PRTorque
       <|> pm parseTorqueRerun PRTorque

--------------------------------------------------------------------------------
-- | The 'getJsonKey' function return the key under which the normalised message should appear when JSON is produced
getJsonKey :: ParseResult  -- ^ Wrapped result for which we need to get a key
           -> Text         -- ^ Key for use in the JSON encoding of the result
--getJsonKey (PR_Huppel _)    = "huppel"
getJsonKey (PRLmod _)      = "lmod"
getJsonKey (PRTorque _)    = "torque"
getJsonKey (PRShorewall _) = "shorewall"
getJsonKey (PRSnoopy _)    = "snoopy"

--------------------------------------------------------------------------------
-- | The 'parseRsyslogLogstashString' currently is a placeholder function that will convert the incoming rsyslog message
-- if it is encoded as expected in a plain string format
-- <%PRI%>1 %timegenerated:::date-rfc3339% %HOSTNAME% %syslogtag% - %APP-NAME%: %msg%
parseRsyslogLogstashString :: Maybe [(Text, Text)]     -- ^ Output fields
                           -> Parser NormalisedRsyslog -- SBS.ByteString   -- ^ Resulting encoded JSON representation
parseRsyslogLogstashString fs = do
    abspri <- maybeOption $ do
        char '<'
        p <- decimal
        char '>'
        v <- maybeOption decimal
        return (p, v)
    timereported <- skipSpace *> zonedTime
    hostname <- skipSpace *> takeTill isSpace
    syslogtag <- skipSpace *> takeTill isSpace  -- FIXME: this might be incorrect
    skipSpace *> char '-' *> skipSpace
    (original, (appname, parsed)) <- match parseMessage
    return $ let jsonkey = getJsonKey parsed
             in NRsyslog
                    { rsyslog = Rsyslog
                        { msg              = original
                        , timereported     = timereported
                        , hostname         = hostname
                        , syslogtag        = syslogtag
                        , inputname        = T.empty
                        , fromhost         = T.empty
                        , fromhost_ip      = T.empty
                        , pri              = abspri >>= \(p, _) -> return p
                        , version          = abspri >>= snd
                        , syslogfacility   = T.empty
                        , syslogseverity   = T.empty
                        , timegenerated    = Nothing
                        , programname      = T.empty
                        , protocol_version = T.empty
                        , app_name         = appname
                        , procid           = T.empty
                        }
                    , normalised = parsed
                    , jsonkey = jsonkey
                    , fields = fs
                    }
