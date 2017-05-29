{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Parser where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Aeson                 (encode)
import           Data.Attoparsec.Text
import           Data.Attoparsec.Time
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Text                  (Text, empty)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
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
        (parseLmodLoad   >>= (\(a, v) -> return $ (a, PR_Lmod v)))
    <|> (parseShorewall  >>= (\(a, v) -> return $ (a, PR_Shorewall v)))
    <|> (parseSnoopy     >>= (\(a, v) -> return $ (a, PR_Snoopy v)))
    <|> (parseTorqueExit >>= (\(a, v) -> return $ (a, PR_Torque v)))

--------------------------------------------------------------------------------
-- | The 'getJsonKey' function return the key under which the normalised message should appear when JSON is produced
getJsonKey :: ParseResult -> Text
getJsonKey (PR_Huppel _) = "huppel"
getJsonKey (PR_Lmod _) = "lmod"
getJsonKey (PR_Torque _) = "torque"
getJsonKey (PR_Shorewall _) = "shorewall"
getJsonKey (PR_Snoopy _) = "snoopy"

--------------------------------------------------------------------------------
-- | The 'parseRsyslogLogstashString' currently is a placeholder function that will convert the incoming rsyslog message
-- if it is encoded as expected in a plain string format
-- <%PRI%>1 %timegenerated:::date-rfc3339% %HOSTNAME% %syslogtag% - %APP-NAME%: %msg%
parseRsyslogLogstashString :: Parser SBS.ByteString
parseRsyslogLogstashString = do
    abspri <- maybeOption $ do
        char '<'
        p <- decimal
        char '>'
        v <- maybeOption $ decimal
        return (p, v)
    timegenerated <- skipSpace *> zonedTime
    hostname <- skipSpace *> takeTill isSpace
    syslogtag <- skipSpace *> takeTill isSpace  -- FIXME: this might be incorrect
    skipSpace *> char '-' *> skipSpace
    (original, (appname, parsed)) <- match parseMessage
    return $ let jsonkey = getJsonKey parsed
             in BS.toStrict $ encode $ NRsyslog
                    { rsyslog = Rsyslog
                        { msg              = original
                        , timereported     = Nothing
                        , hostname         = hostname
                        , syslogtag        = syslogtag
                        , inputname        = T.empty
                        , fromhost         = T.empty
                        , fromhost_ip      = T.empty
                        , pri              = case abspri of
                                Just (p, _) -> Just p
                                Nothing     -> Nothing
                        , syslogfacility   = T.empty
                        , syslogseverity   = T.empty
                        , timegenerated    = timegenerated
                        , programname      = T.empty
                        , protocol_version = T.empty
                        , app_name         = appname
                        , procid           = T.empty
                        }
                    , normalised = parsed
                    , jsonkey = jsonkey
                    }
