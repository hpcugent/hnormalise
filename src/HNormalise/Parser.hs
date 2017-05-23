{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module HNormalise.Parser where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Aeson                 (encode)
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Text                  (Text, empty)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
--------------------------------------------------------------------------------
import           HNormalise.Huppel.Parser
import           HNormalise.Internal
import           HNormalise.Json
import           HNormalise.Lmod.Parser
import           HNormalise.Shorewall.Parser
import           HNormalise.Torque.Parser
--------------------------------------------------------------------------------
rsyslogLogstashTemplate = "<%PRI%>1 %timegenerated:::date-rfc3339% %HOSTNAME% %syslogtag% - %APP-NAME%: %msg:::drop-last-lf%\n"

--------------------------------------------------------------------------------
parseMessage :: Parser ParseResult
parseMessage =
        (parseLmodLoad >>= (\v -> return $ PR_L v))
    <|> (parseShorewall >>= (\v -> return $ PR_S v))
    <|> (parseTorqueExit >>= (\v -> return $ PR_T v))

--------------------------------------------------------------------------------
getJsonKey :: ParseResult -> Text
getJsonKey (PR_H _) = "huppel"
getJsonKey (PR_L _) = "lmod"
getJsonKey (PR_T _) = "torque"
getJsonKey (PR_S _) = "shorewall"


--------------------------------------------------------------------------------
parseRsyslogLogstashString :: Parser SBS.ByteString
parseRsyslogLogstashString = do
    pri <- char '<' *> takeTill (== '>')
    char '>' *> decimal
    timegenerated <- skipSpace *> takeTill isSpace
    hostname <- skipSpace *> takeTill isSpace
    syslogtag <- skipSpace *> takeTill isSpace  -- FIXME: this might be incorrect
    skipSpace *> char '-'
    appname <- skipSpace *> takeTill (== ':')
    msg <- char ':' *> skipSpace *> parseMessage
    return $ let jsonkey = getJsonKey msg
             in BS.toStrict $ encode $ NRsyslog
                    { rsyslog = Rsyslog
                        { msg              = T.empty
                        , timereported     = T.empty
                        , hostname         = hostname
                        , syslogtag        = syslogtag
                        , inputname        = T.empty
                        , fromhost         = T.empty
                        , fromhost_ip      = T.empty
                        , pri              = pri
                        , syslogfacility   = T.empty
                        , syslogseverity   = T.empty
                        , timegenerated    = timegenerated
                        , programname      = T.empty
                        , protocol_version = T.empty
                        , app_name         = appname
                        , procid           = T.empty
                        }
                    , normalised = msg
                    , jsonkey = jsonkey
                    }
