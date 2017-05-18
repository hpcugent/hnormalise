{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE OverloadedStrings          #-}

module HNormalise.Rsyslog.Parser where


--------------------------------------------------------------------------------
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
--------------------------------------------------------------------------------
import           HNormalise.Rsyslog.Internal

--------------------------------------------------------------------------------
rsyslogLogstashTemplate = "<%PRI%>1 %timegenerated:::date-rfc3339% %HOSTNAME% %syslogtag% - %APP-NAME%: %msg:::drop-last-lf%\n"

--------------------------------------------------------------------------------
parseRsyslogLogstashString :: Parser Rsyslog
parseRsyslogLogstashString = do
    pri <- char '<' *> takeTill (== '>')
    char '>' *> decimal
    timegenerated <- skipSpace *> takeTill isSpace
    hostname <- skipSpace *> takeTill isSpace
    syslogtag <- skipSpace *> takeTill isSpace  -- FIXME: this might be incorrect
    skipSpace *> char '-'
    appname <- skipSpace *> takeTill (== ':')
    msg <- char ':' *> skipSpace *> takeByteString
    return Rsyslog
        { msg              = TE.decodeUtf8 msg
        , timereported     = T.empty
        , hostname         = TE.decodeUtf8 hostname
        , syslogtag        = TE.decodeUtf8 syslogtag
        , inputname        = T.empty
        , fromhost         = T.empty
        , fromhost_ip      = T.empty
        , pri              = TE.decodeUtf8 pri
        , syslogfacility   = T.empty
        , syslogseverity   = T.empty
        , timegenerated    = TE.decodeUtf8 timegenerated
        , programname      = T.empty
        , protocol_version = T.empty
        , app_name         = TE.decodeUtf8 appname
        , procid           = T.empty
    }
