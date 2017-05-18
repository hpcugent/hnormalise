{-# LANGUAGE OverloadedStrings          #-}


module HNormalise
    ( normaliseRsyslog
    , parseMessage
    , HNormalise.Internal.Rsyslog(..)
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative         ((<|>))
import           Data.Aeson                  (ToJSON)
import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Text             (encodeToLazyText)
import           Data.Attoparsec.Combinator  (lookAhead, manyTill)
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8       as SBS
import qualified Data.ByteString.Lazy.Char8  as BS
import           Data.Text                   (Text, empty)
import           Data.Text.Lazy              (toStrict)

import           Debug.Trace

--------------------------------------------------------------------------------
import           HNormalise.Internal
import           HNormalise.Json
import           HNormalise.Parser
import           HNormalise.Huppel.Internal
import           HNormalise.Huppel.Json
import           HNormalise.Lmod.Internal
import           HNormalise.Lmod.Json
import           HNormalise.Torque.Internal
import           HNormalise.Torque.Json

--------------------------------------------------------------------------------
getJsonKey :: ParseResult -> Text
getJsonKey (PR_H _) = "huppel"
getJsonKey (PR_L _) = "lmod"
getJsonKey (PR_T _) = "torque"


--------------------------------------------------------------------------------
convertMessage :: Text -> Maybe ParseResult
convertMessage message =
    case parse parseMessage message of
        Done _ pm -> {-trace (show pm) $-} Just pm
        Partial c -> {-trace ("partial result") $ -}case c empty of
            Done _ pm -> {-trace (show pm) $-} Just pm
            _ -> {-trace ("no result after partial continuation") $-} Nothing
        _         -> {-trace ("no result") $-} Nothing

--------------------------------------------------------------------------------
normaliseRsyslog rsyslog = undefined
{-normaliseRsyslog :: Rsyslog               -- ^ Incoming rsyslog information
                 -> Maybe SBS.ByteString  -- ^ IF the conversion succeeded the JSON encoded rsyslog message to forward
normaliseRsyslog rsyslog = do
    cm <- convertMessage $ msg rsyslog
    return $ BS.toStrict
           $ Aeson.encode
           $ NRsyslog { rsyslog = rsyslog, normalised = cm, jsonkey = GetJsonKey getJsonKey }
       -}
