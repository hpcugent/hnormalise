{-# LANGUAGE OverloadedStrings #-}


module HNormalise
    ( normaliseRsyslog
    , normaliseJsonInput
    , normaliseText
    , parseMessage
    , Normalised (..)
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative        ((<|>))
import           Data.Aeson                 (ToJSON)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Text            (encodeToLazyText)
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text, empty)
import           Data.Text.Lazy             (toStrict)

import           Debug.Trace

--------------------------------------------------------------------------------
import           HNormalise.Huppel.Internal
import           HNormalise.Huppel.Json
import           HNormalise.Internal
import           HNormalise.Json
import           HNormalise.Lmod.Internal
import           HNormalise.Lmod.Json
import           HNormalise.Parser
import           HNormalise.Torque.Internal
import           HNormalise.Torque.Json

--------------------------------------------------------------------------------
data Normalised = Transformed !SBS.ByteString
                | Original !SBS.ByteString

--------------------------------------------------------------------------------
normaliseJsonInput :: SBS.ByteString    -- ^ Input
                   -> Normalised        -- ^ Transformed or Original result
normaliseJsonInput logLine =
    case (Aeson.decodeStrict logLine :: Maybe Rsyslog) >>= normaliseRsyslog of
        Just j  -> Transformed j
        Nothing -> Original logLine

--------------------------------------------------------------------------------
normaliseText :: Text          -- ^ Input
              -> Normalised      -- ^ Transformed or Original result
normaliseText logLine =
    case parse parseRsyslogLogstashString logLine of
        Done _ r    -> Transformed r
        Partial c   -> case c empty of
                            Done _ r -> Transformed r
                            _        -> original
        _           -> original
  where
    original = Original $ BS.toStrict $ Aeson.encode $ logLine


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
normaliseRsyslog :: Rsyslog               -- ^ Incoming rsyslog information
                 -> Maybe SBS.ByteString  -- ^ IF the conversion succeeded the JSON encoded rsyslog message to forward
normaliseRsyslog rsyslog = do
    cm <- convertMessage $ msg rsyslog
    return $ BS.toStrict
           $ Aeson.encode
           $ NRsyslog { rsyslog = rsyslog, normalised = cm, jsonkey = getJsonKey cm }
