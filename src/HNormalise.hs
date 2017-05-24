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
data Normalised
    -- | A `Transformed` message contains the JSON normalised representation as a `ByteString`
    = Transformed !SBS.ByteString
    -- | An 'Original' messge contains the unaltered incoming message as a 'ByteString'
    | Original !SBS.ByteString

--------------------------------------------------------------------------------
-- | The 'normaliseJsonInput' function converts a `ByteString` to a normalised message or keeps the original if
-- the conversion (parsing) fails.
normaliseJsonInput :: SBS.ByteString    -- ^ Input representing an rsyslog message in JSON format
                   -> Normalised        -- ^ Transformed or Original result
normaliseJsonInput logLine =
    case (Aeson.decodeStrict logLine :: Maybe Rsyslog) >>= normaliseRsyslog of
        Just j  -> Transformed j
        Nothing -> Original logLine

--------------------------------------------------------------------------------
-- | The 'normaliseText' function converts a 'Text' to a normalised message or keeps the original (in 'ByteString')
-- format if the conversion fails
normaliseText :: Text          -- ^ Input
              -> Normalised    -- ^ Transformed or Original result
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
-- | The 'convertMessage' function transforms the actual message to a 'Maybe' 'ParseResult'. If parsing fails,
-- the result is 'Nothing'.
convertMessage :: Text -> Maybe ParseResult
convertMessage message =
    case parse parseMessage message of
        Done _ pm -> Just pm
        Partial c -> case c empty of
            Done _ pm -> Just pm
            _ -> Nothing
        _         -> Nothing

--------------------------------------------------------------------------------
-- | The 'normaliseRsyslog' function returns an 'NRSyslog' structure tranformed to a 'ByteString' or 'Nothing'
-- when parsing fails
normaliseRsyslog :: Rsyslog               -- ^ Incoming rsyslog information
                 -> Maybe SBS.ByteString  -- ^ IF the conversion succeeded the JSON encoded rsyslog message to forward
normaliseRsyslog rsyslog = do
    cm <- convertMessage $ msg rsyslog
    return $ BS.toStrict
           $ Aeson.encode
           $ NRsyslog { rsyslog = rsyslog, normalised = cm, jsonkey = getJsonKey cm }
