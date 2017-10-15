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
import           Data.Attoparsec.Combinator (lookAhead, manyTill)
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text, empty)
import           Data.Text.Encoding         (encodeUtf8, decodeUtf8)
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
normaliseJsonInput :: Maybe [(Text, Text)]    -- ^ Output fields
                   -> SBS.ByteString          -- ^ Input representing an rsyslog message in JSON format
                   -> Normalised              -- ^ Transformed or Original result
normaliseJsonInput fs logLine =
    case (Aeson.decodeStrict logLine :: Maybe Rsyslog) >>= normaliseRsyslog fs of
        Just j  -> Transformed j
        Nothing -> Original logLine

--------------------------------------------------------------------------------
-- | The 'normaliseText' function converts a 'Text' to a normalised message or keeps the original (in 'ByteString')
-- format if the conversion fails
normaliseText :: Maybe [(Text, Text)]                        -- ^ Output fields
              -> SBS.ByteString                              -- ^ Input
              -> Either SBS.ByteString NormalisedRsyslog     -- ^ Transformed or Original result
normaliseText fs logLine =
    let !p = parse (parseRsyslogLogstashString fs) $ decodeUtf8 logLine
    in case p of
        Done _ r    -> Right r
        Partial c   -> case c empty of
                            Done _ r -> Right r
                            _        -> llogline
        _           -> llogline
  where
    llogline = Left logLine


--------------------------------------------------------------------------------
-- | The 'convertMessage' function transforms the actual message to a 'Maybe' 'ParseResult'. If parsing fails,
-- the result is 'Nothing'.
convertMessage :: Text               -- ^ The actual message part of the incoming JSON payload
               -> Maybe ParseResult  -- ^ The resulting Haskell data structure wrapped in a ParseResult
convertMessage message =
    case parse parseMessage message of
        Done _ (_, pm) -> Just pm
        Partial c -> case c empty of
            Done _ (_, pm) -> Just pm
            _ -> Nothing
        _         -> Nothing

--------------------------------------------------------------------------------
-- | The 'normaliseRsyslog' function returns an 'NRSyslog' structure tranformed to a 'ByteString' or 'Nothing'
-- when parsing fails
normaliseRsyslog :: Maybe [(Text, Text)]   -- ^ Output fields
                 -> Rsyslog                -- ^ Incoming rsyslog information
                 -> Maybe SBS.ByteString   -- ^ If the conversion succeeded the JSON encoded rsyslog message to forward
normaliseRsyslog fs rsyslog = do
    cm <- convertMessage $ msg rsyslog
    return $ BS.toStrict
           $ Aeson.encode NRsyslog { rsyslog = rsyslog, normalised = cm, jsonkey = getJsonKey cm, fields = fs }
