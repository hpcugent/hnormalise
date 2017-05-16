module HNormalise
    ( normaliseRsyslog
    , HNormalise.Rsyslog.Internal.Rsyslog(..)
    ) where

--------------------------------------------------------------------------------
import Control.Applicative ( (<|>) )
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator ( lookAhead, manyTill )
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text, empty)
import Data.Text.Lazy (toStrict)

import Debug.Trace

--------------------------------------------------------------------------------
import HNormalise.Huppel.Internal
import HNormalise.Huppel.Json
import HNormalise.Huppel.Parser
import HNormalise.Lmod.Internal
import HNormalise.Lmod.Json
import HNormalise.Lmod.Parser
import HNormalise.Rsyslog.Internal
import HNormalise.Rsyslog.Json
import HNormalise.Torque.Internal
import HNormalise.Torque.Json
import HNormalise.Torque.Parser

--------------------------------------------------------------------------------

data ParseResult = PR_H Huppel
                 | PR_L LmodLoad
                 | PR_T TorqueJobExit
                 deriving  (Show, Eq)

instance ToJSON ParseResult where
    toJSON (PR_H v) = Aeson.toJSON v
    toJSON (PR_L v) = Aeson.toJSON v
    toJSON (PR_T v) = Aeson.toJSON v


parseMessage =
        (parseHuppel >>= (\v -> return $ PR_H v))
    <|> (parseLmodLoad >>= (\v -> return $ PR_L v))
    <|> (parseTorqueExit >>= (\v -> return $ PR_T v))


convertMessage :: Text -> Maybe ParseResult
convertMessage message =
    case parse parseMessage message of
        Done _ pm -> trace (show pm) $ Just pm
        Partial c -> trace ("partial result") $ case c empty of
            Done _ pm -> trace (show pm) $ Just pm
            _ -> trace ("no result after partial continuation") $ Nothing
        _         -> trace ("no result") $ Nothing

normaliseRsyslog :: Rsyslog               -- ^ Incoming rsyslog information
                 -> Maybe SBS.ByteString  -- ^ IF the conversion succeeded the JSON encoded rsyslog message to forward
normaliseRsyslog rsyslog = do
    cm <- convertMessage $ msg rsyslog
    return $ BS.toStrict $ Aeson.encode $ rsyslog { msg = toStrict $ encodeToLazyText $ cm }
