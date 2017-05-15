module Lib
    ( convertMessage
    , Rsyslog.Internal.Rsyslog(..)
    ) where

--------------------------------------------------------------------------------
import Control.Applicative ( (<|>) )
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator ( lookAhead, manyTill )
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text, empty)

import Debug.Trace

--------------------------------------------------------------------------------
import Huppel.Internal
import Huppel.Json
import Huppel.Parser
import Lmod.Internal
import Lmod.Json
import Lmod.Parser
import Rsyslog.Internal
import Torque.Internal
import Torque.Json
import Torque.Parser

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


convertMessage :: Rsyslog -> Maybe SBS.ByteString
convertMessage l =
    case parse parseMessage (msg l) of
        Done _ pm -> trace (show pm) $ Just $ BS.toStrict $ Aeson.encode pm
        Partial c -> trace ("partial result") $ case c empty of
            Done _ pm -> trace (show pm) $ Just $ BS.toStrict $ Aeson.encode pm
            _ -> trace ("no result after partial continuation") $ Nothing
        _         -> trace ("no result") $ Nothing
