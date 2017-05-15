{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

--------------------------------------------------------------------------------
import Control.Concurrent.Async ( concurrently, Concurrently(..), runConcurrently)
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Attoparsec.Text
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit.Combinators as C ( line, lineAscii, head, peek, foldMap, mapM_, fold, filterE, map, takeWhile )
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as SBS
import qualified Data.Text.Encoding as TE
import Prelude hiding (foldMap, mapM_, fold, head, map, takeWhile)
import Data.String (fromString, IsString)
import Data.Word8 (_cr)
--------------------------------------------------------------------------------

import Lib (convertMessage)
import Rsyslog.Json

--------------------------------------------------------------------------------

data Normalised = Transformed SBS.ByteString
                | Original SBS.ByteString

normalise :: SBS.ByteString  -- information arrives as a string representing JSON information
          -> Normalised
normalise logLine =
    case Data.Aeson.decodeStrict logLine >>= convertMessage of
        Just j  -> Transformed j
        Nothing -> Original logLine

messageSink success failure = loop
  where
    loop = do
        v <- await
        case v of
            Just (Transformed json) -> do
                yield (SBS.snoc json '\n') $$ appSink success
                loop
            Just (Original l) -> do
                yield (SBS.snoc l '\n') $$ appSink failure
                loop
            Nothing -> return ()

main :: IO ()
main = do
        runTCPServer (serverSettings 4000 "*") $ \appData ->
            --runTCPServer (serverSettings 4017 "*") $ \rsyslogES ->
            --runTCPServer (serverSettings 4018 "*") $ \rsyslogLT ->
            runTCPClient (clientSettings 4017 "localhost") $ \successServer ->
            runTCPClient (clientSettings 4018 "localhost") $ \failServer -> do
            appSource appData $= CB.lines $= C.map normalise $$ messageSink successServer failServer
