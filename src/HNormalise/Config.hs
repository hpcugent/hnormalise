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


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module HNormalise.Config
    ( Config(..)
    , PortConfig(..)
    , loadConfig
    ) where

--------------------------------------------------------------------------------
import           Control.Monad    (mplus)
import           Data.Aeson       (defaultOptions)
import           Data.Aeson.TH    (deriveJSON)
import qualified Data.ByteString  as B
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import qualified Data.Yaml        as Y
import           System.Directory


--------------------------------------------------------------------------------
data PortConfig = PortConfig
    { listenPort  :: !(Maybe Int)    -- ^ port for incoming messages
    , listenHost  :: !(Maybe Text)   -- ^ binding to this host specification (TODO: needs support for HostPreference)
    , successPort :: !(Maybe Int)    -- ^ port to send rsyslog with successfully parsed and normalised msg part
    , successHost :: !(Maybe Text)   -- ^ host to send normalised data to
    , failPort    :: !(Maybe Int)    -- ^ port to send rsyslog messges that failed to parse
    , failHost    :: !(Maybe Text)   -- ^ host to send original data to when parsing failed
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid PortConfig where
    mempty = PortConfig
                Nothing Nothing Nothing Nothing Nothing Nothing
    mappend l r = PortConfig
        { listenPort  = listenPort  l `mplus` listenPort  r
        , listenHost  = listenHost  l `mplus` listenHost  r
        , successPort = successPort l `mplus` successPort r
        , successHost = successHost l `mplus` successHost r
        , failPort    = failPort    l `mplus` failPort    r
        , failHost    = failHost    l `mplus` failHost    r
        }

--------------------------------------------------------------------------------
defaultPortConfig = PortConfig
    { listenPort = Just 4019
    , listenHost = Just "localhost"
    , successPort = Just 26002
    , successHost = Just "localhost"
    , failPort = Just 4018
    , failHost = Just "localhost"
    }

--------------------------------------------------------------------------------
data Config = Config
    { ports :: !(Maybe PortConfig)
    , fields :: !(Maybe [(Text, Text)])
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid Config where
    mempty = Config Nothing Nothing
    mappend l r = Config
        { ports = ports l `mplus` ports r
        , fields = fields l `mplus` fields r
        }

defaultConfig = Config
    { ports = Just defaultPortConfig
    , fields = Nothing
    }

--------------------------------------------------------------------------------
systemConfigFileLocation :: FilePath
systemConfigFileLocation = "/etc/hnormalise.yaml"

--------------------------------------------------------------------------------
readConfig :: FilePath -> IO Config
readConfig fp = do
    exists <- doesFileExist fp
    if exists then do
        contents <- B.readFile fp
        case Y.decodeEither contents of
            Left err     -> error $ "HNormalise.Config.readConfig: " ++ err
            Right config -> return (config :: Config)
    else
        return mempty


--------------------------------------------------------------------------------
loadConfig :: Maybe FilePath -> IO Config
loadConfig fp = do
    userConfig <- case fp of
        Just fp' -> readConfig fp'
        Nothing  -> return mempty
    systemConfig <- readConfig systemConfigFileLocation
    return $ userConfig <> systemConfig <> defaultConfig

--------------------------------------------------------------------------------
$(deriveJSON defaultOptions ''PortConfig)
$(deriveJSON defaultOptions ''Config)
