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
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module HNormalise.Config
    ( Config(..)
    , ConnectionType(..)
    , InputConfig(..)
    , OutputConfig(..)
    , TcpOutputConfig(..)
    , TcpPortConfig(..)
    , ZeroMQOutputConfig(..)
    , ZeroMQPortConfig(..)
    , connectionType
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
data ConnectionType = TCP
                    | ZeroMQ
                    deriving (Eq, Ord, Show)

connectionType :: Config -> ConnectionType
connectionType c =
    case input c >>= \(InputConfig t z) -> t of
        Just _ -> TCP
        _      -> ZeroMQ

--------------------------------------------------------------------------------
data TcpPortConfig = TcpPortConfig
    { host   :: !(Maybe Text)
    , port   :: !(Maybe Int)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid TcpPortConfig where
    mempty = TcpPortConfig Nothing Nothing
    mappend (TcpPortConfig hl pl) (TcpPortConfig hr pr) = TcpPortConfig
        { host = hl `mplus` hr
        , port = pl `mplus` pr
        }

--------------------------------------------------------------------------------
data TcpOutputConfig = TcpOutputConfig
    { success :: !(Maybe TcpPortConfig)
    , failure :: !(Maybe TcpPortConfig)
    } deriving Show

--------------------------------------------------------------------------------
instance Monoid TcpOutputConfig where
    mempty = TcpOutputConfig Nothing Nothing
    mappend (TcpOutputConfig sl fl) (TcpOutputConfig sr fr) = TcpOutputConfig
        { success = sl `mplus` sr
        , failure = fl `mplus` fr
        }

--------------------------------------------------------------------------------
data ZeroMQPortConfig = ZeroMQPortConfig
    { method :: !(Maybe Text)
    , host   :: !(Maybe Text)
    , port   :: !(Maybe Int)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid ZeroMQPortConfig where
    mempty = ZeroMQPortConfig Nothing Nothing Nothing
    mappend (ZeroMQPortConfig ml hl pl) (ZeroMQPortConfig mr hr pr) = ZeroMQPortConfig
        { method = ml `mplus` mr
        , host   = hl `mplus` hr
        , port   = pl `mplus` pr
        }

--------------------------------------------------------------------------------
data ZeroMQOutputConfig = ZeroMQOutputConfig
    { success  :: !(Maybe ZeroMQPortConfig)
    , failure  :: !(Maybe ZeroMQPortConfig)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid ZeroMQOutputConfig where
    mempty = ZeroMQOutputConfig Nothing Nothing
    mappend (ZeroMQOutputConfig sl fl) (ZeroMQOutputConfig sr fr) = ZeroMQOutputConfig
        { success = sl `mplus` sr
        , failure = fl `mplus` fr
        }

--------------------------------------------------------------------------------
data InputConfig = InputConfig
    { tcp     :: !(Maybe TcpPortConfig)
    , zeromq  :: !(Maybe ZeroMQPortConfig)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid InputConfig where
    mempty = InputConfig Nothing Nothing
    mappend (InputConfig tl zl) (InputConfig tr zr) = InputConfig
        { tcp    = tl `mplus` tr
        , zeromq = zl `mplus` zr
        }

--------------------------------------------------------------------------------
data OutputConfig = OutputConfig
    { tcp     :: !(Maybe TcpOutputConfig)
    , zeromq  :: !(Maybe ZeroMQOutputConfig)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid OutputConfig where
    mempty = OutputConfig Nothing Nothing
    mappend (OutputConfig tl zl) (OutputConfig tr zr) = OutputConfig
        { tcp    = tl `mplus` tr
        , zeromq = zl `mplus` zr
        }


--------------------------------------------------------------------------------
defaultInputTcpConfig = TcpPortConfig
    { port = Just 4019
    , host = Just "localhost"
    }

defaultOutputTcpConfig = TcpOutputConfig
    { success = Just $ TcpPortConfig { host = Just "localhost", port = Just 26001 }
    , failure = Just $ TcpPortConfig { host = Just "localhost", port = Just 26002 }
    }

defaultInputConfig = InputConfig
    { tcp = Just defaultInputTcpConfig
    , zeromq = Nothing
    }

defaultOutputConfig = OutputConfig
    { tcp = Just defaultOutputTcpConfig
    , zeromq = Nothing
    }

--------------------------------------------------------------------------------
data Config = Config
    { input  :: !(Maybe InputConfig)
    , output :: !(Maybe OutputConfig)
    , fields :: !(Maybe [(Text, Text)])
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid Config where
    mempty = Config Nothing Nothing Nothing
    mappend l r = Config
        { input  = input l  `mplus` input r
        , output = output l `mplus` output r
        , fields = fields l `mplus` fields r
        }

defaultConfig = Config
    { input = Just defaultInputConfig
    , output = Just defaultOutputConfig
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
$(deriveJSON defaultOptions ''TcpPortConfig)
$(deriveJSON defaultOptions ''TcpOutputConfig)
$(deriveJSON defaultOptions ''ZeroMQPortConfig)
$(deriveJSON defaultOptions ''ZeroMQOutputConfig)
$(deriveJSON defaultOptions ''InputConfig)
$(deriveJSON defaultOptions ''OutputConfig)
$(deriveJSON defaultOptions ''Config)
