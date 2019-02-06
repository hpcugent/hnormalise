{- hnormalise - a log normalisation library
 -
 - Copyright Ghent University (c) 2017-2019
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
    , LoggingConfig(..)
    , OutputConfig(..)
    , TcpOutputConfig(..)
    , TcpPortConfig(..)
    , ZeroMQOutputConfig(..)
    , ZeroMQPortConfig(..)
    , connectionType
    , defaultLoggingFrequency
    , loadConfig
    ) where

--------------------------------------------------------------------------------
import           Control.Monad    (mplus)
import           Data.Aeson       (defaultOptions)
import           Data.Aeson.TH    (deriveJSON)
import qualified Data.ByteString  as B
import           Data.Maybe       (isJust)
import           Data.Semigroup   ((<>))
import           Data.Text        (Text)
import qualified Data.Yaml        as Y
import           System.Directory

import           Debug.Trace

--------------------------------------------------------------------------------
data ConnectionType = TCP
                    | ZeroMQ
                    deriving (Eq, Ord, Show)

-- | `connectionType` defaults to TCP
connectionType :: Config -> ConnectionType
connectionType c =
    case input c >>= \(InputConfig t z) -> t of
        Just _ -> TCP
        _      -> ZeroMQ

--------------------------------------------------------------------------------
defaultLoggingFrequency = 100000

data LoggingConfig = LoggingConfig
    { frequency :: !(Maybe Int)    -- ^ How often should we write an update (# of messages processed)
    } deriving (Show)

--------------------------------------------------------------------------------
data TcpPortConfig = TcpPortConfig
    { host :: !(Maybe Text)
    , port :: !(Maybe Int)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Semigroup TcpPortConfig where
    (<>) (TcpPortConfig hl pl) (TcpPortConfig hr pr) = TcpPortConfig
        { host = hl `mplus` hr
        , port = pl `mplus` pr
        }
instance Monoid TcpPortConfig where
    mempty = TcpPortConfig Nothing Nothing

--------------------------------------------------------------------------------
data TcpOutputConfig = TcpOutputConfig
    { success :: !(Maybe TcpPortConfig)
    , failure :: !(Maybe TcpPortConfig)
    } deriving Show

--------------------------------------------------------------------------------
instance Semigroup TcpOutputConfig where
    (<>) (TcpOutputConfig sl fl) (TcpOutputConfig sr fr) = TcpOutputConfig
        { success = sl `mplus` sr
        , failure = fl `mplus` fr
        }
instance Monoid TcpOutputConfig where
    mempty = TcpOutputConfig Nothing Nothing

--------------------------------------------------------------------------------
data ZeroMQPortConfig = ZeroMQPortConfig
    { method :: !(Maybe Text)
    , host   :: !(Maybe Text)
    , port   :: !(Maybe Int)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Semigroup ZeroMQPortConfig where
    (<>) (ZeroMQPortConfig ml hl pl) (ZeroMQPortConfig mr hr pr) = ZeroMQPortConfig
        { method = ml `mplus` mr
        , host   = hl `mplus` hr
        , port   = pl `mplus` pr
        }
instance Monoid ZeroMQPortConfig where
    mempty = ZeroMQPortConfig Nothing Nothing Nothing

--------------------------------------------------------------------------------
data ZeroMQOutputConfig = ZeroMQOutputConfig
    { success :: !(Maybe ZeroMQPortConfig)
    , failure :: !(Maybe ZeroMQPortConfig)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Semigroup ZeroMQOutputConfig where
    (<>) (ZeroMQOutputConfig sl fl) (ZeroMQOutputConfig sr fr) = ZeroMQOutputConfig
        { success = sl `mplus` sr
        , failure = fl `mplus` fr
        }
instance Monoid ZeroMQOutputConfig where
    mempty = ZeroMQOutputConfig Nothing Nothing

--------------------------------------------------------------------------------
data InputConfig = InputConfig
    { tcp    :: !(Maybe TcpPortConfig)
    , zeromq :: !(Maybe ZeroMQPortConfig)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Semigroup InputConfig where
    (<>) (InputConfig tl zl) (InputConfig tr zr) = InputConfig
        { tcp    = tl `mplus` tr
        , zeromq = zl `mplus` zr
        }
instance Monoid InputConfig where
    mempty = InputConfig Nothing Nothing

--------------------------------------------------------------------------------
data OutputConfig = OutputConfig
    { tcp    :: !(Maybe TcpOutputConfig)
    , zeromq :: !(Maybe ZeroMQOutputConfig)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Semigroup OutputConfig where
    (<>) (OutputConfig tl zl) (OutputConfig tr zr) = OutputConfig
        { tcp    = tl `mplus` tr
        , zeromq = zl `mplus` zr
        }
instance Monoid OutputConfig where
    mempty = OutputConfig Nothing Nothing


--------------------------------------------------------------------------------
defaultInputTcpConfig = TcpPortConfig
    { port = Just 4019
    , host = Just "localhost"
    }

defaultOutputTcpConfig = TcpOutputConfig
    { success = Just TcpPortConfig { host = Just "localhost", port = Just 26001 }
    , failure = Just TcpPortConfig { host = Just "localhost", port = Just 26002 }
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
    { logging :: !(Maybe LoggingConfig)
    , input   :: !(Maybe InputConfig)
    , output  :: !(Maybe OutputConfig)
    , fields  :: !(Maybe [(Text, Text)])
    } deriving (Show)

--------------------------------------------------------------------------------
instance Semigroup Config where
    (<>) l r = Config
        { logging = logging l `mplus` logging r
        , input  = input l  `mplus` input r
        , output = output l `mplus` output r
        , fields = fields l `mplus` fields r
        }
instance Monoid Config where
    mempty = Config Nothing Nothing Nothing Nothing

defaultConfig = Config
    { logging = Just LoggingConfig { frequency = Just defaultLoggingFrequency }
    , input = Nothing
    , output = Nothing
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
        trace "file read" $ return ()
        case Y.decodeEither contents of
            Left err     -> error $ "HNormalise.Config.readConfig: " ++ err
            Right config -> trace ("got " ++ show config) $ return (config :: Config)
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
$(deriveJSON defaultOptions ''LoggingConfig)
$(deriveJSON defaultOptions ''TcpPortConfig)
$(deriveJSON defaultOptions ''TcpOutputConfig)
$(deriveJSON defaultOptions ''ZeroMQPortConfig)
$(deriveJSON defaultOptions ''ZeroMQOutputConfig)
$(deriveJSON defaultOptions ''InputConfig)
$(deriveJSON defaultOptions ''OutputConfig)
$(deriveJSON defaultOptions ''Config)
