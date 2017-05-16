{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module HNormalise.Config
    ( Config(..)
    , loadConfig
    ) where

--------------------------------------------------------------------------------
import           Control.Monad          (mplus)
import           Data.Aeson             (defaultOptions)
import           Data.Aeson.TH          (deriveJSON)
import qualified Data.ByteString        as B
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Yaml              as Y
import           System.Directory


--------------------------------------------------------------------------------
data Config = Config
    { listenPort    :: !(Maybe Int)    -- ^ port for incoming messages
    , listenHost    :: !(Maybe Text)   -- ^ binding to this host specification (TODO: needs support for HostPreference)
    , successPort   :: !(Maybe Int)    -- ^ port to send rsyslog with successfully parsed and normalised msg part
    , successHost   :: !(Maybe Text)   -- ^ host to send normalised data to
    , failPort      :: !(Maybe Int)    -- ^ port to send rsyslog messges that failed to parse
    , failHost      :: !(Maybe Text)   -- ^ host to send original data to when parsing failed
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid Config where
    mempty = Config
                Nothing Nothing Nothing Nothing Nothing Nothing
    mappend l r = Config
        { listenPort  = listenPort  l `mplus` listenPort  r
        , listenHost  = listenHost  l `mplus` listenHost  r
        , successPort = successPort l `mplus` successPort r
        , successHost = successHost l `mplus` successHost r
        , failPort    = failPort    l `mplus` failPort    r
        , failHost    = failHost    l `mplus` failHost    r
        }

--------------------------------------------------------------------------------
defaultConfig = Config
    { listenPort = Just 4019
    , listenHost = Just "localhost"
    , successPort = Just 26002
    , successHost = Just "localhost"
    , failPort = Just 4018
    , failHost = Just "localhost"
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
            Left err -> error $ "HNormalise.Config.readConfig: " ++ err
            Right config -> return (config :: Config)
    else
        return mempty


--------------------------------------------------------------------------------
loadConfig :: Maybe FilePath -> IO Config
loadConfig fp = do
    userConfig <- case fp of
        Just fp' -> readConfig fp'
        Nothing -> return mempty
    systemConfig <- readConfig systemConfigFileLocation
    return $ userConfig <> systemConfig <> defaultConfig

--------------------------------------------------------------------------------
$(deriveJSON defaultOptions ''Config)
