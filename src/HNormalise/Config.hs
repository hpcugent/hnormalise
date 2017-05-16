{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module HNormalise.Config
    (loadConfig
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
    { listenPort :: !(Maybe Int)
    , successPort :: !(Maybe Int)
    , successHost :: !(Maybe Text)
    , failPort :: !(Maybe Int)
    , failHost :: !(Maybe Text)
    } deriving (Show)

--------------------------------------------------------------------------------
instance Monoid Config where
    mempty = Config
                Nothing Nothing Nothing Nothing Nothing
    mappend l r = Config
        { listenPort  = listenPort  l `mplus` listenPort  r
        , successPort = successPort l `mplus` successPort r
        , successHost = successHost l `mplus` successHost r
        , failPort    = failPort    l `mplus` failPort    r
        , failHost    = failHost    l `mplus` failHost    r
        }

--------------------------------------------------------------------------------
defaultConfig = Config
    { listenPort = Just 4019
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
