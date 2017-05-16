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
import           Data.Text              (Text)
import qualified Data.Yaml.Config       as YC

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
defaultConfigFileLocation :: FilePath
defaultConfigFileLocation = "/etc/hnormalise.yml"


--------------------------------------------------------------------------------
loadConfig :: Maybe FilePath -> IO Config
loadConfig fp = undefined

--------------------------------------------------------------------------------
$(deriveJSON defaultOptions ''Config)
