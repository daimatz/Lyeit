{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Web.Lyeit.Config
    ( Config (..)
    , ConfigM
    , runConfigM
    , readConfig
    , config
    ) where

import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import           Data.Aeson
import           Data.Aeson.TH        (deriveJSON)
import qualified Data.ByteString.Lazy as BSL
import           Data.Data            (Data, Typeable)
import           Data.Maybe           (fromMaybe)
import           Data.Text.Lazy       (Text)
import           Web.Scotty           (ActionM)

data Config = Config
    { host          :: Text
    , port          :: Int
    , document_root :: FilePath
    }
  deriving (Show, Eq, Ord, Data, Typeable)
$(deriveJSON id ''Config)

type ConfigM a = ReaderT Config ActionM a

runConfigM :: Config -> ConfigM a -> ActionM a
runConfigM = flip runReaderT

config :: (Config -> a) -> ConfigM a
config = asks

readConfig :: FilePath -> IO Config
readConfig path = do
    contents <- BSL.readFile path
    return $ fromMaybe (error "failed to decode config file of JSON") $
        decode contents
