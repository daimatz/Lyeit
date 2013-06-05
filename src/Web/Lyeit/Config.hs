{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Web.Lyeit.Config
    ( Config (..)
    , ConfigM
    , runConfigM
    , readConfig
    , asks
    ) where

import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import qualified Data.Aeson           as Aeson
import           Data.Aeson.TH        (deriveJSON)
import qualified Data.ByteString.Lazy as BSL
import           Data.Data            (Data)
import           Data.Maybe           (fromMaybe)
import           Data.Text.Lazy       (Text)
import           Data.Typeable        (Typeable)
import           Web.Scotty           (ActionM)

data Config = Config
    { host         :: Text
    , port         :: Int
    , documentRoot :: FilePath
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable)
$(deriveJSON id ''Config)

type ConfigM a = ReaderT Config ActionM a

runConfigM :: Config -> ConfigM a -> ActionM a
runConfigM = flip runReaderT

readConfig :: FilePath -> IO Config
readConfig path = do
    contents <- BSL.readFile path
    return $ fromMaybe (error "failed to decode config file of JSON") $
        Aeson.decode contents
