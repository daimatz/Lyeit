{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Web.Lyeit.Config
    ( Config (..)
    , ConfigM
    , runConfigM
    , asks
    ) where

import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Aeson.TH        (deriveJSON)
import           Data.Data            (Data)
import           Data.Text.Lazy       (Text)
import           Data.Typeable        (Typeable)
import           Web.Scotty           (ActionM)

data Config = Config
    { configHost         :: Text
    , configPort         :: Int
    , configDocumentRoot :: FilePath
    }
  deriving (Show, Read, Eq, Ord, Data, Typeable)
$(deriveJSON id ''Config)

type ConfigM a = ReaderT Config ActionM a

runConfigM :: Config -> ConfigM a -> ActionM a
runConfigM = flip runReaderT
