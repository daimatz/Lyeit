module Web.Lyeit.Config
    ( runConfigM
    , readConfig
    , config
    ) where

import           Control.Monad.Reader (asks, runReaderT)
import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromMaybe)
import           Web.Scotty           (ActionM)

import           Web.Lyeit.Type

runConfigM :: Config -> ConfigM a -> ActionM a
runConfigM = flip runReaderT

config :: (Config -> a) -> ConfigM a
config = asks

readConfig :: FilePath -> IO Config
readConfig path = do
    contents <- BSL.readFile path
    return $ fromMaybe (error "failed to decode config file of JSON") $
        decode contents
