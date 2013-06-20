module Web.Lyeit.Config
    ( runConfigM
    , readConfig
    , config
    ) where

import           Control.Monad.Reader (asks, runReaderT)
import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromMaybe)
import           System.Environment   (getEnv)
import qualified System.FilePath      as FP
import           Web.Scotty           (ActionM)

import           Web.Lyeit.FileUtil
import           Web.Lyeit.Type

runConfigM :: Config -> ConfigM a -> ActionM a
runConfigM = flip runReaderT

config :: (Config -> a) -> ConfigM a
config = asks

readConfig :: FilePath -> IO Config
readConfig path = do
    contents <- tryNTimes BSL.readFile path

    let c = fromMaybe (error "failed to decode config file of JSON") $
            decode contents

    document_root_full_ <- tildaToHome $ document_root_full c
    template_path_      <- tildaToHome $ template_path c

    return $ c
        { document_root_full = normalise document_root_full_
        , document_root_show = normalise $ document_root_show c
        , template_path      = normalise template_path_
        }

normalise :: FilePath -> FilePath
normalise = FP.dropTrailingPathSeparator . FP.normalise

tildaToHome :: FilePath -> IO FilePath
tildaToHome path = do
    home <- getEnv "HOME"
    return $ replace '~' home path
  where
    replace _ _ [] = []
    replace from to (s:ss) = if s == from
            then to ++ replace from to ss
            else s : replace from to ss
