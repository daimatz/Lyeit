module Web.Lyeit.Config
    ( runConfigM
    , readConfig
    , config
    ) where

import           Control.Monad.Reader (asks, runReaderT)
import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromMaybe)
import           Prelude              hiding (FilePath)
import           System.Directory     (getHomeDirectory)
import qualified System.FilePath      as FP
import           Web.Scotty           (ActionM)

import           Web.Lyeit.FileUtil
import           Web.Lyeit.Type

runConfigM :: Config -> ConfigM a -> ActionM a
runConfigM = flip runReaderT

config :: (Config -> a) -> ConfigM a
config = asks

readConfig :: FullPath -> IO Config
readConfig (FullPath full) = do
    contents <- tryNTimes BSL.readFile (FullPath full)

    let c = fromMaybe (error "failed to decode config file of JSON") $
            decode contents

    (FullPath dfull) <- tildaToHome $ document_root_full c
    (FullPath tfull) <- tildaToHome $ template_path c

    return $ c
        { document_root_full = FullPath $ normalise dfull
        , document_root_show = normalise $ document_root_show c
        , template_path      = FullPath $ normalise tfull
        }

normalise :: String -> String
normalise = FP.dropTrailingPathSeparator . FP.normalise

tildaToHome :: FullPath -> IO FullPath
tildaToHome (FullPath path) = do
    home <- getHomeDirectory
    return $ FullPath $ FP.normalise $ replace '~' home path
  where
    replace _ _ [] = []
    replace from to (s:ss) = if s == from
            then to ++ replace from to ss
            else s : replace from to ss
