{-# LANGUAGE DoAndIfThenElse #-}

module Web.Lyeit.DirUtil where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM, join)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.FilePath     ((</>))

-- | `ls -a' command with not containing "." and "..".
dirFiles :: FilePath -> IO [FilePath]
dirFiles dir = filter (`notElem` [".", ".."]) <$> getDirectoryContents dir

-- | `find' command
findCommand :: FilePath -> IO [FilePath]
findCommand path = do
    isDir <- doesDirectoryExist path
    if isDir then
        (:) <$> return path <*> inner path
    else
        return [path]
  where
    inner :: FilePath -> IO [FilePath]
    inner dir = do
        fs <- dirFiles dir
        isDirs <- mapM (doesDirectoryExist . (dir </>)) fs
        res <- forM (zip fs isDirs) $ \(f, d) ->
            let newPath = dir </> f in
            if d then
                (:) <$> return newPath <*> inner newPath
            else
                return [newPath]
        return $ join res
