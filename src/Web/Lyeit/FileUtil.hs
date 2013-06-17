{-# LANGUAGE DoAndIfThenElse #-}

module Web.Lyeit.FileUtil where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM, join)
import           Data.List           (elemIndices)
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.FilePath     ((</>))

import           Web.Lyeit.Type

-- | emurates `ls -a' command with not containing "." and "..".
dirFiles :: FilePath -> IO [FilePath]
dirFiles dir = filter (`notElem` [".", ".."]) <$> getDirectoryContents dir

-- | emurates `find - grep' command.
-- find path -exec grep queries
findGrep :: FilePath -> [Text] -> IO [FilePath]
findGrep path queries = do
    isDir <- doesDirectoryExist path
    if isDir then
        matchDir path
    else
        matchFile path
  where
    inner :: FilePath -> IO [FilePath]
    inner dir = do
        fs <- dirFiles dir
        isDirs <- mapM (doesDirectoryExist . (dir </>)) fs
        res <- forM (zip fs isDirs) $ \(f, d) ->
            let newPath = dir </> f in
            if d then
                matchDir newPath
            else
                matchFile newPath
        return $ join res
    matchToFilePath :: FilePath -> Bool
    matchToFilePath pathstr = all (`TL.isInfixOf` TL.pack pathstr) queries
    matchDir :: FilePath -> IO [FilePath]
    matchDir dirpath =
        -- if dirpath is directory, check the directory name
        if matchToFilePath dirpath then
            (:) <$> return dirpath <*> inner dirpath
        else
            inner dirpath
    matchFile :: FilePath -> IO [FilePath]
    matchFile filepath = case getFileType filepath of
        -- if FileType is not Document, check the file name
        Other ->
            return [filepath | all (`TL.isInfixOf` TL.pack filepath) queries]
        -- if FileType is Document, read it and check the content
        _     -> do
            contents <- TLIO.readFile filepath
            -- TODO: once convert to text and search it
            -- TODO: case insensitive search (pull-request to case-insensitive?)
            return [filepath | all (`TL.isInfixOf` contents) queries]

-- | getFileType
-- convert filename to filetype
--
-- >>> getFileType "example.rst"
-- RST
-- >>> getFileType "example.com.md"
-- Markdown
-- >>> getFileType "example.md.html"
-- Html
-- >>> getFileType "example.html.tex.gif"
-- Other
-- >>> getFileType ".example"
-- Other
-- >>> getFileType ".example.json"
-- JSON
getFileType :: FilePath -> FileType
getFileType path =
    case elemIndices '.' path of
        []      -> Other
        indices -> read $ drop (1 + last indices) path
