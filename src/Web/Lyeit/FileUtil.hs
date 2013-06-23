{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Lyeit.FileUtil where

import           Control.Applicative ((<$>), (<*>))
import           Control.Exception   (catch, throwIO)
import           Control.Monad       (forM, join)
import           Data.List           (elemIndices)
import           Data.String         (IsString)
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
import           Data.Time           (UTCTime, formatTime, utcToLocalZonedTime)
import           Prelude             hiding (FilePath)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.FilePath     ((</>), pathSeparator)
import           System.Locale       (defaultTimeLocale)
import qualified System.Posix        as Posix

import           Web.Lyeit.Config
import           Web.Lyeit.Const
import           Web.Lyeit.Type

-- | emurates `ls -a' command with not containing "." and "..".
dirFiles :: FullPath -> IO [RelativePath]
dirFiles (FullPath full) = do
     ls <- filter (`notElem` [".", ".."]) <$> getDirectoryContents full
     return $ map RelativePath ls

-- | emurates `find - grep' command.
-- find path -exec grep queries
findGrep :: FullPath -> [Text] -> IO [FullPath]
findGrep (FullPath path) queries = do
    isDir <- doesDirectoryExist path
    if isDir then
        matchDir $ FullPath path
    else
        matchFile $ FullPath path
  where
    inner :: FullPath -> IO [FullPath]
    inner (FullPath full) = do
        fs <- dirFiles (FullPath full)
        isDirs <- forM fs $ \(RelativePath f) -> doesDirectoryExist $ full </> f
        res <- forM (zip fs isDirs) $ \(RelativePath f, d) ->
            let newPath = FullPath $ full </> f in
            if d then
                matchDir newPath
            else
                matchFile newPath
        return $ join res
    matchToFilePath :: FullPath -> Bool
    matchToFilePath (FullPath full)
        = all (`TL.isInfixOf` TL.pack full) queries
    matchDir :: FullPath -> IO [FullPath]
    matchDir dir =
        -- if dirpath is directory, check the directory name
        if matchToFilePath dir then
            (:) <$> return dir <*> inner dir
        else
            inner dir
    matchFile :: FullPath -> IO [FullPath]
    matchFile (FullPath full) = case getFileType full of
        -- if FileType is not Document, check the file name
        OtherFile ->
            return [FullPath full | all (`TL.isInfixOf` TL.pack full) queries]
        -- if FileType is Document, read it and check the content
        _     -> do
            contents <- tryNTimes TLIO.readFile (FullPath full)
            -- TODO: once convert to text and search it
            -- TODO: case insensitive search (pull-request to case-insensitive?)
            return [FullPath full | all (`TL.isInfixOf` contents) queries]

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
-- OtherFile
-- >>> getFileType ".example"
-- OtherFile
-- >>> getFileType ".example.json"
-- JSON
getFileType :: String -> FileType
getFileType path =
    case elemIndices '.' path of
        []      -> OtherFile
        indices -> read $ drop (1 + last indices) path

tryNTimes :: forall s . (IsString s) => (String -> IO s) -> FullPath -> IO s
tryNTimes action (FullPath path) = inner retry_times
  where
    inner m = action path
        `catch` (\(e :: IOError) ->
            if m == 0 then throwIO e else inner (m-1))

getFileSizeKB :: FullPath -> IO Double
getFileSizeKB (FullPath full) = do
    size <- Posix.getFileStatus full
    let kb = fromIntegral (Posix.fileSize size) / (1024 :: Double)
    return $ fromIntegral (truncate (10 * kb) :: Int) / (10 :: Double)

timeFormat :: UTCTime -> IO String
timeFormat utctime = do
    zonedtime <- utcToLocalZonedTime utctime
    return $ formatTime defaultTimeLocale "%F (%a) %T" zonedtime

-- | fullpath
--
-- combine given path with document_root.
fullpath :: RequestPath -> ConfigM FullPath
fullpath (RequestPath request) = do
    (FullPath dfull) <- config document_root_full
    return $ FullPath $ dfull </> dropWhile (== pathSeparator) request
