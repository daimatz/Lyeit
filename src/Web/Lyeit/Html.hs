module Web.Lyeit.Html
    ( headHtml
    , dirHtml
    , footHtml
    , footHtmlWithPath
    )
    where

import           Control.Arrow           (first)
import           Control.Monad.Trans     (MonadIO, liftIO)
import           Data.Map                ((!))
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time               (formatTime)
import           Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import           System.FilePath         (pathSeparators, splitDirectories,
                                          (</>))
import           System.Locale           (defaultTimeLocale)
import           System.Posix            (getFileStatus, modificationTime)

import           Text.Hastache
import           Text.Hastache.Context

import           Web.Lyeit.Config
import           Web.Lyeit.Type

mustache :: MonadIO m => FilePath -> MuContext m -> m Text
mustache path context = return . decodeUtf8
    =<< hastacheFile defaultConfig path context

nullContext :: MonadIO m => MuContext m
nullContext = mkStrContext $ const $ MuVariable ("" :: Text)

errorRef :: MuType m
errorRef = MuVariable ("error" :: Text)

headHtml :: FilePath -> Text -> Text -> ConfigM Text
headHtml path title query = do
    root <- config document_root

    let splitter x y = (pathSeparators <> y, snd x </> y)
        paths = let splited = scanl splitter ("", pathSeparators) $
                        splitDirectories path
                in  first (root </>) (head splited) : tail splited

        context "title" = MuVariable title
        context "query" = MuVariable query
        context "path"  = MuVariable $ pathSeparators <> path
        context "paths" = MuList $ map (mkStrContext . listContext) paths
        context _       = errorRef

        listContext tuple "partial" = MuVariable $ fst tuple
        listContext tuple "request" = MuVariable $ snd tuple
        listContext _     _         = errorRef

    liftIO $ mustache "view/head.mustache" (mkStrContext context)

dirHtml :: FilePath -> ListFiles -> ConfigM Text
dirHtml request cts
    = liftIO $ mustache "view/dir.mustache" (mkStrContext context)
  where
    context "directory"
        = MuList $ map (mkStrContext . directory) (cts ! Directory)
    context "document"
        = MuList $ map (mkStrContext . document) (cts ! Document)
    context "other"
        = MuList $ map (mkStrContext . other) (cts ! Other)
    context _
        = errorRef
    -- FIXME: other properties than path, ex: title...
    directory dir  "path"  = MuVariable $ requestPath dir
    directory dir  "title" = MuVariable dir
    directory _    _       = errorRef
    document  doc  "path"  = MuVariable $ requestPath doc
    document  doc  "title" = MuVariable doc
    document  _    _       = errorRef
    other     ot   "path"  = MuVariable $ requestPath ot
    other     ot   "title" = MuVariable ot
    other     _    _       = errorRef
    requestPath path
        = (if null request then "" else "/" <> request) <> "/" <> path

footHtml :: ConfigM Text
footHtml = liftIO $ mustache "view/foot.mustache" nullContext

footHtmlWithPath :: FilePath -> ConfigM Text
footHtmlWithPath watchpath = do
    stat <- liftIO $ getFileStatus watchpath

    let mtime = posixSecondsToUTCTime $ realToFrac $ modificationTime stat
        form = formatTime defaultTimeLocale "updated: %F %T" mtime
        context "updated" = MuVariable form
        context _         = errorRef

    liftIO $ mustache "view/foot.mustache" (mkStrContext context)
