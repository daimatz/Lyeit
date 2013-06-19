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
import qualified Text.Hastache           as H
import qualified Text.Hastache.Context   as HC

import           Web.Lyeit.Config
import           Web.Lyeit.Type

mustache :: MonadIO m => FilePath -> H.MuContext m -> m Text
mustache path context = return . decodeUtf8
    =<< H.hastacheFile H.defaultConfig path context

nullContext :: MonadIO m => H.MuContext m
nullContext = HC.mkStrContext $ const $ H.MuVariable ("" :: Text)

errorRef :: H.MuType m
errorRef = H.MuVariable ("error" :: Text)

headHtml :: FilePath -> Text -> Text -> ConfigM Text
headHtml path title query = do
    root <- config document_root

    let splitter x y = (pathSeparators <> y, snd x </> y)
        paths = let splited = scanl splitter ("", pathSeparators) $
                        splitDirectories path
                in  first (root </>) (head splited) : tail splited

        context "title" = H.MuVariable title
        context "query" = H.MuVariable query
        context "path"  = H.MuVariable $ pathSeparators <> path
        context "paths" = H.MuList $ map (HC.mkStrContext . listContext) paths
        context _       = errorRef

        listContext tuple "partial" = H.MuVariable $ fst tuple
        listContext tuple "request" = H.MuVariable $ snd tuple
        listContext _     _         = errorRef

    liftIO $ mustache "view/head.mustache" (HC.mkStrContext context)

dirHtml :: FilePath -> ListFiles -> ConfigM Text
dirHtml request cts
    = liftIO $ mustache "view/dir.mustache" (HC.mkStrContext context)
  where
    context "directory"
        = H.MuList $ map (HC.mkStrContext . directory) (cts ! Directory)
    context "document"
        = H.MuList $ map (HC.mkStrContext . document) (cts ! Document)
    context "other"
        = H.MuList $ map (HC.mkStrContext . other) (cts ! Other)
    context _
        = errorRef
    -- FIXME: other properties than path, ex: title...
    directory dir  "path"  = H.MuVariable $ requestPath $ fst dir
    directory dir  "title" = H.MuVariable $ snd dir
    directory _    _       = errorRef
    document  doc  "path"  = H.MuVariable $ requestPath $ fst doc
    document  doc  "title" = H.MuVariable $ snd doc
    document  _    _       = errorRef
    other     ot   "path"  = H.MuVariable $ requestPath $ fst ot
    other     ot   "title" = H.MuVariable $ snd ot
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
        context "updated" = H.MuVariable form
        context _         = errorRef

    liftIO $ mustache "view/foot.mustache" (HC.mkStrContext context)
