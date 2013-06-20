module Web.Lyeit.Html
    ( headHtml
    , dirHtml
    , footHtml
    , footHtmlWithPath
    , toHtml
    )
    where

import           Control.Arrow           (first)
import           Control.Monad           (when)
import           Control.Monad.Trans     (MonadIO, liftIO)
import           Data.Map                ((!))
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time               (formatTime)
import           Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import           System.Directory        (copyFile, doesFileExist)
import           System.FilePath         (pathSeparators, splitDirectories,
                                          (</>))
import           System.Locale           (defaultTimeLocale)
import           System.Posix            (getFileStatus, modificationTime)
import qualified Text.Hastache           as H
import qualified Text.Hastache.Context   as HC
import qualified Text.Pandoc             as P

import           Web.Lyeit.Config
import           Web.Lyeit.Type

toHtml :: String -> String -> P.Pandoc -> ConfigM Text
toHtml path query pandoc = do
    template <- config template_path

    exists <- liftIO $ doesFileExist template
    when (not exists) $ liftIO $ copyFile "view/default.html" template

    template_content <- liftIO $ readFile template

    root <- config document_root_show
    mathjax <- config mathjax_url

    let def = P.def
            { P.writerStandalone = True
            , P.writerTemplate = template_content
            , P.writerTableOfContents = True
            , P.writerHTMLMathMethod = P.MathJax mathjax
            , P.writerVariables =
                [ ("mathjax_url", mathjax)
                , ("path_links", path_links root path)
                , ("search_form", search_form path query)
                ]
            }
    return $ TL.pack $ P.writeHtmlString def pandoc

path_links :: String -> String -> String
path_links root path = concatMap format paths
  where
    splitter x y = (pathSeparators <> y, snd x </> y)
    paths = let splited = scanl splitter ("", pathSeparators) $
                    splitDirectories path
            in  first (root </>) (head splited) : tail splited
    format (partial, request) = concat
        [ "<a href=\"", request, "\">", partial, "</a>"
        ]

search_form :: String -> String -> String
search_form p q = concat
    [ "<form action=\"/search\" method=\"get\">"
    , "<input type=\"hidden\" name=\"p\" value=\"", p, "\" />"
    , "<input type=\"text\" name=\"q\" value=\"", q, "\" size=\"24\" />"
    , "<input type=\"submit\" value=\"search\" />"
    , "</form>"
    ]

mustache :: MonadIO m => FilePath -> H.MuContext m -> m Text
mustache path context = return . decodeUtf8
    =<< H.hastacheFile H.defaultConfig path context

nullContext :: MonadIO m => H.MuContext m
nullContext = HC.mkStrContext $ const $ H.MuVariable ("" :: Text)

errorRef :: H.MuType m
errorRef = H.MuVariable ("error" :: Text)

headHtml :: FilePath -> Maybe Text -> Maybe Text -> ConfigM Text
headHtml path title query = do
    root <- config document_root_show
    mathjax <- config mathjax_url

    let splitter x y = (pathSeparators <> y, snd x </> y)
        paths = let splited = scanl splitter ("", pathSeparators) $
                        splitDirectories path
                in  first (root </>) (head splited) : tail splited

        context "title" = H.MuVariable $
            fromMaybe (TL.pack $ concat $ "Index of " : map fst paths) title
        context "query" = H.MuVariable $ fromMaybe "" query
        context "path"  = H.MuVariable $ pathSeparators <> path
        context "paths" = H.MuList $ map (HC.mkStrContext . listContext) paths
        context "mathjax_url" = H.MuVariable mathjax
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
