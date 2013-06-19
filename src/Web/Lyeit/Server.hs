module Web.Lyeit.Server
    ( server
    ) where

import           Control.Applicative  ((<$>))
import           Control.Monad        (liftM)
import           Control.Monad.Trans  (lift, liftIO)
import           Data.CaseInsensitive (mk)
import           Data.List            (sort)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (mappend, (<>))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
import           System.Directory     (doesDirectoryExist, doesFileExist)
import           System.FilePath      (dropTrailingPathSeparator, normalise,
                                       (</>))
import qualified Text.Pandoc          as P
import qualified Web.Scotty           as S

import           Web.Lyeit.Config
import           Web.Lyeit.FileUtil
import           Web.Lyeit.Html
import           Web.Lyeit.Type

server :: FilePath -> IO ()
server configPath = do
    c <- readConfig configPath

    let get uri = S.get uri . runConfigM c

    S.scotty (port c) $ do

        get "/search" $ do
            ps <- params
            maybe (raise "required parameters `p' and `q'")
                (uncurry actionSearch) $ do
                -- Maybe Monad
                path  <- lookup "p" ps
                query <- lookup "q" ps
                return (TL.unpack path, query)

        get (S.regex "^/*(.*)$") $ do
            path <-  dropTrailingPathSeparator
                 <$> normalise
                 <$> TL.unpack
                 <$> param "1"
            actionPath (if path == "." then "" else path)

        S.notFound $ S.html "<h1>Not Found.</h1>"
  where
    params  = lift S.params
    param   = lift . S.param
    raise   = lift . S.raise

actionPath :: FilePath -> ConfigM ()
actionPath path = do
    full   <- fullpath path
    isFile <- liftIO $ doesFileExist full
    isDir  <- liftIO $ doesDirectoryExist full
    case (isFile, isDir) of
        (True, _) -> actionFile path
        (_, True) -> actionDir path
        _         -> lift S.next

actionSearch :: FilePath -> Text -> ConfigM ()
actionSearch path query = do
    full <- fullpath path
    fs <- liftIO $ findGrep full (TL.words query)
    responseHtml $ TL.pack $ show $ sort $ map mk fs

actionDir :: FilePath -> ConfigM ()
actionDir path = do
    full <- fullpath path
    fs <- liftIO $ dirFiles full
    isDirs <- mapM (liftIO . doesDirectoryExist . (full </>)) fs
    let cts = foldl gather emptyDir  (zip fs isDirs)
    h <- headHtml path (TL.pack $ "Index of " <> path) ""
    b <- dirHtml path cts
    f <- footHtmlWithPath full
    responseHtml $ h <> b <> f
  where
    emptyDir = Map.fromList
        [ (Directory, [])
        , (Document, [])
        , (Other, [])
        ]
    add :: ListFiles -> ListType -> FilePath -> ListFiles
    add lst genre p = Map.insertWith mappend genre [p] lst
    gather :: ListFiles -> (FilePath, Bool) -> ListFiles
    gather lst (f, d) =
        if d then
            add lst Directory f
        else
            case getFileType f of
                OtherFile -> add lst Other f
                _         -> add lst Document f

actionFile :: FilePath -> ConfigM ()
actionFile path = do
    full <- fullpath path
    contents <- liftIO $ readFile full

    let responseDocument reader = do
            let pandoc = reader P.def contents
                title = fromMaybe path $ getTitle pandoc
            h <- headHtml path (TL.pack title) ""
            let b = TL.pack $ P.writeHtmlString P.def pandoc
            f <- footHtmlWithPath full
            responseHtml $ h <> b <> f

    maybe (responseFile full) responseDocument $
        selectReader $ getFileType full

selectReader :: FileType -> Maybe (P.ReaderOptions -> String -> P.Pandoc)
selectReader tp = case tp of
    Markdown  -> Just P.readMarkdown
    RST       -> Just P.readRST
    MediaWiki -> Just P.readMediaWiki
    DocBook   -> Just P.readDocBook
    TexTile   -> Just P.readTextile
    LaTeX     -> Just P.readLaTeX
    _         -> Nothing

getTitle :: P.Pandoc -> Maybe String
getTitle (P.Pandoc meta body) = case P.docTitle meta of
    P.Str x : _ -> Just x
    _ -> getTitleFromBody body
  where
    getTitleFromBody [] = Nothing
    getTitleFromBody (P.Header _ (str,_,_) _ : _) = Just str
    getTitleFromBody (_ : next) = getTitleFromBody next

response :: ConfigM () -> ConfigM ()
response action = do
    lift $ S.header "Cache-Control" "no-cache, no-store, must-revalidate"
    lift $ S.header "Pragma" "no-cache"
    lift $ S.header "Expires" "0"
    action

responseHtml :: Text -> ConfigM ()
responseHtml = response . lift . S.html

responseFile :: FilePath -> ConfigM ()
responseFile = response . lift . S.file

fullpath :: FilePath -> ConfigM FilePath
fullpath path = (</> path) `liftM` config document_root
