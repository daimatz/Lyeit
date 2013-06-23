module Web.Lyeit.Server
    ( server
    ) where

import           Control.Applicative  ((<$>))
import           Control.Monad        (forM)
import           Control.Monad.Trans  (lift, liftIO)
import           Data.CaseInsensitive (mk)
import           Data.List            (sort)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (mappend)
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
import           Prelude              hiding (FilePath)
import           System.Directory     (doesDirectoryExist, doesFileExist,
                                       getModificationTime)
import           System.FilePath      (dropTrailingPathSeparator, normalise,
                                       (</>))
import qualified Text.Pandoc          as P
import           Text.Pandoc.Shared   (stringify)
import qualified Web.Scotty           as S

import           Web.Lyeit.Config
import           Web.Lyeit.FileUtil
import           Web.Lyeit.Html
import           Web.Lyeit.Type

server :: FullPath -> IO ()
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
                return (RequestPath $ TL.unpack path, query)

        get (S.regex "^/*(.*)$") $ do
            path <-  dropTrailingPathSeparator
                 <$> normalise
                 <$> TL.unpack
                 <$> param "1"
            actionPath $ RequestPath (if path == "." then "" else path)

        S.notFound $ S.html "<h1>Not Found.</h1>"
  where
    params  = lift S.params
    param   = lift . S.param
    raise   = lift . S.raise

actionPath :: RequestPath -> ConfigM ()
actionPath (RequestPath request) = do
    (FullPath full) <- fullpath (RequestPath request)
    isFile <- liftIO $ doesFileExist full
    isDir  <- liftIO $ doesDirectoryExist full
    case (isFile, isDir) of
        (True, _) -> actionFile (RequestPath request)
        (_, True) -> actionDir (RequestPath request)
        _         -> lift S.next

actionSearch :: RequestPath -> Text -> ConfigM ()
actionSearch (RequestPath request) query = do
    (FullPath full) <- fullpath (RequestPath request)
    fs <- liftIO $ findGrep (FullPath full) (TL.words query)
    responseHtml $ TL.pack $ show $ sort $ map (\(FullPath f) -> mk f) fs

actionDir :: RequestPath -> ConfigM ()
actionDir (RequestPath request) = do
    (FullPath full) <- fullpath (RequestPath request)
    fs <- liftIO $ dirFiles (FullPath full)
    isDirs <- forM fs $ \(RelativePath f) -> do
        isDir <- liftIO $ doesDirectoryExist $ full </> f
        title <- case selectReader (getFileType f) of
            Just reader -> do
                contents <- liftIO $ tryNTimes readFile (FullPath $ full </> f)
                return $ fromMaybe f $ getTitle $ reader P.def contents
            Nothing -> return f
        return (isDir, title)
    let cts = foldl gather emptyDir ((uncurry zip3 . unzip) isDirs fs)
        body = dirHtml (RequestPath request) cts

    pandoc <- setMeta (RequestPath request) $ P.readMarkdown P.def body
    responseHtml =<< toHtml (RequestPath request) "" pandoc
  where
    emptyDir = Map.fromList
        [ (Directory, [])
        , (Document, [])
        , (Other, [])
        ]
    add :: ListFiles -> ListType -> (RelativePath, Title) -> ListFiles
    add lst genre p = Map.insertWith mappend genre [p] lst
    gather :: ListFiles -> (Bool, Title, RelativePath) -> ListFiles
    gather lst (d, t, RelativePath f) =
        if d then
            add lst Directory (RelativePath f, t)
        else
            case getFileType f of
                OtherFile -> add lst Other (RelativePath f, t)
                _         -> add lst Document (RelativePath f, t)

actionFile :: RequestPath -> ConfigM ()
actionFile (RequestPath request) = do
    (FullPath full) <- fullpath (RequestPath request)
    body <- liftIO $ tryNTimes readFile (FullPath full)

    let responseDocument reader = do
            pandoc <- setMeta (RequestPath request) $ reader P.def body
            responseHtml =<< toHtml (RequestPath request) "" pandoc

    maybe (responseFile $ FullPath full) responseDocument $
        selectReader $ getFileType full

selectReader :: FileType -> Maybe (P.ReaderOptions -> String -> P.Pandoc)
selectReader tp = case tp of
    Markdown  -> Just P.readMarkdown
    RST       -> Just P.readRST
    MediaWiki -> Just P.readMediaWiki
    DocBook   -> Just P.readDocBook
    TexTile   -> Just P.readTextile
    LaTeX     -> Just P.readLaTeX
    Html      -> Just P.readHtml
    _         -> Nothing

setMeta :: RequestPath -> P.Pandoc -> ConfigM P.Pandoc
setMeta (RequestPath request) pandoc@(P.Pandoc meta body) = do
    (FullPath full) <- fullpath (RequestPath request)
    time <- liftIO $ timeFormat =<< getModificationTime full

    let title = case P.docTitle meta of
            [] -> [P.Str $ fromMaybe request $ getTitle pandoc]
            t  -> t
        date  = [P.Str time]

    return $ P.Pandoc meta { P.docTitle = title, P.docDate = date } body

getTitle :: P.Pandoc -> Maybe Title
getTitle (P.Pandoc meta body) = case P.docTitle meta of
    [] -> getTitleFromBody body
    ils -> Just $ stringify ils
  where
    getTitleFromBody [] = Nothing
    getTitleFromBody (P.Header _ _ ils : _) = Just $ stringify ils
    getTitleFromBody (_ : next) = getTitleFromBody next

response :: ConfigM () -> ConfigM ()
response action = do
    lift $ S.header "Cache-Control" "no-cache, no-store, must-revalidate"
    lift $ S.header "Pragma" "no-cache"
    lift $ S.header "Expires" "0"
    action

responseHtml :: Text -> ConfigM ()
responseHtml = response . lift . S.html

responseFile :: FullPath -> ConfigM ()
responseFile (FullPath full) = response $ lift $ S.file full
