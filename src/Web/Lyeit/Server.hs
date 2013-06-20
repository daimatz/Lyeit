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
import           Data.Monoid          (mappend, (<>))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
import           System.Directory     (doesDirectoryExist, doesFileExist)
import           System.FilePath      (dropTrailingPathSeparator, normalise,
                                       pathSeparator, (</>))
import qualified Text.Pandoc          as P
import           Text.Pandoc.Shared   (stringify)
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
    isDirs <- forM fs $ \f -> do
        isDir <- liftIO $ doesDirectoryExist $ full </> f
        title <- case selectReader (getFileType f) of
            Just reader -> do
                contents <- liftIO $ tryNTimes readFile $ full </> f
                return $ fromMaybe (TL.pack f) $ getTitle $ reader P.def contents
            Nothing -> return $ TL.pack f
        return (isDir, title)
    let cts = foldl gather emptyDir ((uncurry zip3 . unzip) isDirs fs)

    h <- headHtml path Nothing Nothing
    b <- dirHtml path cts
    f <- footHtmlWithPath full
    responseHtml $ h <> b <> f
  where
    emptyDir = Map.fromList
        [ (Directory, [])
        , (Document, [])
        , (Other, [])
        ]
    add :: ListFiles -> ListType -> (FilePath, Title) -> ListFiles
    add lst genre p = Map.insertWith mappend genre [p] lst
    gather :: ListFiles -> (Bool, Title, FilePath) -> ListFiles
    gather lst (d, t, f) =
        if d then
            add lst Directory (f, t)
        else
            case getFileType f of
                OtherFile -> add lst Other (f, t)
                _         -> add lst Document (f, t)

actionFile :: FilePath -> ConfigM ()
actionFile path = do
    full <- fullpath path
    contents <- liftIO $ tryNTimes readFile full

    let responseDocument reader = do
            let pandoc = reader P.def contents
                title = fromMaybe (TL.pack path) $ getTitle pandoc
            h <- headHtml path (Just title) Nothing
            b <- TL.pack <$> flip P.writeHtmlString pandoc <$> def
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
    Html      -> Just P.readHtml
    _         -> Nothing

getTitle :: P.Pandoc -> Maybe Title
getTitle (P.Pandoc meta body) = case P.docTitle meta of
    [] -> getTitleFromBody body
    ils -> Just $ TL.pack $ stringify ils
  where
    getTitleFromBody [] = Nothing
    getTitleFromBody (P.Header _ _ ils : _) = Just $ TL.pack $ stringify ils
    getTitleFromBody (_ : next) = getTitleFromBody next

response :: ConfigM () -> ConfigM ()
response action = do
    lift $ S.header "Cache-Control" "no-cache, no-store, must-revalidate"
    lift $ S.header "Pragma" "no-cache"
    lift $ S.header "Expires" "0"
    action

def :: ConfigM P.WriterOptions
def = do
    url <- config mathjax_url
    return $ P.def
        { P.writerHTMLMathMethod  = P.MathJax url
        }

responseHtml :: Text -> ConfigM ()
responseHtml = response . lift . S.html

responseFile :: FilePath -> ConfigM ()
responseFile = response . lift . S.file

-- | fullpath
--
-- combine given path with document_root.
fullpath :: FilePath -> ConfigM FilePath
fullpath path
    = (</> dropWhile (== pathSeparator) path) <$> config document_root_full
