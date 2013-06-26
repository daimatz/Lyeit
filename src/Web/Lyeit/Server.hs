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
    (FullPath fullDirPath) <- fullpath (RequestPath request)
    fs <- liftIO $ dirFiles (FullPath fullDirPath)

    stats <- liftIO $ forM fs $ \(RelativePath f) ->
        getFileStat $ FullPath $ fullDirPath </> f
    let cts = foldl gather emptyDir stats
        body = dirHtml (RequestPath request) cts

    pandoc <- setMeta (RequestPath request) $ P.readMarkdown P.def body
    responseHtml =<< toHtml (RequestPath request) "" pandoc
  where
    emptyDir = Map.fromList
        [ (Directory, [])
        , (Document, [])
        , (Other, [])
        ]
    add :: ListFiles -> ListType -> FileStat -> ListFiles
    add lst genre stat = Map.insertWith mappend genre [stat] lst
    gather :: ListFiles -> FileStat -> ListFiles
    gather lst stat@(StatDir _ _) = add lst Directory stat
    gather lst stat = case statFileType stat of
        OtherFile -> add lst Other stat
        _         -> add lst Document stat

actionFile :: RequestPath -> ConfigM ()
actionFile (RequestPath request) = do
    (FullPath full) <- fullpath (RequestPath request)
    body <- liftIO $ tryNTimes readFile (FullPath full)

    let responseDocument reader = do
            pandoc <- setMeta (RequestPath request) $ reader P.def body
            responseHtml =<< toHtml (RequestPath request) "" pandoc

    maybe (responseFile $ FullPath full) responseDocument $
        selectReader $ getFileType full

setMeta :: RequestPath -> P.Pandoc -> ConfigM P.Pandoc
setMeta (RequestPath request) pandoc@(P.Pandoc meta body) = do
    (FullPath full) <- fullpath (RequestPath request)
    time <- liftIO $ timeFormat =<< getModificationTime full

    let title = case P.docTitle meta of
            [] -> [P.Str $ fromMaybe request $ getTitle pandoc]
            t  -> t
        date  = [P.Str time]

    return $ P.Pandoc meta { P.docTitle = title, P.docDate = date } body

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
