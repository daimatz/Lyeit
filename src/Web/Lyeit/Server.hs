module Web.Lyeit.Server
    ( server
    ) where

import           Control.Applicative  ((<$>))
import           Control.Monad.Trans  (lift, liftIO)
import           Data.CaseInsensitive (mk)
import           Data.List            (sort)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Monoid          (mappend)
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
import           System.Directory     (doesDirectoryExist, doesFileExist)
import           System.FilePath      (dropTrailingPathSeparator, normalise,
                                       (</>))
import qualified Text.Pandoc          as P
import qualified Web.Scotty           as S

import           Web.Lyeit.Config
import           Web.Lyeit.FileUtil
import           Web.Lyeit.Type

server :: FilePath -> IO ()
server configPath = do
    -- TODO: Ugly...
    config <- readConfig configPath

    S.scotty (port config) $ do

        S.get "/search" $ do
            ps <- S.params
            maybe (S.raise "required parameters `p' and `q'")
                (runConfigM configPath . uncurry actionSearch) $ do
                -- Maybe Monad
                path  <- lookup "p" ps
                query <- lookup "q" ps
                return (TL.unpack path, query)

        S.get (S.regex "^(.*)$") $ do
            path   <-  dropTrailingPathSeparator
                   <$> normalise
                   <$> (documentRoot config </>)
                   <$> TL.unpack
                   <$> S.param "1"
            isFile <- liftIO $ doesFileExist path
            isDir  <- liftIO $ doesDirectoryExist path
            case (isFile, isDir) of
                (True, _) -> runConfigM configPath $ actionFile path
                (_, True) -> runConfigM configPath $ actionDir path
                _         -> S.next

        S.notFound $ S.html "<h1>Not Found.</h1>"

actionSearch :: FilePath -> Text -> ConfigM ()
actionSearch path query = do
    fs <- liftIO $ findGrep path (TL.words query)
    responseHtml $ TL.pack $ show $ sort $ map mk fs

type ListFiles = Map Text [FilePath]

actionDir :: FilePath -> ConfigM ()
actionDir path = do
    fs <- liftIO $ dirFiles path
    isDirs <- mapM (liftIO . doesDirectoryExist . ((path++"/")++)) fs
    let cts = foldl gather emptyDir  (zip fs isDirs)
    responseHtml $ TL.pack (show cts)
  where
    emptyDir = Map.fromList
        [ ("Directories", [])
        , ("Documents", [])
        , ("Others", [])
        ]
    add :: ListFiles -> Text -> FilePath -> ListFiles
    add lst genre p = Map.insertWith mappend genre [p] lst
    gather :: ListFiles -> (FilePath, Bool) -> ListFiles
    gather lst (f, d) =
        if d then
            add lst "Directories" f
        else
            case getFileType f of
                Other -> add lst "Others" f
                _     -> add lst "Documents" f

actionFile :: FilePath -> ConfigM ()
actionFile path = do
    contents <- liftIO $ readFile path
    let toHtml reader = TL.pack $ P.writeHtmlString P.def $ reader P.def contents
    case getFileType path of
        Plain     -> responseFile path
        JSON      -> responseFile path
        Markdown  -> responseHtml $ toHtml P.readMarkdown
        RST       -> responseHtml $ toHtml P.readRST
        MediaWiki -> responseHtml $ toHtml P.readMediaWiki
        DocBook   -> responseHtml $ toHtml P.readDocBook
        TexTile   -> responseHtml $ toHtml P.readTextile
        Html      -> responseHtml $ TL.pack contents
        LaTeX     -> responseHtml $ toHtml P.readLaTeX
        Other     -> responseFile path

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
