module Web.Lyeit.Server
    ( server
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as TL
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      getDirectoryContents)
import qualified Text.Pandoc         as P
import qualified Web.Scotty          as S

import           Web.Lyeit.Type

server :: Int -> IO ()
server port = S.scotty port $ do

    S.get "/search" $ do
        ps <- S.params
        maybe (S.raise "required parameters `p' and `q'")
            (uncurry actionSearch) $ do
            -- Maybe Monad
            path  <- lookup "p" ps
            query <- lookup "q" ps
            return (TL.unpack path, query)

    S.get (S.regex "^/(.*)$") $ do
        path   <- TL.unpack <$> trimLastSlashes <$> S.param "1"
        isFile <- liftIO $ doesFileExist path
        isDir  <- liftIO $ doesDirectoryExist path
        case (isFile, isDir) of
            (True, _) -> actionFile path
            (_, True) -> actionDir path
            _         -> S.next

    S.notFound $ S.html "<h1>Not Found.</h1>"

  where
    trimLastSlashes path =
        if TL.last path == '/' then
            trimLastSlashes (TL.init path)
        else
            path

actionSearch :: FilePath -> Text -> S.ActionM ()
actionSearch path query =
    responseHtml $ "Search: path = " <> TL.pack path <> ", query = " <> query

data DirectoryContents = DirectoryContents
    { directories :: [FilePath]
    , documents   :: [FilePath]
    , others      :: [FilePath]
    }
  deriving (Show, Read, Eq, Ord)

actionDir :: FilePath -> S.ActionM ()
actionDir path = do
    fs <- filter (`notElem` [".", ".."]) <$> liftIO (getDirectoryContents path)
    isDirs <- mapM (liftIO . doesDirectoryExist . ((path++"/")++)) fs
    let cts = foldl gather (DirectoryContents [] [] []) (zip fs isDirs)
    responseHtml $ TL.pack (show cts)
  where
    gather cts (f, d) =
        if d then
            cts { directories = f : directories cts }
        else
            case getFileType f of
                Other -> cts { others = f : others cts }
                _     -> cts { documents = f : documents cts }

actionFile :: FilePath -> S.ActionM ()
actionFile path = do
    contents <- liftIO $ readFile path
    let toHtml reader = TL.pack $ P.writeHtmlString P.def $ reader P.def contents
    case getFileType path of
        Native    -> responseFile path
        JSON      -> responseFile path
        Markdown  -> responseHtml $ toHtml P.readMarkdown
        RST       -> responseHtml $ toHtml P.readRST
        MediaWiki -> responseHtml $ toHtml P.readMediaWiki
        DocBook   -> responseHtml $ toHtml P.readDocBook
        TexTile   -> responseHtml $ toHtml P.readTextile
        Html      -> responseHtml $ TL.pack contents
        LaTeX     -> responseHtml $ toHtml P.readLaTeX
        Other     -> responseFile path

response :: S.ActionM () -> S.ActionM ()
response action = do
    S.header "Cache-Control" "no-cache, no-store, must-revalidate"
    S.header "Pragma" "no-cache"
    S.header "Expires" "0"
    action

responseHtml :: Text -> S.ActionM ()
responseHtml = response . S.html

responseFile :: FilePath -> S.ActionM ()
responseFile = response . S.file
