module Web.Lyeit.Server
    ( server
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text, pack, unpack)
import           System.Directory    (doesDirectoryExist, doesFileExist)
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
            return (unpack path, query)

    S.get (S.regex "^/(.*)$") $ do
        path   <- unpack <$> S.param "1"
        isFile <- liftIO $ doesFileExist path
        isDir  <- liftIO $ doesDirectoryExist path
        case (isFile, isDir) of
            (True, _) -> actionFile path
            (_, True) -> actionDir path
            _         -> S.next

    S.notFound $ S.html "<h1>Not Found.</h1>"

actionSearch :: FilePath -> Text -> S.ActionM ()
actionSearch path query =
    responseHtml $ "Search: path = " <> pack path <> ", query = " <> query

actionDir :: FilePath -> S.ActionM ()
actionDir path =
    responseHtml $ "Directory: path = " <> pack path

actionFile :: FilePath -> S.ActionM ()
actionFile path = do
    contents <- liftIO $ readFile path
    let toHtml reader = pack $ P.writeHtmlString P.def $ reader P.def contents
    case getFileType path of
        Native    -> responseFile path
        JSON      -> responseFile path
        Markdown  -> responseHtml $ toHtml P.readMarkdown
        RST       -> responseHtml $ toHtml P.readRST
        MediaWiki -> responseHtml $ toHtml P.readMediaWiki
        DocBook   -> responseHtml $ toHtml P.readDocBook
        TexTile   -> responseHtml $ toHtml P.readTextile
        Html      -> responseHtml $ pack contents
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
