{-# LANGUAGE DoAndIfThenElse #-}
module Web.Lyeit.Server
    ( server
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as TL
import           System.Directory    (doesDirectoryExist, doesFileExist)
import           Text.Pandoc
import           Web.Scotty

import           Web.Lyeit.Type

server :: Int -> IO ()
server port = scotty port $ do

    get "/search" $ do
        ps <- params
        maybe (raise "required parameters `p' and `q'")
            (uncurry actionSearch) $ do
            -- Maybe Monad
            p <- lookup "p" ps
            q <- lookup "q" ps
            return (TL.unpack p, q)

    get (regex "^/(.*)$") $ do
        path <- TL.unpack <$> param "1"
        isFile <- liftIO $ doesFileExist path
        isDir <- liftIO $ doesDirectoryExist path
        if isFile then actionFile path
        else
            if isDir then actionDir path
            else next

    notFound $ html "<h1>Not Found.</h1>"

actionSearch :: FilePath -> Text -> ActionM ()
actionSearch path query =
    responseHtml $ "Search: path = " <> TL.pack path <> ", query = " <> query

actionDir :: FilePath -> ActionM ()
actionDir path =
    responseHtml $ "Directory: path = " <> TL.pack path

actionFile :: FilePath -> ActionM ()
actionFile path = do
    contents <- liftIO $ readFile path
    let toHtml reader = TL.pack $ writeHtmlString def $ reader def contents
    case getFileType path of
        Native    -> responseFile path
        JSON      -> responseFile path
        Markdown  -> responseHtml $ toHtml readMarkdown
        RST       -> responseHtml $ toHtml readRST
        MediaWiki -> responseHtml $ toHtml readMediaWiki
        DocBook   -> responseHtml $ toHtml readDocBook
        TexTile   -> responseHtml $ toHtml readTextile
        Html      -> responseHtml $ TL.pack contents
        LaTeX     -> responseHtml $ toHtml readLaTeX
        Other     -> responseFile path

response :: ActionM () -> ActionM ()
response action = do
    header "Cache-Control" "no-cache, no-store, must-revalidate"
    header "Pragma" "no-cache"
    header "Expires" "0"
    action

responseHtml :: Text -> ActionM ()
responseHtml = response . html

responseFile :: FilePath -> ActionM ()
responseFile = response . file
