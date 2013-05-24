module Web.Lyeit.Server where

import           Control.Monad.Trans (liftIO)
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as TL
import           System.Directory    (doesDirectoryExist)

import           Web.Scotty

server :: Int -> IO ()
server port = scotty port $ do
    get "/search" $ do
        ps <- params
        maybe (raise "required parameters `p' and `q'")
            (uncurry actionSearch) $ do
            -- Maybe Monad
            p <- lookup "p" ps
            q <- lookup "q" ps
            return (p, q)
    get (regex "^/(.*)$") $ do
        path <- param "1"
        isDir <- liftIO $ doesDirectoryExist $ TL.unpack path
        if isDir then actionDir path else actionFile path

actionSearch :: Text -> Text -> ActionM ()
actionSearch path query =
    response $ "Search: path = " <> path <> ", query = " <> query

actionDir :: Text -> ActionM ()
actionDir path =
    response $ "Directory: path = " <> path

actionFile :: Text -> ActionM ()
actionFile path =
    response $ "File: path = " <> path

response :: Text -> ActionM ()
response content = do
    header "Cache-Control" "no-cache, no-store, must-revalidate"
    header "Pragma" "no-cache"
    header "Expires" "0"
    html content
