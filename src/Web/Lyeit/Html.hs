module Web.Lyeit.Html
    ( dirHtml
    , toHtml
    )
    where

import           Control.Arrow       (first)
import           Control.Monad       (unless)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Map            ((!))
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as TL
import           Prelude             hiding (FilePath)
import           System.Directory    (copyFile, doesFileExist)
import           System.FilePath     (pathSeparators, splitDirectories, (</>))
import qualified Text.Pandoc         as P

import           Web.Lyeit.Config
import           Web.Lyeit.Type

toHtml :: RequestPath -> String -> P.Pandoc -> ConfigM Text
toHtml (RequestPath request) query pandoc = do
    (FullPath template) <- config template_path

    exists <- liftIO $ doesFileExist template
    unless exists $ liftIO $ copyFile "view/default.html" template

    template_content <- liftIO $ readFile template

    root <- config document_root_show
    mathjax <- config mathjax_url


    let def = P.def
            { P.writerStandalone = True
            , P.writerTemplate = template_content
            , P.writerTableOfContents = True
            , P.writerHTMLMathMethod = P.MathJax mathjax
            , P.writerSectionDivs = True
            , P.writerVariables =
                [ ("mathjax_url", mathjax)
                , ("path_links", path_links root request)
                , ("search_form", search_form request query)
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

dirHtml :: RequestPath -> ListFiles -> String
dirHtml (RequestPath request) cts =
    let headers = [Directory, Document, Other] in
    concat $ concat $ flip map headers $ \h ->
        let c = cts ! h in
        ["<div id=\"dir", show h, "\"><h2>", show h, "</h2><ul>"]
        ++ map (uncurry list) c
        ++ ["</ul></div>"]
  where
    list (RelativePath relative) title
        = concat ["<li><a href=\"", requestPath relative, "\">", title, "</a></li>"]
    requestPath path
        = (if null request then "" else "/" <> request) <> "/" <> path
