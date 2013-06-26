module Web.Lyeit.Html
    ( dirMarkdown
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
import           System.FilePath     ((</>))
import qualified System.FilePath     as FP
import qualified Text.Pandoc         as P

import           Web.Lyeit.Config
import           Web.Lyeit.FileUtil
import           Web.Lyeit.Type

toHtml :: RequestPath -> String -> P.Pandoc -> ConfigM Text
toHtml (RequestPath request) query pandoc = do
    (FullPath template) <- config template_path

    exists <- liftIO $ doesFileExist template
    unless exists $ liftIO $ copyFile "view/default.html" template

    template_content <- liftIO $ readFile template

    root_show <- config document_root_show
    mathjax <- config mathjax_url

    (FullPath root_full) <- config document_root_full
    let full = root_full </> request
    stat <- liftIO $ getFileStat (FullPath full)

    let def = P.def
            { P.writerStandalone = True
            , P.writerTemplate = template_content
            , P.writerTableOfContents = True
            , P.writerHTMLMathMethod = P.MathJax mathjax
            , P.writerSectionDivs = True
            , P.writerVariables =
                [ ("mathjax_url", mathjax)
                , ("path_links", path_links root_show request)
                , ("search_form", search_form request query)
                , ("filename", FP.takeFileName request)
                , ("size", case stat of
                    StatFile {} -> show (statFileSizeKb stat)
                    _ -> "")
                ]
            }
    return $ TL.pack $ P.writeHtmlString def pandoc

dirMarkdown :: RequestPath -> ListFiles -> ConfigM String
dirMarkdown (RequestPath request) cts = do
    let headers = [Directory, Document, Other]
        body = concat $ concat $ flip map headers $ \h ->
            let c = cts ! h in
            ["## ", show h, "\n\n"]
            ++ map list c
            ++ ["\n"]
    root_show <- config document_root_show
    return $ "# Index of " ++ (root_show </> request) ++ "\n\n" ++ body
  where
    list (StatDir (RelativePath r) _)
        = concat ["- [", r, "/](", requestPath r, ") [", r, ", dir]\n"]
    list stat
        = let (RelativePath r) = statFileRelativePath stat in concat
            [ "- ["
            , statFileTitle stat
            , "]("
            , requestPath r
            , ") ["
            , r
            , " ("
            , show (statFileType stat)
            , "), "
            , show (statFileSizeKb stat)
            , "KB"
            , "]\n"
            ]
    requestPath relative
        = (if null request then "" else "/" <> request) <> "/" <> relative

path_links :: String -> String -> String
path_links root path = concatMap format paths
  where
    splitter x y = ([FP.pathSeparator] <> y, snd x </> y)
    paths = let splited = scanl splitter ("", [FP.pathSeparator]) $
                    FP.splitDirectories path
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
