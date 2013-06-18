module Web.Lyeit.Type
    ( FileType (..)
    ) where

data FileType
    = Plain
    | JSON
    | Markdown
    | RST
    | MediaWiki
    | DocBook
    | TexTile
    | Html
    | LaTeX
    | OtherFile
  deriving (Show, Eq, Ord)

instance Read FileType where
    readsPrec _ s = case s of
        "txt"      -> [(Plain, "")]
        "json"     -> [(JSON, "")]
        "markdown" -> [(Markdown, "")]
        "md"       -> [(Markdown, "")]
        "mdown"    -> [(Markdown, "")]
        "rst"      -> [(RST, "")]
        "sgm"      -> [(DocBook, "")]
        "sgml"     -> [(DocBook, "")]
        "xml"      -> [(DocBook, "")]
        "textile"  -> [(TexTile, "")]
        "htm"      -> [(Html, "")]
        "html"     -> [(Html, "")]
        "tex"      -> [(LaTeX, "")]
        _          -> [(OtherFile, "")]
