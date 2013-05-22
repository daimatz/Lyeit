module Lyeit.Type where

import           Data.List  (elemIndices)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

data FileType
    = Native
    | JSON
    | Markdown
    | RST
    | MediaWiki
    | DocBook
    | TexTile
    | HTML
    | LaTeX
    | Other
  deriving (Show, Eq, Ord)

extFileType :: Map String FileType
extFileType = Map.fromList [
      ( "txt"     , Native   )
    , ( "json"    , JSON     )
    , ( "markdown", Markdown )
    , ( "md"      , Markdown )
    , ( "mdown"   , Markdown )
    , ( "rst"     , RST      )
    , ( "sgm"     , DocBook  )
    , ( "sgml"    , DocBook  )
    , ( "xml"     , DocBook  )
    , ( "textile" , TexTile  )
    , ( "htm"     , HTML     )
    , ( "html"    , HTML     )
    , ( "tex"     , LaTeX    )
    ]

strToFileType :: String -> FileType
strToFileType s = fromMaybe Other (Map.lookup s extFileType)

-- | getFileType
-- convert filename to filetype
--
-- >>> getFileType "example.rst"
-- RST
-- >>> getFileType "example.com.md"
-- Markdown
-- >>> getFileType "example.md.html"
-- HTML
-- >>> getFileType "example.html.tex.gif"
-- Other
-- >>> getFileType ".example"
-- Other
-- >>> getFileType ".example.json"
-- JSON
getFileType :: FilePath -> FileType
getFileType path =
    case elemIndices '.' path of
        []      -> Other
        indices -> strToFileType $ drop (1 + last indices) path
