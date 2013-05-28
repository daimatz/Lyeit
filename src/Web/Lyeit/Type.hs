module Web.Lyeit.Type
    ( FileType (..)
    , getFileType
    ) where

import           Data.Char  (toLower)
import           Data.List  (elemIndices)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

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
    | Other
  deriving (Show, Eq, Ord)

extFileType :: Map String FileType
extFileType = Map.fromList [
      ( "txt"     , Plain    )
    , ( "json"    , JSON     )
    , ( "markdown", Markdown )
    , ( "md"      , Markdown )
    , ( "mdown"   , Markdown )
    , ( "rst"     , RST      )
    , ( "sgm"     , DocBook  )
    , ( "sgml"    , DocBook  )
    , ( "xml"     , DocBook  )
    , ( "textile" , TexTile  )
    , ( "htm"     , Html     )
    , ( "html"    , Html     )
    , ( "tex"     , LaTeX    )
    ]

-- | extToFileType
-- convert extension string to filetype
--
-- >>> extToFileType "txt"
-- Plain
-- >>> extToFileType "mdown"
-- Markdown
-- >>> extToFileType "Html"
-- Html
-- >>> extToFileType "madown"
-- Other
extToFileType :: String -> FileType
extToFileType s = fromMaybe Other (Map.lookup (map toLower s) extFileType)

-- | getFileType
-- convert filename to filetype
--
-- >>> getFileType "example.rst"
-- RST
-- >>> getFileType "example.com.md"
-- Markdown
-- >>> getFileType "example.md.html"
-- Html
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
        indices -> extToFileType $ drop (1 + last indices) path
