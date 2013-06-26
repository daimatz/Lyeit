module Web.Lyeit.Type where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad.Reader (ReaderT)
import           Data.Map             (Map)
import           Data.Text.Lazy       (Text)
import           Web.Scotty           (ActionM)

import           Data.Aeson

import           Web.Lyeit.Const

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
        "Plain"    -> [(Plain, "")]

        "json"     -> [(JSON, "")]
        "JSON"     -> [(JSON, "")]

        "markdown" -> [(Markdown, "")]
        "md"       -> [(Markdown, "")]
        "mdown"    -> [(Markdown, "")]
        "Markdown" -> [(Markdown, "")]

        "rst"      -> [(RST, "")]
        "RST"      -> [(RST, "")]

        "sgm"      -> [(DocBook, "")]
        "sgml"     -> [(DocBook, "")]
        "xml"      -> [(DocBook, "")]
        "DocBook"  -> [(DocBook, "")]

        "textile"  -> [(TexTile, "")]
        "TexTile"  -> [(TexTile, "")]

        "htm"      -> [(Html, "")]
        "html"     -> [(Html, "")]
        "Html"      -> [(Html, "")]

        "tex"      -> [(LaTeX, "")]
        "LaTeX"    -> [(LaTeX, "")]

        _          -> [(OtherFile, "")]

data Config = Config
    { host               :: Text
    , port               :: Int
    , document_root_full :: FullPath
    , document_root_show :: String
    , mathjax_url        :: String
    , template_path      :: FullPath
    }
  deriving (Show, Eq, Ord)

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> o .:  "host"
        <*> o .:  "port"
        -- Not Yet FullPath !!
        <*> fmap FullPath (o .:  "document_root")
        <*> o .:  "document_root"
        <*> o .:? "mathjax_url" .!= mathjax_url_default
        -- Not Yet FullPath !!
        <*> fmap FullPath (o .:? "template_path" .!= template_path_default)
    parseJSON _ = error "failed to decode config file of JSON"

type ConfigM a = ReaderT Config ActionM a

data ListType = Directory | Document | Other
  deriving (Show, Read, Eq, Ord)

type Title = String

type ListFiles = Map ListType [FileStat]

data FileStat
    = StatDir
        { statDirRelativePath :: RelativePath
        , statDirFullPath     :: FullPath
        }
    | StatFile
        { statFileType         :: FileType
        , statFileRelativePath :: RelativePath
        , statFileFullPath     :: FullPath
        , statFileTitle        :: Title
        , statFileSizeKb       :: Double
        }
  deriving (Show, Read, Eq, Ord)

newtype FullPath = FullPath FilePath
  deriving (Show, Read, Eq, Ord)
newtype RelativePath = RelativePath FilePath
  deriving (Show, Read, Eq, Ord)
newtype RequestPath = RequestPath FilePath
  deriving (Show, Read, Eq, Ord)
