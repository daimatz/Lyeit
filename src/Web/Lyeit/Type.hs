module Web.Lyeit.Type
    ( FileType (..)
    , Config (..)
    , ConfigM
    , ListType (..)
    , Title
    , ListFiles
    ) where

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

data Config = Config
    { host               :: Text
    , port               :: Int
    , document_root_full :: FilePath
    , document_root_show :: FilePath
    , mathjax_url        :: String
    }
  deriving (Show, Eq, Ord)

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> o .:  "host"
        <*> o .:  "port"
        <*> o .:  "document_root"
        <*> o .:  "document_root"
        <*> o .:? "mathjax_url"   .!= mathjax_url_default
    parseJSON _ = error "failed to decode config file of JSON"

type ConfigM a = ReaderT Config ActionM a

data ListType = Directory | Document | Other
  deriving (Show, Read, Eq, Ord)

type Title = Text

type ListFiles = Map ListType [(FilePath, Text)]
