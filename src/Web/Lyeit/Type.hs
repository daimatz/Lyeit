{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Web.Lyeit.Type
    ( FileType (..)
    , Config (..)
    , ConfigM
    , ListType (..)
    , Title
    , ListFiles
    ) where

import           Control.Monad.Reader (ReaderT)
import           Data.Aeson.TH        (deriveJSON)
import           Data.Data            (Data, Typeable)
import           Data.Map             (Map)
import           Data.Text.Lazy       (Text)
import           Web.Scotty           (ActionM)

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
    { host          :: Text
    , port          :: Int
    , document_root :: FilePath
    }
  deriving (Show, Eq, Ord, Data, Typeable)
$(deriveJSON id ''Config)

type ConfigM a = ReaderT Config ActionM a

data ListType = Directory | Document | Other
  deriving (Show, Read, Eq, Ord)

type Title = Text

type ListFiles = Map ListType [(FilePath, Text)]
