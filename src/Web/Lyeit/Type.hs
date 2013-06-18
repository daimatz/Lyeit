{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Web.Lyeit.Type
    ( FileType (..)
    , Config (..)
    , ConfigM
    ) where

import           Control.Monad.Reader (ReaderT)
import           Data.Aeson.TH        (deriveJSON)
import           Web.Scotty           (ActionM)
import           Data.Data            (Data, Typeable)
import           Data.Text.Lazy       (Text)

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
