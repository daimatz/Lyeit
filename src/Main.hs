module Main where

import           System.Environment (getArgs)

import           Web.Lyeit.Server
import           Web.Lyeit.Type

main :: IO ()
main = do
    args <- getArgs
    server $ FullPath (args !! 0)
