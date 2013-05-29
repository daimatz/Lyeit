module Main where

import           System.Environment (getArgs)

import           Web.Lyeit.Server

main :: IO ()
main = do
    args <- getArgs
    server (args !! 0)
