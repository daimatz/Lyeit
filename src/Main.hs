module Main where

import           Web.Scotty

import           Lyeit.Type

main :: IO ()
main = do
    scotty 3000 $ do
        get "/" $ do
            html "hoge"
