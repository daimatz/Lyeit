module Main where

import           Control.Applicative ((<$>))
import           System.Environment  (getEnv)

import           Web.Lyeit.Server

main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    server port
