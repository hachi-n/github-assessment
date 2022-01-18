module Main where

import qualified Scraping
import           System.Environment

main :: IO ()
main = getArgs >>= Scraping.handleArgs
