module Main where

import System.IO

import Parser (parseCCode)
import Scanner (scanCCode, filterWhiteSpace)

main :: IO ()
main = do
  withFile
    "test/hello.c"
    ReadMode
    (\f -> do
       contents <- hGetContents f
       print $ do
         scanItems <- scanCCode contents
         return (parseCCode $ filterWhiteSpace scanItems))
