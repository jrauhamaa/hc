module Main where

import System.IO

import Scanner (fromSpec, scanInput)
import Terminal (CTerminal(..), terminals)

scanFileContents :: String -> Maybe [CTerminal]
scanFileContents toParse = do
  scnr <- fromSpec terminals
  scanInput scnr toParse

main :: IO ()
main = do
  withFile
    "test/hello.c"
    ReadMode
    (\f -> do
       contents <- hGetContents f
       print $ scanFileContents contents)
