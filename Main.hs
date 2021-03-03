module Main where

import System.IO

import Scanner (fromSpec, scanInput, ScanElement)
import Lexeme (lexemes)

scanFileContents :: String -> Maybe [ScanElement]
scanFileContents toParse = do
  scnr <- fromSpec lexemes
  scanInput scnr toParse (1, 1)

main :: IO ()
main = do
  withFile
    "test/hello.c"
    ReadMode
    (\f -> do
       contents <- hGetContents f
       print $ scanFileContents contents)
