module Main where

import System.IO

import Scanner2 (scanCCode)
-- import Lexeme (CLexeme(..))

main :: IO ()
main = do
  withFile
    "test/hello.c"
    ReadMode
    (\f -> do
       contents <- hGetContents f
       print $ scanCCode contents)
{-
import Lexeme
import Parser (Parser(..), cParser)
import Scanner (ScanElement, fromSpec, scanInput)

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
       print $ do
         scanElems <- scanFileContents contents
         return
           $ show(runParser cParser $
            filter ((`notElem` [LWhiteSpace, LComment]) . snd) scanElems))
-}
