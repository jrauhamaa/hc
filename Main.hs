module Main where

import System.IO

import Lexeme (CLexeme(..))
import Parser (Parser(..), cParser)
import Scanner2 (ScanElement(..), scanCCode)

main :: IO ()
main = do
  withFile
    "test/hello.c"
    ReadMode
    (\f -> do
       contents <- hGetContents f
       print $ do
         scanElems <- scanCCode contents
         let fltrd =
               [ l
               | l <- scanElems
               , scanElem l `notElem` [LWhiteSpace, LComment]
               ]
         return (runParser cParser fltrd))
