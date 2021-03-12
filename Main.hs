module Main where

import System.IO

import Lexeme (CLexeme(..))
import Parser (parseCCode)
import Scanner (ScanElement(..), scanCCode)

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
         return (parseCCode fltrd))
