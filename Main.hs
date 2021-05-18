module Main where

import Parser (parseCCode)
import PreProcess (preProcessCode)
import Symbols (typeAnnotate)
import Scanner (ScanItem(..), scanStr, CLexeme(..))

main :: IO ()
main = do
  let fName = "test/hellotransformed.c"
  let endmarker = ScanItem ("", (0, 0)) "" LEndMarker
  sourceCode <- readFile fName
  pp <- preProcessCode fName sourceCode
  print $ (concatMap scanStr <$> pp)
  let parsed = pp >>= (\x -> parseCCode $ x ++ [endmarker])
  print  (show parsed)
  let typechecked = parsed >>= typeAnnotate
  print  (show typechecked)
