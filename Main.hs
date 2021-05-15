module Main where

import Parser (parseCCode)
import PreProcess (preProcessCode)
import TypeCheck (typeCheck)

main :: IO ()
main = do
  let fName = "test/hello.c"
  sourceCode <- readFile fName
  pp <- preProcessCode fName sourceCode
  print $ do
    preProcessed <- pp
    parsed <- parseCCode preProcessed
    typeChecked <- typeCheck parsed
    return typeChecked
