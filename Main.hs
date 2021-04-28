module Main where

import Control.Monad
import System.IO

import Parser (parseCCode)
import Scanner (scanCCode, filterWhiteSpace)
import TypeCheck (typeCheck)

main :: IO ()
main = do
  withFile
    "test/hello.c"
    ReadMode $
    \f -> do
       contents <- hGetContents f
       print $ join $ do
         scanItems <- scanCCode contents
         return $ do
           ast <- parseCCode $ filterWhiteSpace scanItems
           return $ typeCheck ast

