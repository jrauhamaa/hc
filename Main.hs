module Main where

import Regex (scanRegex)

testRegex :: String
testRegex = "re(re)[e-r]re"

main :: IO ()
main = print $ scanRegex testRegex
