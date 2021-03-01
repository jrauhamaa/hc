module Main where

import Regex (scanRegex)
import qualified NFA

testRegex :: String
testRegex = "re(re)[e-r]re"

main :: IO ()
main = print $ NFA.fromRegexValue <$> scanRegex testRegex
