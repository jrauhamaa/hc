module Main where

import Scanner (fromSpec, scanInput)
import Terminal (CTerminal(..), terminals)


testInput :: String
testInput = "int float -123.4 true label"

testOutput :: Maybe [CTerminal]
testOutput = do
  scnr <- fromSpec terminals
  scanInput scnr testInput

main :: IO ()
main = print testOutput
