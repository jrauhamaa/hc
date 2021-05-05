import TestScanner (testScanner)
import TestParser (testParser)
import TestPreProcessor (testPreProcessor)
import TestSymbols (testSymbols)

main :: IO ()
main = do
  testScanner
  testParser
  testPreProcessor
  testSymbols
