import TestScanner (testScanner)
import TestParser (testParser)
import TestPreProcessor (testPreProcessor)
import TestSymbols (testSymbols)
import TestTypeCheck (testTypeCheck)

main :: IO ()
main = do
  testScanner
  testParser
  testPreProcessor
  testSymbols
  testTypeCheck
