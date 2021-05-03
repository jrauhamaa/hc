import TestScanner (testScanner)
import TestParser (testParser)
import TestPreProcessor (testPreProcessor)

main :: IO ()
main = do
  testScanner
  testParser
  testPreProcessor
