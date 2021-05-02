import TestScanner (testScanner)
import TestParser (testParser)

main :: IO ()
main = do
  testScanner
  testParser
