module TestSymbols where

import Test.Hspec

import Control.Monad
import Data.Either

import Lexeme
import Utils
import TypeCheck
import Symbols
import Scanner
import Parser
import ParseItem

testSymbols :: IO ()
testSymbols = do
  hspec testFunctionDefinition

mainFunctionType :: CType
mainFunctionType =
  CType
    { storageClass = []
    , typeQualifier = []
    , dataType =
        TFunction
          "main"
          (CType [] [] TShort)
          [ (CType [] [] TShort)
          , (CType
              { storageClass = []
              , typeQualifier = []
              , dataType =
                  TArray
                  (CType
                    { storageClass = []
                    , typeQualifier = []
                    , dataType =
                        TPointer (CType [] [] TChar)
                    })
                  Nothing
              })
          ]
          False
    }

testFunctionDefinition :: Spec
testFunctionDefinition =
  describe "readFunctionDefinition" $ do
    context "when given good input" $ do
      it "reads a new style function type" $ do
        let inputSource = "int main (int argc, char* argv[]) { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCLine (0, 0) inputSource)
            ast = (\(_, _, i) -> i) <$> join (runParser cFunctionDefinitionP <$> scanItems)
            tcItem =
              (\i -> TypeCheckItem
                       { typeCheckSymbols = initialSymbols
                       , previousType = emptyType
                       , typeCheckLoc = (0, 0)
                       , typeCheckItem = parseItem i
                       }) <$> ast
        join (readFunctionDefinition <$> tcItem)
          `shouldBe` (Right ("main", mainFunctionType, ["argc", "argv"]))

      it "reads an old style function type" $ do
        let inputSource = "int main (argc, argv) int argc; char* argv[]; { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCLine (0, 0) inputSource)
            ast = (\(_, _, i) -> i) <$> join (runParser cFunctionDefinitionP <$> scanItems)
            tcItem =
              (\i -> TypeCheckItem
                       { typeCheckSymbols = initialSymbols
                       , previousType = emptyType
                       , typeCheckLoc = (0, 0)
                       , typeCheckItem = parseItem i
                       }) <$> ast
        join (readFunctionDefinition <$> tcItem)
          `shouldBe` (Right ("main", mainFunctionType, ["argc", "argv"]))

      it "reads a function with no args" $ do
        let inputSource = "int main () { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCLine (0, 0) inputSource)
            ast = (\(_, _, i) -> i) <$> join (runParser cFunctionDefinitionP <$> scanItems)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType =
                    TFunction
                    "main"
                    (CType [] [] TShort)
                    []
                    False
                }
            tcItem =
              (\i -> TypeCheckItem
                       { typeCheckSymbols = initialSymbols
                       , previousType = emptyType
                       , typeCheckLoc = (0, 0)
                       , typeCheckItem = parseItem i
                       }) <$> ast
        join (readFunctionDefinition <$> tcItem)
          `shouldBe` (Right ("main", expectedType, []))

    context "when given bad input" $ do
      it "rejects function with repeated argument names" $ do
        let inputSource = "int main (int foo, char foo) { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCLine (0, 0) inputSource)
            ast = (\(_, _, i) -> i) <$> join (runParser cFunctionDefinitionP <$> scanItems)
            tcItem =
              (\i -> TypeCheckItem
                       { typeCheckSymbols = initialSymbols
                       , previousType = emptyType
                       , typeCheckLoc = (0, 0)
                       , typeCheckItem = parseItem i
                       }) <$> ast
        join (readFunctionDefinition <$> tcItem) `shouldSatisfy` isLeft

      it "rejects function with a struct return type" $ do
        let inputSource = "struct { int foo; } main () { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCLine (0, 0) inputSource)
            ast = (\(_, _, i) -> i) <$> join (runParser cFunctionDefinitionP <$> scanItems)
            tcItem =
              (\i -> TypeCheckItem
                       { typeCheckSymbols = initialSymbols
                       , previousType = emptyType
                       , typeCheckLoc = (0, 0)
                       , typeCheckItem = parseItem i
                       }) <$> ast
        join (readFunctionDefinition <$> tcItem) `shouldSatisfy` isLeft




