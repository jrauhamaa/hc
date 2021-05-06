module TestTypeCheck where

import Test.Hspec

import Control.Monad
import Data.Either
import qualified Data.Map as M

import Utils
import ParseItem
import Lexeme
import TypeCheck
import Scanner
import Parser

testTypeCheck :: IO ()
testTypeCheck = hspec $ do
  describe "typeCheck" $ do
    context "when given good input" $ do
      it "accepts valid c source code" $ do
        let sourceCode = "float foo = 1; int main (int argc, char* argv[]) { return 0; }\n"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
            expectedSymbols =
              M.fromList
                [ ("foo", CType [] [] TFloat)
                , ( "main"
                  , CType
                      []
                      []
                      (TFunction
                        "main"
                        (CType [] [] TShort)
                        [ (CType [] [] TShort)
                        , (CType
                            []
                            []
                            (TArray
                              (CType
                                []
                                []
                                (TPointer
                                  (CType [] [] TChar)))
                              Nothing))
                        ]
                        False)
                  )
                ]
        symbols <$> symbolTable <$> join (typeCheck <$> ast)
          `shouldBe` Right expectedSymbols

      it "reads enum declarations" $ do
        let sourceCode = "enum foo { FOO, BAR, BAZ };\n"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
            expectedSymbols =
              M.singleton
                "foo"
                (CType
                  []
                  []
                  (TEnum
                    (Just "foo")
                    (M.fromList [("FOO", 1), ("BAR", 1), ("BAZ", 1)])))
        enums <$> symbolTable <$> join (typeCheck <$> ast)
          `shouldBe` Right expectedSymbols

      it "reads union declarations" $ do
        let sourceCode = "union foo { int bar; float baz; };\n"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
            expectedSymbols =
              M.singleton
                "foo"
                (CType
                  []
                  []
                  (TUnion
                    (Just "foo")
                    (M.fromList
                      [ ("bar", CType [] [] TShort)
                      , ("baz", CType [] [] TFloat)
                      ])))
        unions <$> symbolTable <$> join (typeCheck <$> ast)
          `shouldBe` Right expectedSymbols

      it "reads struct declarations" $ do
        let sourceCode = "struct foo { int bar; float baz; };\n"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
            expectedSymbols =
              M.singleton
                "foo"
                (CType
                  []
                  []
                  (TStruct
                    (Just "foo")
                    [ (CType [] [] TShort, Just "bar", Nothing)
                    , (CType [] [] TFloat, Just "baz", Nothing)
                    ]))
        structs <$> symbolTable <$> join (typeCheck <$> ast)
          `shouldBe` Right expectedSymbols

      it "reads typedef declarations" $ do
        let sourceCode = "typedef struct foo { int bar; float baz; } qux;\n"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
            expectedType =
                (CType
                  []
                  []
                  (TStruct
                    (Just "foo")
                    [ (CType [] [] TShort, Just "bar", Nothing)
                    , (CType [] [] TFloat, Just "baz", Nothing)
                    ]))
        typedef <$> symbolTable <$> join (typeCheck <$> ast)
          `shouldBe` (Right $ M.singleton "qux" expectedType)
        structs <$> symbolTable <$> join (typeCheck <$> ast)
          `shouldBe` (Right $ M.singleton "foo" expectedType)

      it "accepts overlapping labels in different scopes" $ do
        let sourceCode = "float argc = 1.1; char foo = '1'; int main (int argc, char* argv[]) { int foo = 2; return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
        join (typeCheck <$> ast) `shouldSatisfy` isRight


      it "reads labels" $ do
        let sourceCode = "{ label1: printf(\"foobar\"); label2: return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCLine (0, 0) sourceCode)
            ast = join $ (runParser cCompoundStatementP) <$> scanItems
            expectedLabels =
              M.fromList [("label1", (0, 3)), ("label2", (0, 29))]
            typeCheckedAst = join ((\(_, _, ast') -> tCompoundStatement ast' initialSymbols) <$> ast)
            statements =
              case parseItem <$> typeCheckedAst of
                Left e -> Left e
                Right (CCompoundStatement _ statements') -> Right statements'
        labels <$> symbolTable <$> statements `shouldBe` Right expectedLabels


    context "when given bad input" $ do
      it "rejects code with variable name collisions" $ do
        let sourceCode = "float main = 1.1; int main (int argc, char* argv[]) { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
        join (typeCheck <$> ast) `shouldSatisfy` isLeft

      it "rejects code with variable name collisions inside a function" $ do
        let sourceCode = "int main (int argc, char* argv[]) { int foo = 0; char foo = '0'; return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> (scanCCode sourceCode)
            ast = join $ parseCCode <$> scanItems
        join (typeCheck <$> ast) `shouldSatisfy` isLeft

