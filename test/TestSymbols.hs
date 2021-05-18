module TestSymbols where

import Test.Hspec

import Data.Either
import qualified Data.Map as M

import Utils
import Symbols.TypeReader
import Scanner
import Parser.Parser
import Parser.ParserUtils
import Parser.ParseItem

testSymbols :: IO ()
testSymbols = do
  hspec testFunctionDefinition
  hspec testDeclaration

mainFunctionType :: CType
mainFunctionType =
  CType
    { storageClass = []
    , typeQualifier = []
    , dataType =
        TFunction
          "main"
          (CType [] [] TShort)
          [ CType [] [] TShort
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
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cFunctionDefinitionP)
        (ast >>= readFunctionDefinition ("", (0, 0)) initialSymbols . parseItem)
          `shouldBe` Right ("main", mainFunctionType, ["argc", "argv"])

      it "reads an old style function type" $ do
        let inputSource = "int main (argc, argv) int argc; char* argv[]; { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cFunctionDefinitionP)
        (ast >>= readFunctionDefinition ("", (0, 0)) initialSymbols . parseItem)
          `shouldBe` Right ("main", mainFunctionType, ["argc", "argv"])

      it "reads a function with no args" $ do
        let inputSource = "int main () { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cFunctionDefinitionP)
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
        (ast >>= readFunctionDefinition ("", (0, 0)) initialSymbols . parseItem)
          `shouldBe` Right ("main", expectedType, [])

    context "when given bad input" $ do
      it "rejects function with repeated argument names" $ do
        let inputSource = "int main (int foo, char foo) { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cFunctionDefinitionP)
        (ast >>= readFunctionDefinition ("", (0, 0)) initialSymbols . parseItem) `shouldSatisfy` isLeft

      it "rejects function with a struct return type" $ do
        let inputSource = "struct { int foo; } main () { return 0; }"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cFunctionDefinitionP)
        (ast >>= readFunctionDefinition ("", (0, 0)) initialSymbols . parseItem) `shouldSatisfy` isLeft

testDeclaration :: Spec
testDeclaration =
  describe "readDeclaration" $ do
    context "when given good input" $ do
      it "reads a variable declaration" $ do
        let inputSource = "int foo;"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType = TShort
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(False, "foo", expectedType)])

      it "reads a variable declaration with assignment" $ do
        let inputSource = "int foo = 1;"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType = TShort
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(True, "foo", expectedType)])

      it "reads a multiple variable declaration" $ do
        let inputSource = "const int foo = 1, bar = 2;"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = [TQConst]
                , dataType = TShort
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(True, "foo", expectedType), (True, "bar", expectedType)])

      it "reads an enum declaration" $ do
        let inputSource = "enum foo { FOO, BAR, BAZ };"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType =
                    TEnum
                      (Just "foo")
                      (M.fromList [("FOO", 1), ("BAR", 2), ("BAZ", 3)])
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(False, "foo", expectedType)])

      it "reads an union declaration" $ do
        let inputSource = "union foo { int bar; float baz; };"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType =
                    TUnion
                      (Just "foo")
                      (M.fromList
                        [ ( "bar"
                          , CType { storageClass = []
                                  , typeQualifier = []
                                  , dataType = TShort
                                  }
                          )
                        , ( "baz"
                          , CType { storageClass = []
                                  , typeQualifier = []
                                  , dataType = TFloat
                                  }
                          )
                        ])
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(False, "foo", expectedType)])

      it "reads a struct declaration" $ do
        let inputSource = "struct foo { int bar; float baz; };"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType =
                    TStruct
                      (Just "foo")
                      [ ( CType { storageClass = []
                                , typeQualifier = []
                                , dataType = TShort
                                }
                        , Just "bar"
                        , Nothing
                        )
                      , ( CType { storageClass = []
                                , typeQualifier = []
                                , dataType = TFloat
                                }
                        , Just "baz"
                        , Nothing
                        )
                      ]
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(False, "foo", expectedType)])

      it "reads a function declaration" $ do
        let inputSource = "int foo (int, float);"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType =
                    TFunction
                      "foo"
                      CType { storageClass = []
                            , typeQualifier = []
                            , dataType = TShort
                            }
                      [ CType { storageClass = []
                              , typeQualifier = []
                              , dataType = TShort
                              }
                      , CType { storageClass = []
                              , typeQualifier = []
                              , dataType = TFloat
                              }
                      ]
                      False
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(False, "foo", expectedType)])

      it "reads a function declaration with variable names" $ do
        let inputSource = "int foo (int bar, float baz);"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
            expectedType =
              CType
                { storageClass = []
                , typeQualifier = []
                , dataType =
                    TFunction
                      "foo"
                      CType { storageClass = []
                            , typeQualifier = []
                            , dataType = TShort
                            }
                      [ CType { storageClass = []
                              , typeQualifier = []
                              , dataType = TShort
                              }
                      , CType { storageClass = []
                              , typeQualifier = []
                              , dataType = TFloat
                              }
                      ]
                      False
                }
        (ast >>= readDeclaration initialSymbols . parseItem)
          `shouldBe` Right (False, [(False, "foo", expectedType)])

    context "when given bad input" $ do
      it "rejects a struct with multiple fields with same name" $ do
        let inputSource = "struct foo { int bar; float bar; };"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
        (ast >>= readDeclaration initialSymbols . parseItem) `shouldSatisfy` isLeft

    context "when given bad input" $ do
      it "rejects a function with struct as return type" $ do
        let inputSource = "struct { int foo; } bar (int baz);"
            scanItems = filter (\i -> scanItem i /= LWhiteSpace) <$> scanCLine ("", (0, 0)) inputSource
            ast = (\(_, i) -> i) <$> (scanItems >>= runParser cDeclarationP)
        (ast >>= readDeclaration initialSymbols . parseItem) `shouldSatisfy` isLeft

