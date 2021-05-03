module TestPreProcessor where

import Test.Hspec

import Control.Monad
import Data.Either

import Lexeme
import Scanner
import PreProcess

testPreProcessor :: IO ()
testPreProcessor = do
  hspec testTriGraph
  hspec testLineSplice
  hspec testRemoveComments
  hspec testParsers

testTriGraph :: Spec
testTriGraph =
  describe "trigraph" $ do
    it "translates trigraph sequences" $ do
      let input = "??= ??/ ??' ??( ??) ??! ??< ??> ??- ???"
          expected = "# \\ ^ [ ] | { } ~ ???"
      trigraph input `shouldBe` expected

testLineSplice :: Spec
testLineSplice =
  describe "lineSplice" $ do
    it "concats lines ending with a backslash" $ do
      let input = "line1\nline2\\\nline3\\\nline4\nline5\n"
          expected = ["line1", "line2 line3 line4", "line5"]
      lineSplice input `shouldBe` expected

makeScanItem :: CLexeme -> ScanItem CLexeme
makeScanItem item = ScanItem { scanLoc = (0, 0), scanStr = "", scanItem = item }

testRemoveComments :: Spec
testRemoveComments =
  describe "removeComments" $ do
    it "replaces comments with single spaces" $ do
      let input = map
                    makeScanItem
                    [ LInt, LLabel "var1", LAssign
                    , LComment
                    , LIntLiteral 1
                    , LComment
                    , LSemiColon
                    ]
          expected = [ LInt, LLabel "var1", LAssign, LWhiteSpace
                     , LIntLiteral 1, LWhiteSpace, LSemiColon
                     ]
      map scanItem (removeComments input) `shouldBe` expected

testParsers :: Spec
testParsers =
  describe "translationUnitParser" $ do
    context "when given valid input" $ do
      it "parses source code lines" $ do
        let inputLine =
              map
                makeScanItem
                [ LPPConcat, LInt, LLabel "var1", LAssign
                , LIntLiteral 1, LSemiColon
                ]
            input = [inputLine]
            expected = PPTranslationUnit
                         [PPSourceLineCodeLine inputLine]
        runPPParser translationUnitParser input
          `shouldBe` (Right ([], expected))

      it "parses empty preprocessor directives" $ do
        let input = [map makeScanItem [LPPEmpty, LComment]]
            expected = PPTranslationUnit
                         [PPSourceLineDirective PPDirectiveEmpty]
        runPPParser translationUnitParser input
          `shouldBe` (Right ([], expected))

      it "parses include directives" $ do
        let inputLine = map
                          makeScanItem
                          [ LWhiteSpace, LPPInclude
                          , LLT, LLabel "stdio", LDot, LLabel "h", LGT
                          ]
            expected = PPTranslationUnit
                         [ PPSourceLineDirective
                           $ PPDirectiveInclude
                             $ PPIncludeMacro
                               $ drop 2 inputLine
                         ]
        runPPParser translationUnitParser [inputLine]
          `shouldBe` (Right ([], expected))

      it "parses define directives" $ do
        let inputLine =
              [ ScanItem { scanLoc = (0, 0), scanStr = "#define", scanItem = LPPDefine }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "VARNAME", scanItem = LLabel "VARNAME" }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "1", scanItem = LIntLiteral 1 }
              ]
            expected = PPTranslationUnit
                         [ PPSourceLineDirective
                           $ PPDirectiveDefine
                             $ PPDefineConst "VARNAME" "1"
                         ]
        runPPParser translationUnitParser [inputLine]
          `shouldBe` (Right ([], expected))

      it "parses macros" $ do
        let inputLine =
              -- #define VARNAME(a, b) (a) + (b)
              [ ScanItem { scanLoc = (0, 0), scanStr = "#define", scanItem = LPPDefine }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "VARNAME", scanItem = LLabel "VARNAME" }
              , ScanItem { scanLoc = (0, 0), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = (0, 0), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = (0, 0), scanStr = ",", scanItem = LComma }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "b", scanItem = LLabel "b" }
              , ScanItem { scanLoc = (0, 0), scanStr = ")", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = (0, 0), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = (0, 0), scanStr = ")", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "+", scanItem = LPlus }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = (0, 0), scanStr = "b", scanItem = LLabel "a" }
              , ScanItem { scanLoc = (0, 0), scanStr = ")", scanItem = LParenthesisClose }
              ]
            expected = PPTranslationUnit
                         [ PPSourceLineDirective
                           $ PPDirectiveDefine
                             $ PPDefineMacro "VARNAME" ["a", "b"] $ drop 10 inputLine
                         ]
        runPPParser translationUnitParser [inputLine]
          `shouldBe` (Right ([], expected))

      it "parses if directives" $ do
        let inputLines =
              [ "#if ifCondition1"
              , "#if ifCondition2"
              , "return 1;"
              , "#elif ifCondition3"
              , "return 2;"
              , "#endif"
              , "#else"
              , "return 3;"
              , "#endif"
              ]
            inputItems = traverse (scanCLine (0, 0)) inputLines
            expected = do
              inputItems' <- inputItems
              return $ PPTranslationUnit
                         [ PPSourceLineDirective (
                             PPDirectiveIf (
                               PPIf
                                 -- outer if condition
                                 (drop 2 $ inputItems' !! 0)
                                 -- outer if body
                                 [ PPSourceLineDirective (
                                     PPDirectiveIf (
                                       PPIf
                                         -- inner condition
                                         (drop 2 $ inputItems' !! 1)
                                         -- inner if body
                                         [ PPSourceLineCodeLine
                                           $ inputItems' !! 2
                                         ]
                                         -- inner if elif
                                         (Just (PPElif
                                                 -- elif condition
                                                 (drop 2 $ inputItems' !! 3)
                                                 -- elif body
                                                 [ PPSourceLineCodeLine
                                                   $ inputItems' !! 4
                                                 ]
                                                 -- second elif
                                                 Nothing))
                                         -- inner if else
                                         Nothing
                                     )
                                   )
                                 ]
                                 -- outer if elif
                                 Nothing
                                 -- outer else
                                 (Just (PPElse [ PPSourceLineCodeLine
                                                $ inputItems' !! 7
                                              ]))
                             )
                           )
                         ]
        (snd <$> (join (runPPParser translationUnitParser <$> inputItems)))
          `shouldBe` expected

    context "when given invalid input" $ do
      it "discards bad macro definitions" $ do
        let inputLine =
              -- #define VARNAME(a, b (a) + (b)
              [ ScanItem { scanLoc = (0, 0), scanStr = "#define", scanItem = LPPDefine }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "VARNAME", scanItem = LLabel "VARNAME" }
              , ScanItem { scanLoc = (0, 0), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = (0, 0), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = (0, 0), scanStr = ",", scanItem = LComma }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "b", scanItem = LLabel "b" }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = (0, 0), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = (0, 0), scanStr = ")", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "+", scanItem = LPlus }
              , ScanItem { scanLoc = (0, 0), scanStr = " ", scanItem = LWhiteSpace }
              , ScanItem { scanLoc = (0, 0), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = (0, 0), scanStr = "b", scanItem = LLabel "a" }
              , ScanItem { scanLoc = (0, 0), scanStr = ")", scanItem = LParenthesisClose }
              ]
        runPPParser translationUnitParser [inputLine]
          `shouldSatisfy` isLeft

      it "discards unterminated if statements" $ do
        let inputLines =
              [ map makeScanItem [LPPIf, LWhiteSpace, LLabel "ifcondition"]
              , map makeScanItem [LReturn, LWhiteSpace, LIntLiteral 1]
              ]
        runPPParser translationUnitParser inputLines
          `shouldSatisfy` isLeft

