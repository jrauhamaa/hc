module TestPreProcessor where

import Test.Hspec

import System.IO
import Data.Either
import qualified Data.Map as M

import Utils
import Scanner
import PreProcess.Macro
import PreProcess.PreTransform
import PreProcess.Parser
import PreProcess.PPTransform

testPreProcessor :: IO ()
testPreProcessor = do
  hspec testTriGraph
  hspec testLineSplice
  hspec testRemoveComments
  hspec testParsers
  hspec testConcatTokens
  hspec testPPTransform
  hspec testPreTransform
  hspec testMacroExpand
  testPreProcessCode

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
makeScanItem item = ScanItem { scanLoc = ("", (0, 0)), scanStr = "", scanItem = item }

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
          expected = [LInt, LLabel "var1", LAssign, LWhiteSpace, LIntLiteral 1, LWhiteSpace, LSemiColon]
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
          `shouldBe` Right ([], expected)

      it "parses empty preprocessor directives" $ do
        let input = [map makeScanItem [LPPEmpty]]
            expected = PPTranslationUnit
                         [PPSourceLineDirective PPDirectiveEmpty]
        runPPParser translationUnitParser input
          `shouldBe` Right ([], expected)

      it "parses include directives" $ do
        let inputLine = map
                          makeScanItem
                          [ LPPInclude
                          , LLT, LLabel "stdio", LDot, LLabel "h", LGT
                          ]
            expected = PPTranslationUnit
                         [ PPSourceLineDirective
                           $ PPDirectiveInclude
                             $ PPIncludeMacro
                               $ tail inputLine
                         ]
        runPPParser translationUnitParser [inputLine]
          `shouldBe` Right ([], expected)

      it "parses define directives" $ do
        let inputLine =
              [ ScanItem { scanLoc = ("", (0, 0)), scanStr = "#define ", scanItem = LPPDefine }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "VARNAME ", scanItem = LLabel "VARNAME" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "1", scanItem = LIntLiteral 1 }
              ]
            expected = PPTranslationUnit
                         [ PPSourceLineDirective
                           $ PPDirectiveDefine
                             $ PPDefineConst "VARNAME" "1"
                         ]
        runPPParser translationUnitParser [inputLine]
          `shouldBe` Right ([], expected)

      it "parses macros" $ do
        let inputLine =
              -- #define VARNAME(a, b) (a) + (b)
              [ ScanItem { scanLoc = ("", (0, 0)), scanStr = "#define ", scanItem = LPPDefine }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "VARNAME", scanItem = LLabel "VARNAME" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ", ", scanItem = LComma }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "b", scanItem = LLabel "b" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ") ", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ") ", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "+ ", scanItem = LPlus }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "b", scanItem = LLabel "a" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ")", scanItem = LParenthesisClose }
              ]
            expected = PPTranslationUnit
                         [ PPSourceLineDirective
                           $ PPDirectiveDefine
                             $ PPDefineMacro "VARNAME" ["a", "b"] $ drop 7 inputLine
                         ]
        runPPParser translationUnitParser [inputLine]
          `shouldBe` Right ([], expected)

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
            inputItems = traverse (scanCLineNoWS ("", (0, 0))) inputLines
            expected = do
              inputItems' <- inputItems
              return $ PPTranslationUnit
                         [ PPSourceLineDirective (
                             PPDirectiveIf (
                               PPIf
                                 -- outer if condition
                                 (tail $ head inputItems')
                                 -- outer if body
                                 [ PPSourceLineDirective (
                                     PPDirectiveIf (
                                       PPIf
                                         -- inner condition
                                         (tail $ inputItems' !! 1)
                                         -- inner if body
                                         [ PPSourceLineCodeLine
                                           $ inputItems' !! 2
                                         ]
                                         -- inner if elif
                                         (Just (PPElif
                                                 -- elif condition
                                                 (tail $ inputItems' !! 3)
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
        (snd <$> (inputItems >>= runPPParser translationUnitParser))
          `shouldBe` expected

    context "when given invalid input" $ do
      it "discards bad macro definitions" $ do
        let inputLine =
              -- #define VARNAME(a, b c) (a) + (b)
              [ ScanItem { scanLoc = ("", (0, 0)), scanStr = "#define ", scanItem = LPPDefine }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "VARNAME", scanItem = LLabel "VARNAME" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ", ", scanItem = LComma }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "b ", scanItem = LLabel "b" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "c", scanItem = LLabel "c" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ") ", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "a", scanItem = LLabel "a" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ") ", scanItem = LParenthesisClose }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "+ ", scanItem = LPlus }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = "b", scanItem = LLabel "a" }
              , ScanItem { scanLoc = ("", (0, 0)), scanStr = ")", scanItem = LParenthesisClose }
              ]
        runPPParser translationUnitParser [inputLine]
          `shouldSatisfy` isLeft

      it "discards unterminated if statements" $ do
        let inputLines =
              [ map makeScanItem [LPPIf, LLabel "ifcondition"]
              , map makeScanItem [LReturn, LIntLiteral 1]
              ]
        runPPParser translationUnitParser inputLines
          `shouldSatisfy` isLeft

testConcatTokens :: Spec
testConcatTokens =
  describe "concatTokens" $ do
    it "replaces macro constans with relevant values" $ do
      let sourceCode      = "int foo ## bar = 1;"
          transformedCode = "int foobar = 1;"
      (scanCLineNoWS ("", (0, 0)) sourceCode >>= concatTokens)
        `shouldBe` scanCLineNoWS ("", (0, 0)) transformedCode

testMacroExpand :: Spec
testMacroExpand =
  describe "macroExpand" $ do
    it "replaces macro constans with relevant values" $ do
      let macroDict       = M.singleton "PI" $ MacroConstant "3.14"
          sourceCode      = "int area(int r) { return PI * r * r; }"
          transformedCode = "int area(int r) { return 3.14 * r * r; }"
      (scanCLineNoWS ("", (0, 0)) sourceCode >>= macroExpand ("", (0, 0)) macroDict)
        `shouldBe` scanCLineNoWS ("", (0, 0)) transformedCode

    it "replaces macro function invocations with relevant values" $ do
      let macroDict =
            M.fromList
              [ ( "PI", MacroConstant "3.14" )
              , ( "AREA"
                , MacroFunction
                    ["r"]
                    -- (PI * (r) * (r))
                    [ ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "PI ", scanItem = LLabel "PI" }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "* ", scanItem = LStar }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "r", scanItem = LLabel "r" }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = ") ", scanItem = LParenthesisClose }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "* ", scanItem = LStar }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "(", scanItem = LParenthesisOpen }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = "r", scanItem = LLabel "r" }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = ")", scanItem = LParenthesisClose }
                    , ScanItem { scanLoc = ("", (0, 0)), scanStr = ")", scanItem = LParenthesisClose }
                    ]
                )
              ]
          sourceCode      = "int area = AREA(x+y);"
          transformedCode = "int area = (3.14 * (x+y) * (x+y));"
      (scanCLineNoWS ("", (0, 0)) sourceCode >>= macroExpand ("", (0, 0)) macroDict)
        `shouldBe` scanCLineNoWS ("", (0, 0)) transformedCode

    it "it re-expands macros containing macro values" $ do
      let macroDict =
            M.fromList
              [ ("PI", MacroConstant "3.14")
              , ("DOUBLEPI", MacroConstant "2*PI")
              ]
          sourceCode      = "int x = DOUBLEPI;"
          transformedCode = "int x = 2*3.14;"
      (scanCLineNoWS ("", (0, 0)) sourceCode >>= macroExpand ("", (0, 0)) macroDict)
        `shouldBe` scanCLineNoWS ("", (0, 0)) transformedCode

testPPTransform :: Spec
testPPTransform = do
  describe "ppTransformError" $ do
    it "produces an error" $ do
      result <- ppTransformError
                  (Context "" 0 M.empty)
                  (PPErrorMacro
                    [ScanItem ("", (0, 1)) "errorMessage" $ LLabel "errorMessage"])
      result `shouldBe` Left (PreProcessError ("", (0, 1)) "errorMessage")

  describe "ppTransformLine" $ do
    it "sets the line number and file name" $ do
      result <- ppTransformLine
                  (Context "" 0 M.empty)
                  (PPLineMacro
                    [ ScanItem ("", (0, 1)) "10 " $ LIntLiteral 10
                    , ScanItem ("", (0, 4)) "foo" $ LLabel "foo"
                    , ScanItem ("", (0, 7)) "." LDot
                    , ScanItem ("", (0, 8)) "h" $ LLabel "h"
                    ])
      result `shouldBe` Right (Context "foo.h" 11 M.empty, [])

  describe "ppTransformInclude" $ do
    it "includes contents of specified file" $ do
      let expected =
            Context
              { fileName = "foo"
              , lineNum = 1
              , macroSymbols =
                  M.fromList
                    [ ("PI", MacroConstant "3.14")
                    , ("VARPREFIX", MacroConstant "foo")
                    , ( "MAKEVARNAME"
                      , MacroFunction
                          ["name"]
                          [ ScanItem { scanLoc = ("test/pptest.h", (3, 27))
                                     , scanStr = "VARPREFIX "
                                     , scanItem = LLabel "VARPREFIX" }
                          , ScanItem { scanLoc = ("test/pptest.h", (3, 37))
                                     , scanStr = "## "
                                     , scanItem = LPPConcat }
                          , ScanItem { scanLoc = ("test/pptest.h", (3, 40))
                                     , scanStr = "name"
                                     , scanItem = LLabel "name" }
                          ]
                      )
                    ]
              }
      result <- ppTransformInclude
                  (Context "foo" 0 M.empty)
                  (PPIncludeInternal "test/pptest.h")
      result `shouldBe` Right (expected, [])

testPreTransform :: Spec
testPreTransform = do
  describe "preTransform" $ do
    it "correctly transforms source code" $ do
      let sourceCode =
            unlines [ "??=include <stdio.h>"
                    , "// a comment \\"
                    , ""
                    , "int foo = 1; /* another comment */"
                    ]
          expected =
            [ [ ScanItem { scanLoc = ("", (1, 1)), scanStr = "#include ", scanItem = LPPInclude }
              , ScanItem { scanLoc = ("", (1, 10)), scanStr = "<", scanItem = LLT }
              , ScanItem { scanLoc = ("", (1, 11)), scanStr = "stdio", scanItem = LLabel "stdio" }
              , ScanItem { scanLoc = ("", (1, 16)), scanStr = ".", scanItem = LDot }
              , ScanItem { scanLoc = ("", (1, 17)), scanStr = "h", scanItem = LLabel "h" }
              , ScanItem { scanLoc = ("", (1, 18)), scanStr = ">", scanItem = LGT }
              ]
            , []
            , [ ScanItem { scanLoc = ("", (3, 1)), scanStr = "int ", scanItem = LInt }
              , ScanItem { scanLoc = ("", (3, 5)), scanStr = "foo ", scanItem = LLabel "foo" }
              , ScanItem { scanLoc = ("", (3, 9)), scanStr = "= ", scanItem = LAssign }
              , ScanItem { scanLoc = ("", (3, 11)), scanStr = "1", scanItem = LIntLiteral 1 }
              , ScanItem { scanLoc = ("", (3, 12)), scanStr = ";  ", scanItem = LSemiColon }
              ]
            ]

      preTransform "" sourceCode `shouldBe` Right expected

testPreProcessCode :: IO ()
testPreProcessCode = do
  withFile
    "test/pptest.c"
    ReadMode $
    \f -> do
      contents <- hGetContents f
      hspec $
        describe "preProcessCode" $ do
          it "correctly preprocesses a c source file" $ do
            let expected =
                  [ LInt, LLabel "foo1", LAssign, LIntLiteral 1, LSemiColon
                  , LInt, LLabel "foo4", LAssign, LIntLiteral 4, LSemiColon
                  , LInt, LLabel "foo6", LAssign, LIntLiteral 6, LSemiColon
                  ]
            ppResults <- preProcessCode "pptest.c" contents
            fmap (map scanItem) ppResults `shouldBe` Right expected
