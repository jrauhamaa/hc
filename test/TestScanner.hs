module TestScanner where

import Test.Hspec

import Data.Either
import System.IO

import Scanner
import Lexeme


testScanner :: IO ()
testScanner = do
  testFullSourceFile
  hspec testGoodInput
  hspec testBadInput

testFullSourceFile :: IO ()
testFullSourceFile = do
  withFile
    "test/hello.c"
    ReadMode $
    \f -> do
      contents <- hGetContents f
      hspec $
        describe "scanCCode" $ do
          context "when given valid input" $ do
            it "will scan a c source file" $ do
              scanCCode contents `shouldSatisfy` isRight

testGoodInput :: Spec
testGoodInput =
  describe "scanCCode" $ do
    context "when given valid input" $ do
      it "will recognize c keywords" $ do
        let keywordInput = "do while for if else switch case"
                           <> "default break return continue"
                           <> "register extern typedef volatile"
                           <> "void int float double char"
                           <> "unsigned signed short long"
                           <> "sizeof label"
            expectedResult = [ LDo, LWhile, LFor, LIf, LElse, LSwitch, LCase
                             , LDefault, LBreak, LReturn, LContinue
                             , LRegister, LExtern, LTypedef, LVolatile
                             , LVoid, LInt, LFloat, LDouble, LChar
                             , LUnsigned, LSigned, LShort, LLong
                             , LSizeof, LLabel "label", LEndMarker
                             ]
            results = scanCCode keywordInput
        results `shouldSatisfy` isRight
        ((filter (/= LWhiteSpace) . map scanItem) <$> results) `shouldBe` (Right expectedResult)

      it "will recognize preprocessor directives" $ do
        let input = "#define #undef #error #include #line #pragma"
                    <> "#if #ifdef #ifndef #elif #else #endif"
                    <> "# ##define"
            expected = [ LPPDefine, LPPUndef, LPPError, LPPInclude, LPPLine
                       , LPPPragma, LPPIf, LPPIfdef, LPPIfndef, LPPElif
                       , LPPElse, LPPEndif, LPPEmpty, LPPConcat
                       , LLabel "define", LEndMarker
                       ]
            results = scanCCode input
        results `shouldSatisfy` isRight
        ((filter (/= LWhiteSpace) . map scanItem) <$> results) `shouldBe` (Right expected)

      it "will recognize integer constants" $ do
        ((filter (/= LWhiteSpace) . map scanItem)
         <$> scanCLine (0, 0) "8 8u 8l 8lu 8ul 010 0x8")
         `shouldBe` (Right $ take 7 $ repeat $ LIntLiteral 8)

      it "will recognize char constants" $ do
        ((filter (/= LWhiteSpace) . map scanItem)
         <$> scanCLine (0, 0) "'a' L'a' ' ' '\\t' '\\\\' '\\n' '\\''")
         `shouldBe` (Right $ map LCharLiteral ['a', 'a', ' ', '\t', '\\', '\n', '\''])

      it "will recognize float constants" $ do
        ((filter (/= LWhiteSpace) . map scanItem)
         <$> scanCLine (0, 0) "1. -.1 1.1 1.1f 1f 1fl 1lf -5e2 5e-2")
         `shouldBe` (Right $ map LFloatLiteral [1.0, -0.1, 1.1, 1.1, 1.0, 1.0, 1.0, -500.0, 0.05])

      it "will recognize string literals" $ do
        ((map scanItem) <$> scanCLine (0, 0) "\"foo \\\\ \\n \\t \\\"\"")
         `shouldBe` (Right $ [LStringLiteral "foo \\ \n \t \""])


testBadInput :: Spec
testBadInput =
  describe "scanCCode" $ do
    context "when given bad input" $ do
      it "will not accept empty source file" $ do
        scanCCode "" `shouldSatisfy` isLeft


      it "will not accept unterminated string" $ do
        scanCCode "\"unterminated string" `shouldSatisfy` isLeft


