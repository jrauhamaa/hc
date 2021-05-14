module TestParser where

import Test.Hspec

import Data.Either
import System.IO

import Parser
import ParseItem
import Lexeme
import Scanner (scanCCode, ScanItem(..), filterWhiteSpace)

testParser :: IO ()
testParser = do
  testFullSourceFile
  hspec testBadInput
  hspec testGoodInput

testFullSourceFile :: IO ()
testFullSourceFile = do
  withFile
    "test/hello.c"
    ReadMode $
    \f -> do
      contents <- hGetContents f
      hspec $
        describe "parseCCode" $ do
          context "when given valid input" $ do
            it "parses a c source file" $ do
              -- drop first 6 tokens (an include directive)
              (scanCCode "" contents >>= parseCCode . drop 6 . filterWhiteSpace ) `shouldSatisfy` isRight

unterminatedBlock :: [ScanItem CLexeme]
unterminatedBlock =
  map
   (\item -> ScanItem { scanLoc = ("", (0, 0)), scanStr = "", scanItem = item })
   [ LBraceOpen
   , LReturn
   , LIntLiteral 0
   , LSemiColon
   ]

testBadInput :: Spec
testBadInput =
  describe "parseCCode" $ do
    context "when given bad input" $ do
      it "doesn't accept empty source file" $ do
        parseCCode [ScanItem { scanLoc = ("", (0, 0)), scanStr = "", scanItem = LEndMarker }]
          `shouldSatisfy` isLeft

      it "doesn't accept unterminated block expression" $ do
        runParser cCompoundStatementP unterminatedBlock `shouldSatisfy` isLeft

testDeclarationInput :: [ScanItem CLexeme]
testDeclarationInput =
  map
   (\item -> ScanItem { scanLoc = ("", (0, 0)), scanStr = "", scanItem = item })
   [ LLabel "typedefname"
   , LLabel "variablename"
   , LSemiColon
   ]

pItem :: ParseItem Int
pItem =
  ParseItem
    { parseLoc = ("", (0, 0))
    , symbolTable = initialSymbols
    , parseItem = 1
    }

declarationParse :: ParseItem CDeclaration
declarationParse =
  pItem {
    parseItem =
     CDeclaration
       pItem {
         parseItem =
           CDeclarationSpecifiersTypeSpecifier
             pItem {
               parseItem =
                 CTypeSpecifierTypedef
                   pItem {
                     parseItem =
                       CTypedefName
                         pItem { parseItem = CIdentifier "typedefname" }}}
           pItem { parseItem = CDeclarationSpecifiersOptionalEmpty }}
       pItem {
         parseItem =
           CInitDeclaratorListOptional
             pItem {
               parseItem =
                 CInitDeclaratorList
                   pItem {
                     parseItem =
                       CInitDeclarator
                         pItem {
                           parseItem =
                             CDeclarator
                               pItem { parseItem = CPointerOptionalEmpty }
                               pItem {
                                 parseItem =
                                   CDirectDeclaratorId
                                     pItem {
                                       parseItem = CIdentifier "variablename" }
                                     pItem {
                                       parseItem = CDirectDeclarator'Empty }}}
                         pItem {
                           parseItem =
                             CAssignInitializerOptionalEmpty }}
                   pItem { parseItem = CInitDeclaratorList'Empty }}}}

testGoodInput :: Spec
testGoodInput =
  describe "parseCCode" $ do
    context "when given good input" $ do
      it "parses a block expression" $ do
        runParser
          cCompoundStatementP
          (unterminatedBlock ++
            [ScanItem { scanLoc = ("", (0, 0))
                      , scanStr = ""
                      , scanItem = LBraceClose }])
          `shouldSatisfy` isRight

      it "parses a declaration" $ do
        runParser cDeclarationP testDeclarationInput
          `shouldBe` Right (testDeclarationInput, [], declarationParse)


