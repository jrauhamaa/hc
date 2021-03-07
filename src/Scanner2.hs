{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Scanner2 where

import Control.Applicative
import Data.Char
import Numeric

import Lexeme (CLexeme(..))

-----------
-- TYPES --
-----------
type Row = Int

type Col = Int

type Coordinates = (Row, Col)

data ScanError =
  ScanError
    { errorLoc :: Coordinates
    , errorMsg :: String
    }
  deriving (Show)

data ScanElement a =
  ScanElement
    { loc :: Coordinates
    , result :: a
    }
  deriving (Show, Eq)

type Input = (Coordinates, String)

newtype Scanner a =
  Scanner
    { runScanner :: Input -> Either ScanError (Input, ScanElement a)
    }

instance Functor ScanElement where
  fmap fab sea = ScanElement (loc sea) (fab $ result sea)

instance Applicative ScanElement where
  pure a = ScanElement (1, 1) a
  seab <*> sea = ScanElement (loc seab) (result seab $ result sea)

instance Functor Scanner where
  fmap fab sa = Scanner $ (fmap . fmap . fmap) fab . runScanner sa

instance Applicative Scanner where
  pure a = Scanner $ \input@(l, s) -> Right (input, ScanElement l a)
  sab <*> sa =
    Scanner $ \input -> do
      (input', seab) <- runScanner sab input
      (input'', sea) <- runScanner sa input'
      return (input'', seab <*> sea)

instance Alternative (Either ScanError) where
  empty = Left $ ScanError (1, 1) "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Scanner where
  empty = Scanner $ const empty
  Scanner s1 <|> Scanner s2 = Scanner $ \input -> s1 input <|> s2 input

scanCCode :: String -> Either ScanError [ScanElement CLexeme]
scanCCode input = scanCCode' [] ((1, 1), input)

scanCCode' :: [ScanElement CLexeme] -> Input -> Either ScanError [ScanElement CLexeme]
scanCCode' scanned input =
  case runScanner cScanner input of
    Left e -> Left e
    Right (input', se) ->
      if snd input' == ""
        then Right $ scanned <> [se]
        else scanCCode' (scanned <> [se]) input'


cScanner :: Scanner CLexeme
cScanner =
  lCommentS <|> lAutoS <|> lBreakS <|> lCaseS <|> lCharS <|> lConstS <|>
  lContinueS <|>
  lDefaultS <|>
  lDoS <|>
  lDoubleS <|>
  lElseS <|>
  lEnumS <|>
  lExternS <|>
  lFloatS <|>
  lForS <|>
  lGotoS <|>
  lIfS <|>
  lIntS <|>
  lLongS <|>
  lRegisterS <|>
  lReturnS <|>
  lShortS <|>
  lSignedS <|>
  lSizeofS <|>
  lStaticS <|>
  lStructS <|>
  lSwitchS <|>
  lTypedefS <|>
  lUnionS <|>
  lUnsignedS <|>
  lVoidS <|>
  lVolatileS <|>
  lWhileS <|>
  lParenthesisOpenS <|>
  lParenthesisCloseS <|>
  lBracketOpenS <|>
  lBracketCloseS <|>
  lBraceOpenS <|>
  lBraceCloseS <|>
  lNotEqualsS <|>
  lNotS <|>
  lEqualsS <|>
  lLTES <|>
  lLTS <|>
  lGTES <|>
  lGTS <|>
  lAndS <|>
  lOrS <|>
  lArrowS <|>
  lAssignS <|>
  lModuloAssignS <|>
  lMultiplicationAssignS <|>
  lPlusAssignS <|>
  lMinusAssignS <|>
  lDivisionAssignS <|>
  lBitwiseAndAssignS <|>
  lBitwiseOrAssignS <|>
  lBitShiftLeftAssignS <|>
  lBitShiftRightAssignS <|>
  lBitwiseXorAssignS <|>
  lModuloS <|>
  lStarS <|>
  lIncrementS <|>
  lPlusS <|>
  lDecrementS <|>
  lMinusS <|>
  lDivisionS <|>
  lAmpS <|>
  lBitwiseOrS <|>
  lBitShiftLeftS <|>
  lBitShiftRightS <|>
  lBitwiseXorS <|>
  lBitwiseNotS <|>
  lCommaS <|>
  lVarargsS <|>
  lDotS <|>
  lColonS <|>
  lSemiColonS <|>
  lTernaryS <|>
  lCharLiteralS <|>
  lFloatLiteralS <|>
  lIntLiteralS <|>
  lStringLiteralS <|>
  lLabelS <|>
  lWhiteSpaceS

-----------
-- UTILS --
-----------
emptyInputError :: Coordinates -> ScanError
emptyInputError location = ScanError location "Unexpected end of input"

unexpectedInputError :: String -> String -> Coordinates -> ScanError
unexpectedInputError expected encountered location =
  ScanError location $
  mconcat
    [ "Unexpected input: '"
    , encountered
    , "'. Expected '"
    , expected
    , "' instead."
    ]

nextLoc :: Char -> Coordinates -> Coordinates
nextLoc '\n' l = ((fst l) + 1, 1)
nextLoc _ l = (fst l, (snd l) + 1)

charS :: Char -> Scanner Char
charS c =
  Scanner $ \case
    (l, "") -> Left $ emptyInputError l
    (l, c':rest) ->
      if c == c'
        then Right (((nextLoc c l), rest), ScanElement (nextLoc c l) c)
        else Left $ unexpectedInputError [c] [c'] l

stringS :: String -> Scanner String
stringS = traverse charS

scanIf :: (Char -> Bool) -> Scanner Char
scanIf f =
  Scanner $ \case
    (l, "") -> Left $ emptyInputError l
    (l, c:s) ->
      if f c
        then Right ((nextLoc c l, s), ScanElement (nextLoc c l) c)
        else Left $ ScanError l "Unexpected input."

spanS :: (Char -> Bool) -> Scanner String
spanS = many . scanIf

spanOneOrMoreS :: (Char -> Bool) -> Scanner String
spanOneOrMoreS f = (:) <$> scanIf f <*> spanS f

-- if fail, scan successfully empty string
optionalCharS :: Scanner a -> Scanner [a]
optionalCharS s =
  Scanner $ \input ->
    case runScanner s input of
      Left _ -> Right (input, ScanElement (fst input) [])
      a -> (fmap . fmap) (: []) <$> a

optionalStringS :: Scanner String -> Scanner String
optionalStringS s =
  Scanner $ \input ->
    case runScanner s input of
      Left _ -> Right (input, ScanElement (fst input) [])
      a -> a

escapeChar :: Scanner Char
escapeChar =
  ('\a' <$ stringS "\\a") <|> ('\b' <$ stringS "\\b") <|>
  ('\f' <$ stringS "\\f") <|>
  ('\n' <$ stringS "\\n") <|>
  ('\r' <$ stringS "\\r") <|>
  ('\t' <$ stringS "\\t") <|>
  ('\v' <$ stringS "\\v") <|>
  ('\'' <$ stringS "\\'") <|>
  ('"' <$ stringS "\\\"") <|>
  ('\\' <$ stringS "\\\\")

octalDigit :: String
octalDigit = ['0' .. '7']

escapeCharOctal :: Scanner Char
escapeCharOctal =
  fmap (chr . fst . head . readOct) . (:) <$>
  (stringS "\\0" *> scanIf (`elem` octalDigit)) <*>
  spanS (`elem` octalDigit)

hexDigits :: String
hexDigits = ['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F']

escapeCharHex :: Scanner Char
escapeCharHex =
  fmap (chr . fst . head . readHex) . (:) <$>
  (stringS "\\x" *> scanIf (`elem` hexDigits)) <*>
  spanS (`elem` hexDigits)

escapeS :: Scanner Char
escapeS = escapeChar <|> escapeCharOctal <|> escapeCharHex

----------------------
-- KEYWORD SCANNERS --
----------------------
lAutoS :: Scanner CLexeme
lAutoS = LAuto <$ stringS "auto"

lBreakS :: Scanner CLexeme
lBreakS = LBreak <$ stringS "break"

lCaseS :: Scanner CLexeme
lCaseS = LCase <$ stringS "case"

lCharS :: Scanner CLexeme
lCharS = LChar <$ stringS "char"

lConstS :: Scanner CLexeme
lConstS = LConst <$ stringS "const"

lContinueS :: Scanner CLexeme
lContinueS = LContinue <$ stringS "continue"

lDefaultS :: Scanner CLexeme
lDefaultS = LDefault <$ stringS "default"

lDoS :: Scanner CLexeme
lDoS = LDo <$ stringS "do"

lDoubleS :: Scanner CLexeme
lDoubleS = LDouble <$ stringS "double"

lElseS :: Scanner CLexeme
lElseS = LElse <$ stringS "else"

lEnumS :: Scanner CLexeme
lEnumS = LEnum <$ stringS "enum"

lExternS :: Scanner CLexeme
lExternS = LExtern <$ stringS "extern"

lFloatS :: Scanner CLexeme
lFloatS = LFloat <$ stringS "float"

lForS :: Scanner CLexeme
lForS = LFor <$ stringS "for"

lGotoS :: Scanner CLexeme
lGotoS = LGoto <$ stringS "goto"

lIfS :: Scanner CLexeme
lIfS = LIf <$ stringS "if"

lIntS :: Scanner CLexeme
lIntS = LInt <$ stringS "int"

lLongS :: Scanner CLexeme
lLongS = LLong <$ stringS "long"

lRegisterS :: Scanner CLexeme
lRegisterS = LRegister <$ stringS "register"

lReturnS :: Scanner CLexeme
lReturnS = LReturn <$ stringS "return"

lShortS :: Scanner CLexeme
lShortS = LShort <$ stringS "short"

lSignedS :: Scanner CLexeme
lSignedS = LSigned <$ stringS "signed"

lSizeofS :: Scanner CLexeme
lSizeofS = LSizeof <$ stringS "sizeof"

lStaticS :: Scanner CLexeme
lStaticS = LStatic <$ stringS "static"

lStructS :: Scanner CLexeme
lStructS = LStruct <$ stringS "struct"

lSwitchS :: Scanner CLexeme
lSwitchS = LSwitch <$ stringS "switch"

lTypedefS :: Scanner CLexeme
lTypedefS = LTypedef <$ stringS "typedef"

lUnionS :: Scanner CLexeme
lUnionS = LUnion <$ stringS "union"

lUnsignedS :: Scanner CLexeme
lUnsignedS = LUnsigned <$ stringS "unsigned"

lVoidS :: Scanner CLexeme
lVoidS = LVoid <$ stringS "void"

lVolatileS :: Scanner CLexeme
lVolatileS = LVolatile <$ stringS "volatile"

lWhileS :: Scanner CLexeme
lWhileS = LWhile <$ stringS "while"

lParenthesisOpenS :: Scanner CLexeme
lParenthesisOpenS = LParenthesisOpen <$ charS '('

lParenthesisCloseS :: Scanner CLexeme
lParenthesisCloseS = LParenthesisClose <$ charS ')'

lBracketOpenS :: Scanner CLexeme
lBracketOpenS = LBracketOpen <$ charS '['

lBracketCloseS :: Scanner CLexeme
lBracketCloseS = LBracketClose <$ charS ']'

lBraceOpenS :: Scanner CLexeme
lBraceOpenS = LBraceOpen <$ charS '{'

lBraceCloseS :: Scanner CLexeme
lBraceCloseS = LBraceClose <$ charS '}'

lNotS :: Scanner CLexeme
lNotS = LNot <$ charS '!'

lEqualsS :: Scanner CLexeme
lEqualsS = LEquals <$ stringS "=="

lNotEqualsS :: Scanner CLexeme
lNotEqualsS = LNotEquals <$ stringS "!="

lLTS :: Scanner CLexeme
lLTS = LLT <$ charS '<'

lLTES :: Scanner CLexeme
lLTES = LLTE <$ stringS "<="

lGTS :: Scanner CLexeme
lGTS = LGT <$ charS '>'

lGTES :: Scanner CLexeme
lGTES = LGTE <$ stringS ">="

lAndS :: Scanner CLexeme
lAndS = LAnd <$ stringS "&&"

lOrS :: Scanner CLexeme
lOrS = LOr <$ stringS "||"

lModuloS :: Scanner CLexeme
lModuloS = LModulo <$ charS '%'

lStarS :: Scanner CLexeme
lStarS = LStar <$ charS '*'

lPlusS :: Scanner CLexeme
lPlusS = LPlus <$ charS '+'

lMinusS :: Scanner CLexeme
lMinusS = LMinus <$ charS '-'

lDivisionS :: Scanner CLexeme
lDivisionS = LDivision <$ charS '/'

lAmpS :: Scanner CLexeme
lAmpS = LAmp <$ charS '&'

lBitwiseOrS :: Scanner CLexeme
lBitwiseOrS = LBitwiseOr <$ charS '|'

lBitShiftLeftS :: Scanner CLexeme
lBitShiftLeftS = LBitShiftLeft <$ stringS "<<"

lBitShiftRightS :: Scanner CLexeme
lBitShiftRightS = LBitShiftRight <$ stringS ">>"

lBitwiseXorS :: Scanner CLexeme
lBitwiseXorS = LBitwiseXor <$ charS '^'

lBitwiseNotS :: Scanner CLexeme
lBitwiseNotS = LBitwiseNot <$ charS '~'

lAssignS :: Scanner CLexeme
lAssignS = LAssign <$ charS '='

lModuloAssignS :: Scanner CLexeme
lModuloAssignS = LModuloAssign <$ stringS "%="

lMultiplicationAssignS :: Scanner CLexeme
lMultiplicationAssignS = LMultiplicationAssign <$ stringS "*="

lPlusAssignS :: Scanner CLexeme
lPlusAssignS = LPlusAssign <$ stringS "+="

lMinusAssignS :: Scanner CLexeme
lMinusAssignS = LMinusAssign <$ stringS "-="

lDivisionAssignS :: Scanner CLexeme
lDivisionAssignS = LDivisionAssign <$ stringS "/="

lBitwiseAndAssignS :: Scanner CLexeme
lBitwiseAndAssignS = LBitwiseAndAssign <$ stringS "&="

lBitwiseOrAssignS :: Scanner CLexeme
lBitwiseOrAssignS = LBitwiseOrAssign <$ stringS "|="

lBitShiftLeftAssignS :: Scanner CLexeme
lBitShiftLeftAssignS = LBitShiftLeftAssign <$ stringS "<<="

lBitShiftRightAssignS :: Scanner CLexeme
lBitShiftRightAssignS = LBitShiftRightAssign <$ stringS ">>="

lBitwiseXorAssignS :: Scanner CLexeme
lBitwiseXorAssignS = LBitwiseXorAssign <$ stringS "^="

lIncrementS :: Scanner CLexeme
lIncrementS = LIncrement <$ stringS "++"

lDecrementS :: Scanner CLexeme
lDecrementS = LDecrement <$ stringS "++"

lCommaS :: Scanner CLexeme
lCommaS = LComma <$ charS ','

lArrowS :: Scanner CLexeme
lArrowS = LArrow <$ stringS "->"

lDotS :: Scanner CLexeme
lDotS = LDot <$ charS '.'

lVarargsS :: Scanner CLexeme
lVarargsS = LVarargs <$ stringS "..."

lColonS :: Scanner CLexeme
lColonS = LColon <$ charS ':'

lSemiColonS :: Scanner CLexeme
lSemiColonS = LSemiColon <$ charS ';'

lTernaryS :: Scanner CLexeme
lTernaryS = LTernary <$ charS '?'

--------------------
-- OTHER SCANNERS --
--------------------
whiteSpaceChars :: String
whiteSpaceChars = " \t\n\r"

lWhiteSpaceS :: Scanner CLexeme
lWhiteSpaceS =
  LWhiteSpace <$
  (scanIf (`elem` whiteSpaceChars) *> spanS (`elem` whiteSpaceChars))

lCommentS :: Scanner CLexeme
lCommentS = singleCommentS <|> multiLineCommentS

singleCommentS :: Scanner CLexeme
singleCommentS = LComment <$ (stringS "//" *> spanS (/= '\n'))

multiLineCommentS :: Scanner CLexeme
multiLineCommentS =
  LComment <$ (stringS "/*" *> multiLineCommentS' "" <* stringS "*/")

multiLineCommentS' :: String -> Scanner String
multiLineCommentS' previouslyScanned =
  Scanner $ \case
    (l, c1:c2:rest) ->
      if [c1, c2] == "*/"
        then Right $ ((l, c1 : c2 : rest), ScanElement l previouslyScanned)
        else runScanner
               (multiLineCommentS' (previouslyScanned <> [c1]))
               (nextLoc c1 l, c2 : rest)
    (l, _) -> Left $ emptyInputError l

nonNumeric :: String
nonNumeric = ['a' .. 'z'] <> ['A' .. 'Z'] <> "_"

nonZeroDecimalDigit :: String
nonZeroDecimalDigit = ['1' .. '9']

decimalDigit :: String
decimalDigit = '0' : nonZeroDecimalDigit

hexDigit :: String
hexDigit = decimalDigit <> ['a' .. 'f'] <> ['A' .. 'F']

alphaNumeric :: String
alphaNumeric = decimalDigit <> nonNumeric

lLabelS :: Scanner CLexeme
lLabelS =
  LLabel <$>
  ((:) <$> scanIf (`elem` nonNumeric) <*> spanS (`elem` alphaNumeric))

-- /L?"([^\n"\\]|(\\[afnrtv'"\\]))*"/
lStringLiteralS :: Scanner CLexeme
lStringLiteralS =
  LStringLiteral <$>
  (optionalCharS (charS 'L') *> charS '"' *>
   many (scanIf (`notElem` "\n\"\\") <|> escapeS) <*
   charS '"')

integerSuffixS :: Scanner String
integerSuffixS =
  ((:) <$> scanIf (`elem` "Uu") <*> optionalCharS (scanIf (`elem` "Ll"))) <|>
  ((:) <$> scanIf (`elem` "Ll") <*> optionalCharS (scanIf (`elem` "Uu")))

lIntLiteralDecimalS :: Scanner CLexeme
lIntLiteralDecimalS =
  LIntLiteral . fst . head . readDec <$>
  ((:) <$> scanIf (`elem` nonZeroDecimalDigit) <*>
   (spanS (`elem` decimalDigit) <* optionalStringS integerSuffixS))

lIntLiteralOctalS :: Scanner CLexeme
lIntLiteralOctalS =
  LIntLiteral . fst . head . readOct <$>
  ((:) <$> charS '0' <*> spanS (`elem` octalDigit))

lIntLiteralHexS :: Scanner CLexeme
lIntLiteralHexS =
  LIntLiteral . fst . head . readHex <$>
  (stringS "0x" *> spanOneOrMoreS (`elem` hexDigit))

lIntLiteralS :: Scanner CLexeme
lIntLiteralS = lIntLiteralDecimalS <|> lIntLiteralOctalS <|> lIntLiteralHexS

lCharLiteralS :: Scanner CLexeme
lCharLiteralS =
  LCharLiteral . head <$>
  (optionalCharS (charS 'L') *> charS '\'' *>
   many (scanIf (`notElem` "\\\n'") <|> escapeS) <*
   charS '\'')

floatSuffixS :: Scanner Char
floatSuffixS = scanIf (`elem` "fFlL")

fractionalConstantS' :: Scanner String
fractionalConstantS' =
  (\s1 c s2 -> s1 <> [c] <> s2) <$> spanS (`elem` decimalDigit) <*> charS '.' <*>
  spanOneOrMoreS (`elem` decimalDigit)

fractionalConstantS'' :: Scanner String
fractionalConstantS'' =
  (\s c -> s <> [c]) <$> spanOneOrMoreS (`elem` decimalDigit) <*> charS '.'

fractionalConstantS :: Scanner String
fractionalConstantS = fractionalConstantS' <|> fractionalConstantS''

exponentPartS :: Scanner String
exponentPartS =
  (\c s1 s2 -> [c] <> s1 <> s2) <$> scanIf (`elem` "eE") <*>
  optionalCharS (scanIf (`elem` "+-")) <*>
  spanOneOrMoreS (`elem` decimalDigit)

readFloatLiteral :: String -> String -> Double
readFloatLiteral digits exponentPart = read (cleanDigits <> exponentPart)
  where
    zeroPadded = '0' : digits
    cleanDigits =
      if last zeroPadded == '.'
        then zeroPadded <> "0"
        else zeroPadded

lFloatLiteralS' :: Scanner CLexeme
lFloatLiteralS' =
  LFloatLiteral <$>
  (readFloatLiteral <$> fractionalConstantS <*>
   (optionalStringS exponentPartS <* optionalCharS floatSuffixS))

lFloatLiteralS'' :: Scanner CLexeme
lFloatLiteralS'' =
  LFloatLiteral <$>
  (readFloatLiteral <$> spanOneOrMoreS (`elem` decimalDigit) <*>
   (exponentPartS <* optionalCharS floatSuffixS))

lFloatLiteralS :: Scanner CLexeme
lFloatLiteralS = lFloatLiteralS' <|> lFloatLiteralS''
