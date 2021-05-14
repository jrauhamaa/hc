{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Scanner where

import Control.Applicative
import Data.Char
import Numeric

import Lexeme (CLexeme(..))
import Utils (Filename, Location, Error(..))

-----------
-- TYPES --
-----------

data ScanItem a =
  ScanItem
    { scanLoc :: Location
    , scanStr :: String
    , scanItem :: a
    }
  deriving (Show, Eq)

type Input = (Location, String)

newtype Scanner a =
  Scanner
    { runScanner :: Input -> Either Error (Input, ScanItem a)
    }

instance Functor ScanItem where
  fmap fab sea =
    ScanItem
      { scanLoc = scanLoc sea
      , scanStr = scanStr sea
      , scanItem = fab $ scanItem sea
      }

instance Applicative ScanItem where
  pure a = ScanItem ("", (1, 1)) "" a
  seab <*> sea =
    ScanItem
      { scanLoc = scanLoc seab
      , scanStr = scanStr seab ++ scanStr sea
      , scanItem = scanItem seab $ scanItem sea
      }

instance Functor Scanner where
  fmap fab sa = Scanner $ (fmap . fmap . fmap) fab . runScanner sa

instance Applicative Scanner where
  pure a = Scanner $ \input@(l, _) -> Right (input, ScanItem l "" a)
  sab <*> sa =
    Scanner $ \input -> do
      (input', seab) <- runScanner sab input
      (input'', sea) <- runScanner sa input'
      return (input'', seab <*> sea)

instance Alternative Scanner where
  empty = Scanner $ const empty
  Scanner s1 <|> Scanner s2 =
    Scanner $ \input ->
      case s1 input of
        Left _ -> s2 input
        scan1@(Right (_, item1)) ->
          case s2 input of
            Left _ -> scan1
            scan2@(Right (_, item2)) ->
              if length (scanStr item1) >= length (scanStr item2)
                then scan1
                else scan2

------------------
-- MAIN SCANNER --
------------------

scanCLine :: Location -> String -> Either Error [ScanItem CLexeme]
scanCLine _ [] = return []
scanCLine c input =
  case runScanner cScanner (c, input) of
    Left e -> Left e
    Right ((c', input'), se) ->
      if input' == ""
        then return [se]
        else do
          lineTail <- scanCLine c' input'
          return $ se : lineTail

scanCCode :: Filename -> String -> Either Error [ScanItem CLexeme]
scanCCode fName input = scanCCode' [] ((fName, (1, 1)), input)

scanCCode' ::
     [ScanItem CLexeme] -> Input -> Either Error [ScanItem CLexeme]
scanCCode' scanned input =
  case runScanner cScanner input of
    Left e -> Left e
    Right (input', se) ->
      if snd input' == ""
        then Right $ scanned <> [se, ScanItem ("", (0, 0)) "" LEndMarker]
        else scanCCode' (scanned <> [se]) input'

toFilter :: [CLexeme]
toFilter = [LWhiteSpace, LComment]

filterWhiteSpace :: [ScanItem CLexeme] -> [ScanItem CLexeme]
filterWhiteSpace [] = []
filterWhiteSpace [item]
  | scanItem item `elem` toFilter = []
  | otherwise = [item]
filterWhiteSpace (item@(ScanItem c s a):item'@(ScanItem _ s' _):rest)
  | scanItem item `elem` toFilter = filterWhiteSpace $ item':rest
  | scanItem item' `elem` toFilter = filterWhiteSpace $ ScanItem c (s <> s') a : rest
  | otherwise = item : filterWhiteSpace (item' : rest)

cScanner :: Scanner CLexeme
cScanner
  =   lPPDefine
  <|> lPPUndef
  <|> lPPInclude
  <|> lPPIfdef
  <|> lPPIfndef
  <|> lPPIf
  <|> lPPElif
  <|> lPPElse
  <|> lPPEndif
  <|> lPPLine
  <|> lPPError
  <|> lPPPragma
  <|> lPPConcat
  <|> lPPEmpty
  -- begins with /
  <|> lCommentS
  <|> lDivisionAssignS
  <|> lDivisionS
  -- begins with do
  <|> lDoubleS
  <|> lDoS
  -- begins with <
  <|> lBitShiftLeftAssignS
  <|> lBitShiftLeftS
  <|> lLTES
  <|> lLTS
  -- begins with >
  <|> lBitShiftRightAssignS
  <|> lBitShiftRightS
  <|> lGTES
  <|> lGTS
  -- begins with number
  <|> lFloatLiteralS
  <|> lIntLiteralS
  -- begins with -
  <|> lMinusAssignS
  <|> lDecrementS
  <|> lArrowS
  <|> lMinusS
  -- begins with +
  <|> lPlusAssignS
  <|> lIncrementS
  <|> lPlusS
  -- begins with .
  <|> lVarargsS
  <|> lDotS
  -- begins with &
  <|> lAndS
  <|> lBitwiseAndAssignS
  <|> lAmpS
  -- begins with |
  <|> lOrS
  <|> lBitwiseOrAssignS
  <|> lBitwiseOrS
  -- begins with =
  <|> lEqualsS
  <|> lAssignS
  -- begins with !
  <|> lNotEqualsS
  <|> lNotS
  -- begins with ^
  <|> lBitwiseXorAssignS
  <|> lBitwiseXorS
  -- begins with *
  <|> lMultiplicationAssignS
  <|> lStarS
  -- begins with %
  <|> lModuloAssignS
  <|> lModuloS
  -- no conflicts from this point onwards
  <|> lAutoS
  <|> lBreakS
  <|> lCaseS
  <|> lCharS
  <|> lConstS
  <|> lContinueS
  <|> lDefaultS
  <|> lElseS
  <|> lEnumS
  <|> lExternS
  <|> lFloatS
  <|> lForS
  <|> lGotoS
  <|> lIfS
  <|> lIntS
  <|> lLongS
  <|> lRegisterS
  <|> lReturnS
  <|> lShortS
  <|> lSignedS
  <|> lSizeofS
  <|> lStaticS
  <|> lStructS
  <|> lSwitchS
  <|> lTypedefS
  <|> lUnionS
  <|> lUnsignedS
  <|> lVoidS
  <|> lVolatileS
  <|> lWhileS
  <|> lParenthesisOpenS
  <|> lParenthesisCloseS
  <|> lBracketOpenS
  <|> lBracketCloseS
  <|> lBraceOpenS
  <|> lBraceCloseS
  <|> lBitwiseNotS
  <|> lCommaS
  <|> lColonS
  <|> lSemiColonS
  <|> lTernaryS
  <|> lCharLiteralS
  <|> lStringLiteralS
  <|> lWhiteSpaceS
  -- this must be last
  <|> lLabelS

-----------
-- UTILS --
-----------

emptyInputError :: Location -> Error
emptyInputError location = ScanError location "Unexpected end of input"

unexpectedInputError :: String -> String -> Location -> Error
unexpectedInputError expected encountered location =
  ScanError location $
  mconcat
    [ "Unexpected input: '"
    , encountered
    , "'. Expected '"
    , expected
    , "' instead."
    ]

nextLoc :: Char -> Location -> Location
nextLoc '\n' (fName, (r, _)) = (fName, (r + 1, 1))
nextLoc _ (fName, (r, c)) = (fName, (r, c + 1))

charSIgnoreCase :: Char -> Scanner Char
charSIgnoreCase c =
  Scanner $ \case
    (l, "") -> Left $ emptyInputError l
    (l, c':rest) ->
      if toLower c == toLower c'
        then Right ((nextLoc c l, rest), ScanItem l (c:"") c)
        else Left $ unexpectedInputError [c] [c'] l

charS :: Char -> Scanner Char
charS c =
  Scanner $ \case
    (l, "") -> Left $ emptyInputError l
    (l, c':rest) ->
      if c == c'
        then Right ((nextLoc c l, rest), ScanItem l (c:"") c)
        else Left $ unexpectedInputError [c] [c'] l

stringSIgnoreCase :: String -> Scanner String
stringSIgnoreCase = traverse charSIgnoreCase

stringS :: String -> Scanner String
stringS = traverse charS

scanIf :: (Char -> Bool) -> Scanner Char
scanIf f =
  Scanner $ \case
    (l, "") -> Left $ emptyInputError l
    (l, c:s) ->
      if f c
        then Right ((nextLoc c l, s), ScanItem l (c:"") c)
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
      Left _ -> Right (input, ScanItem (fst input) "" [])
      a -> (fmap . fmap) (: []) <$> a

optionalStringS :: Scanner String -> Scanner String
optionalStringS s =
  Scanner $ \input ->
    case runScanner s input of
      Left _ -> Right (input, ScanItem (fst input) "" [])
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

octalDigit :: String
octalDigit = ['0' .. '7']

whiteSpace :: String
whiteSpace = " \t\n\r"

hexDigits :: String
hexDigits = ['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F']

escapeCharOctal :: Scanner Char
escapeCharOctal =
  fmap (chr . fst . head . readOct) . (:) <$>
  (stringS "\\0" *> scanIf (`elem` octalDigit)) <*>
  spanS (`elem` octalDigit)

escapeCharHex :: Scanner Char
escapeCharHex =
  fmap (chr . fst . head . readHex) . (:) <$>
  (stringS "\\x" *> scanIf (`elem` hexDigits)) <*>
  spanS (`elem` hexDigits)

escapeS :: Scanner Char
escapeS = escapeChar <|> escapeCharOctal <|> escapeCharHex

------------------
-- PREPROCESSOR --
------------------

ppS :: String -> Scanner ()
ppS s = () <$ charS '#'
           <* spanS (`elem` whiteSpace)
           <* stringS s

lPPDefine :: Scanner CLexeme
lPPDefine = LPPDefine <$ ppS "define"

lPPUndef :: Scanner CLexeme
lPPUndef = LPPUndef <$ ppS "undef"

lPPInclude :: Scanner CLexeme
lPPInclude = LPPInclude <$ ppS "include"

lPPIf :: Scanner CLexeme
lPPIf = LPPIf <$ ppS "if"

lPPIfdef :: Scanner CLexeme
lPPIfdef = LPPIfdef <$ ppS "ifdef"

lPPIfndef :: Scanner CLexeme
lPPIfndef = LPPIfndef <$ ppS "ifndef"

lPPElif :: Scanner CLexeme
lPPElif = LPPElif <$ ppS "elif"

lPPElse :: Scanner CLexeme
lPPElse = LPPElse <$ ppS "else"

lPPEndif :: Scanner CLexeme
lPPEndif = LPPEndif <$ ppS "endif"

lPPLine :: Scanner CLexeme
lPPLine = LPPLine <$ ppS "line"

lPPError :: Scanner CLexeme
lPPError = LPPError <$ ppS "error"

lPPPragma :: Scanner CLexeme
lPPPragma = LPPPragma <$ ppS "pragma"

lPPConcat :: Scanner CLexeme
lPPConcat = LPPConcat <$ stringS "##"

lPPEmpty :: Scanner CLexeme
lPPEmpty = LPPEmpty <$ charS '#'

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

----------------
-- WHITESPACE --
----------------
lWhiteSpaceS :: Scanner CLexeme
lWhiteSpaceS = LWhiteSpace <$ spanOneOrMoreS (`elem` whiteSpace)

--------------
-- COMMENTS --
--------------

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
        then Right
               ( (l, c1 : c2 : rest)
               , ScanItem
                   { scanLoc = l
                   , scanStr = previouslyScanned
                   , scanItem = previouslyScanned
                   })
        else runScanner
               (multiLineCommentS' (previouslyScanned <> [c1]))
               (nextLoc c1 l, c2 : rest)
    (l, _) -> Left $ emptyInputError l

----------------
-- IDENTIFIER --
----------------

lLabelS :: Scanner CLexeme
lLabelS =
  LLabel <$>
  liftA2 (:) (scanIf (`elem` nonNumeric)) (spanS (`elem` alphaNumeric))

--------------------
-- STRING LITERAL --
--------------------

-- /L?"([^\n"\\]|(\\[afnrtv'"\\]))*"/
lStringLiteralS :: Scanner CLexeme
lStringLiteralS =
  LStringLiteral <$>
  (optionalCharS (charS 'L') *> charS '"' *>
   many (scanIf (`notElem` "\n\"\\") <|> escapeS) <*
   charS '"')

-----------------
-- INT LITERAL --
-----------------

integerSuffixS :: Scanner String
integerSuffixS =
  liftA2 (:) (scanIf (`elem` "Uu")) (optionalCharS $ scanIf (`elem` "Ll")) <|>
  liftA2 (:) (scanIf (`elem` "Ll")) (optionalCharS $ scanIf (`elem` "Uu"))

lIntLiteralDecimalS :: Scanner CLexeme
lIntLiteralDecimalS =
  LIntLiteral . fst . head . readDec <$>
  liftA2
    (:)
    (scanIf (`elem` nonZeroDecimalDigit))
    (spanS (`elem` decimalDigit) <* optionalStringS integerSuffixS)

lIntLiteralOctalS :: Scanner CLexeme
lIntLiteralOctalS =
  LIntLiteral . fst . head . readOct <$>
  liftA2 (:) (charS '0') (spanS (`elem` octalDigit))

lIntLiteralHexS :: Scanner CLexeme
lIntLiteralHexS =
  LIntLiteral . fst . head . readHex <$>
  (stringS "0x" *> spanOneOrMoreS (`elem` hexDigit))

lIntLiteralS :: Scanner CLexeme
lIntLiteralS = lIntLiteralHexS <|> lIntLiteralOctalS <|> lIntLiteralDecimalS

------------------
-- CHAR LITERAL --
------------------

lCharLiteralS :: Scanner CLexeme
lCharLiteralS =
  LCharLiteral . head <$>
  (optionalCharS (charS 'L') *> charS '\'' *>
   many (scanIf (`notElem` "\\\n'") <|> escapeS) <*
   charS '\'')

-------------------
-- FLOAT LITERAL --
-------------------

readFloatLiteral :: String -> String -> String -> Double
readFloatLiteral s digits exponentPart = sign * read (cleanDigits <> exponentPart)
  where
    zeroPadded = '0' : dropWhile (== '+') digits
    sign = if s == "-" then -1 else 1
    cleanDigits =
      if last zeroPadded == '.'
        then zeroPadded <> "0"
        else zeroPadded

floatSuffixS :: Scanner Char
floatSuffixS = scanIf (`elem` "fFlL")

fractionalConstantS' :: Scanner String
fractionalConstantS' =
  liftA3
    (\s1 c s2 -> s1 <> [c] <> s2)
    (spanS (`elem` decimalDigit))
    (charS '.')
    (spanOneOrMoreS (`elem` decimalDigit))

fractionalConstantS'' :: Scanner String
fractionalConstantS'' =
  liftA2
    (\s c -> s <> [c])
    (spanOneOrMoreS (`elem` decimalDigit))
    (charS '.')

fractionalConstantS :: Scanner String
fractionalConstantS = fractionalConstantS' <|> fractionalConstantS''

exponentPartS :: Scanner String
exponentPartS =
  liftA3
    (\c s1 s2 -> [c] <> s1 <> s2)
    (scanIf (`elem` "eE"))
    (optionalCharS $ scanIf (`elem` "+-"))
    (spanOneOrMoreS (`elem` decimalDigit))

-- a number with decimal dot + optional exponent part
lFloatLiteralS' :: Scanner CLexeme
lFloatLiteralS' =
  LFloatLiteral <$>
  liftA3
     readFloatLiteral
     (optionalCharS $ scanIf (`elem` "+-")  )
     fractionalConstantS
     (optionalStringS exponentPartS <* optionalCharS floatSuffixS)

-- one or more digits + exponent part
lFloatLiteralS'' :: Scanner CLexeme
lFloatLiteralS'' =
  LFloatLiteral <$>
  liftA3
     readFloatLiteral
     (optionalCharS $ scanIf (`elem` "+-"))
     (spanOneOrMoreS (`elem` decimalDigit))
     (exponentPartS <* optionalCharS floatSuffixS)

-- integer followed by f lf or fl
lFloatLiteralS''' :: Scanner CLexeme
lFloatLiteralS''' =
  LFloatLiteral <$>
  liftA2
    (\sign digits -> readFloatLiteral sign digits "")
    (optionalCharS $ scanIf (`elem` "+-"))
    (spanOneOrMoreS (`elem` decimalDigit)
       <* (stringSIgnoreCase "fl"
           <|> stringSIgnoreCase "f"
           <|> stringSIgnoreCase "lf"))

lFloatLiteralS :: Scanner CLexeme
lFloatLiteralS = lFloatLiteralS' <|> lFloatLiteralS'' <|> lFloatLiteralS'''
