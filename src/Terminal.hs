module Terminal
  ( terminals
  , CTerminal(..)
  ) where

import Data.Char
import Data.Tuple
import Numeric

-- as described in https://port70.net/~nsz/c/c89/c89-draft.html
data CTerminal
  -- keywords
  = CAuto
  | CBreak
  | CCase
  | CChar
  | CConst
  | CContinue
  | CDefault -- for switch case
  | CDo
  | CDouble
  | CElse
  | CEnum
  | CExtern
  | CFloat
  | CFor
  | CGoto
  | CIf
  | CInt
  | CLong
  | CRegister
  | CReturn
  | CShort
  | CSigned
  | CSizeof
  | CStatic
  | CStruct
  | CSwitch
  | CTypedef
  | CUnion
  | CUnsigned
  | CVoid
  | CVolatile
  | CWhile
  -- parentheses
  | CParenthesisOpen -- (
  | CParenthesisClose -- )
  | CBracketOpen -- [
  | CBracketClose -- ]
  | CBraceOpen -- {
  | CBraceClose -- }
  -- comparison
  | CNot -- !
  | CEquals -- ==
  | CNotEquals -- !=
  | CLT -- <
  | CLTE -- <=
  | CGT -- >
  | CGTE -- >=
  | CAnd -- &&
  | COr -- ||
  -- operators
  | CModulo -- %
  | CStar -- * (pointer or multiplication)
  | CPlus -- +
  | CMinus -- -
  | CDivision -- /
  | CAmp -- (bitwise and & address of variable)
  | CBitwiseOr -- |
  | CBitShiftLeft -- <<
  | CBitShiftRight -- >>
  | CBitwiseXor -- ^
  | CBitwiseNot -- ~
  -- assign opeartors
  | CModuloAssign -- %=
  | CMultiplicationAssign -- *=
  | CPlusAssign -- +=
  | CMinusAssign -- -=
  | CDivisionAssign -- /=
  | CBitwiseAndAssign -- &=
  | CBitwiseOrAssign -- |=
  | CBitShiftLeftAssign -- <<=
  | CBitShiftRightAssign -- >>=
  | CBitwiseXorAssign -- ^=
  | CIncrement -- ++
  | CDecrement -- --
  -- other symbols
  | CComma -- ,
  | CArrow -- -> (for structs)
  | CDot -- . (for structs)
  | CVarargs -- ... (for functions with variable number of args)
  | CColon -- : (switch case & goto labels & ternary)
  | CSemiColon -- ;
  | CAssign -- =
  | CTernary -- ?
  -- literals
  | CCharLiteral Char
  | CFloatLiteral Double
  | CIntLiteral Int
  | CStringLiteral String
  -- label
  | CLabel String
  -- whitespace
  | CWhiteSpace
  deriving (Show, Eq)

-------------
-- PARSERS --
-------------

parseFloatLiteral :: String -> Double
parseFloatLiteral s = read cleanLiteral
  where
    withoutSuffix =
      if last s `elem` "flFL"
        then init s
        else s
    (digitSequence, exponentSequence) = span (`notElem` "eE") withoutSuffix
    cleanSequence =
      '0' :
      if last digitSequence == '.'
        then digitSequence <> "0"
        else digitSequence
    cleanLiteral = cleanSequence <> exponentSequence

stripIntegerSuffix :: String -> String
stripIntegerSuffix = takeWhile (`notElem` "luLU")

parseIntLiteral :: String -> Int
parseIntLiteral = fst . head . readDec . stripIntegerSuffix

parseOctal :: String -> Int
parseOctal = fst . head . readOct . stripIntegerSuffix

parseHexadecimal :: String -> Int
parseHexadecimal = fst . head . readHex . tail . tail . stripIntegerSuffix

parseChar :: String -> Char
parseChar s =
  if head insideQuotes == '\\'
    then (snd . parseEscapedChar) insideQuotes
    else head insideQuotes
  where
    withoutPrefix = dropWhile (== 'L') s -- ignore prefix
    insideQuotes = (tail . init) withoutPrefix

parseEscapedChar :: String -> (String, Char)
parseEscapedChar s =
  case secondChar of
    '0' -> chr <$> (swap . head . readOct . tail) s
    'x' -> chr <$> (swap . head . readHex . tail . tail) s
    _   -> (swap . head . readLitChar) s
  where
    secondChar = (head . tail) s

parseEscapedString :: String -> String
parseEscapedString s =
  if null escapedString
    then beforeEscape
    else let (afterEscape, escapedChar) = parseEscapedChar escapedString
          in beforeEscape <> [escapedChar] <> parseEscapedString afterEscape
  where
    (beforeEscape, escapedString) = span (/= '\\') s

parseString :: String -> String
parseString s = parseEscapedString insideQuotes
  where
    withoutPrefix = dropWhile (== 'L') s -- ignore prefix
    insideQuotes = (tail . init) withoutPrefix

--------------------
-- REGEX PATTERNS --
--------------------

-- regexes for floating point literals
fractionalConstant :: String
fractionalConstant = "([0-9]\\.[0-9][0-9]*)|([0-9][0-9]*)" -- x.x, .x or x.

-- e & optional sign & number sequence
exponentPart :: String
exponentPart = "[eE](+|-)?[0-9][0-9]*"

-- f -> float, l -> long double
floatingSuffix :: String
floatingSuffix = "[flFL]"

-- unsigned + optional long or long + optional unsigned
integerSuffix :: String
integerSuffix = "([Uu][Ll]?)|([Ll][Uu]?)"

-- backslash + 1-3 octal digits
octalEscapeSequence :: String
octalEscapeSequence = "\\\\[0-7][0-7]?[0-7]?"

-- \x + 1 or mores hexadecimal digits
hexadecimalEscapeSequence :: String
hexadecimalEscapeSequence = "\\\\x[0-9a-fA-F][0-9a-fA-F]*"

-- regular escape sequence, e.g. \n
simpleEscapeSequence :: String
simpleEscapeSequence = "\\\\[abfnrtv'\"?\\\\]"

escapeSequence :: String
escapeSequence =
  "(" <>
  simpleEscapeSequence <>
  ")|(" <> octalEscapeSequence <> ")|(" <> hexadecimalEscapeSequence <> ")"

---------------------------
-- TEMRINAL CONSTRUCTORS --
---------------------------

terminals :: [(String -> CTerminal, String)]
terminals =
  [ (const CAuto, "auto")
  , (const CBreak, "break")
  , (const CCase, "case")
  , (const CChar, "char")
  , (const CConst, "const")
  , (const CContinue, "continue")
  , (const CDefault, "default")
  , (const CDo, "do")
  , (const CDouble, "double")
  , (const CElse, "else")
  , (const CEnum, "enum")
  , (const CExtern, "extern")
  , (const CFloat, "float")
  , (const CFor, "for")
  , (const CGoto, "goto")
  , (const CIf, "if")
  , (const CInt, "int")
  , (const CLong, "long")
  , (const CRegister, "register")
  , (const CReturn, "return")
  , (const CShort, "short")
  , (const CSigned, "signed")
  , (const CSizeof, "sizeof")
  , (const CStatic, "static")
  , (const CStruct, "struct")
  , (const CSwitch, "switch")
  , (const CTypedef, "typedef")
  , (const CUnion, "union")
  , (const CUnsigned, "unsigned")
  , (const CVoid, "void")
  , (const CVolatile, "volatile")
  , (const CWhile, "while")

  , (const CParenthesisOpen, "\\(")
  , (const CParenthesisClose, "\\)")
  , (const CBracketOpen, "\\[")
  , (const CBracketClose, "\\]")
  , (const CBraceOpen, "{")
  , (const CBraceClose, "}")

  , (const CNot, "!")
  , (const CEquals, "==")
  , (const CNotEquals, "!=")
  , (const CLT, "<")
  , (const CLTE, "<=")
  , (const CGT, ">")
  , (const CGTE, ">=")
  , (const CAnd, "&&")
  , (const COr, "\\|\\|")

  , (const CModulo, "%")
  , (const CStar, "\\*")
  , (const CPlus, "+")
  , (const CMinus, "-")
  , (const CDivision, "/")
  , (const CAmp, "&")
  , (const CBitwiseOr, "\\|")
  , (const CBitShiftLeft, "<<")
  , (const CBitShiftRight, ">>")
  , (const CBitwiseXor, "^")
  , (const CBitwiseNot, "~")

  , (const CModuloAssign, "%=")
  , (const CMultiplicationAssign, "\\*=")
  , (const CPlusAssign, "+=")
  , (const CMinusAssign, "-=")
  , (const CDivisionAssign, "/=")
  , (const CBitwiseAndAssign, "&=")
  , (const CBitwiseOrAssign, "\\|=")
  , (const CBitShiftLeftAssign, "<<=")
  , (const CBitShiftRightAssign, ">>=")
  , (const CBitwiseXorAssign, "^=")
  , (const CIncrement, "++")
  , (const CDecrement, "--")

  , (const CComma, ",")
  , (const CArrow, "->")
  , (const CDot, "\\.")
  , (const CVarargs, "\\.\\.\\.")
  , (const CColon, ":")
  , (const CSemiColon, ";")
  , (const CAssign, "=")
  , (const CTernary, "\\?")

  -- digit sequence + exponent + optional suffix
  , ( CFloatLiteral . parseFloatLiteral
    , "[0-9][0-9]*" <> exponentPart <> "(" <> floatingSuffix <> ")?")
  -- fractional + optional exponent + optional suffix
  , ( CFloatLiteral . parseFloatLiteral
    , fractionalConstant <>
      "(" <> exponentPart <> ")?(" <> floatingSuffix <> ")?")
  -- nonzero digit followed by digits
  , (CIntLiteral . parseIntLiteral, "[1-9][0-9]*(" <> integerSuffix <> ")?")
  -- zero followed by octal digits
  , (CIntLiteral . parseOctal, "0[0-7]*(" <> integerSuffix <> ")?")
  -- 0x followd by hexadecimal digits
  , ( CIntLiteral . parseHexadecimal
    , "0x[0-9a-fA-F][0-9a-fA-F]*(" <> integerSuffix <> ")?")
  , (CCharLiteral . parseChar, "L?")
  , ( CStringLiteral . parseString
    , "L?\"([^\"\n\\\\]|(" <> escapeSequence <> "))*\"")

  , (CLabel, "[a-zA-Z_][a-zA-Z0-9_]*")

  , (const CWhiteSpace, "[ \n\t\r]*")
  ]
