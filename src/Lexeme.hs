module Lexeme
  ( lexemes
  , CLexeme(..)
  ) where

import Data.Char
import Data.Tuple
import Numeric

-- as described in https://port70.net/~nsz/c/c89/c89-draft.html
data CLexeme
  -- keywords
  = LAuto
  | LBreak
  | LCase
  | LChar
  | LConst
  | LContinue
  | LDefault -- for switch case
  | LDo
  | LDouble
  | LElse
  | LEnum
  | LExtern
  | LFloat
  | LFor
  | LGoto
  | LIf
  | LInt
  | LLong
  | LRegister
  | LReturn
  | LShort
  | LSigned
  | LSizeof
  | LStatic
  | LStruct
  | LSwitch
  | LTypedef
  | LUnion
  | LUnsigned
  | LVoid
  | LVolatile
  | LWhile
  -- parentheses
  | LParenthesisOpen -- (
  | LParenthesisClose -- )
  | LBracketOpen -- [
  | LBracketClose -- ]
  | LBraceOpen -- {
  | LBraceClose -- }
  -- comparison
  | LNot -- !
  | LEquals -- ==
  | LNotEquals -- !=
  | LLT -- <
  | LLTE -- <=
  | LGT -- >
  | LGTE -- >=
  | LAnd -- &&
  | LOr -- ||
  -- operators
  | LModulo -- %
  | LStar -- * (pointer or multiplication)
  | LPlus -- +
  | LMinus -- -
  | LDivision -- /
  | LAmp -- (bitwise and & address of variable)
  | LBitwiseOr -- |
  | LBitShiftLeft -- <<
  | LBitShiftRight -- >>
  | LBitwiseXor -- ^
  | LBitwiseNot -- ~
  -- assign opeartors
  | LModuloAssign -- %=
  | LMultiplicationAssign -- *=
  | LPlusAssign -- +=
  | LMinusAssign -- -=
  | LDivisionAssign -- /=
  | LBitwiseAndAssign -- &=
  | LBitwiseOrAssign -- |=
  | LBitShiftLeftAssign -- <<=
  | LBitShiftRightAssign -- >>=
  | LBitwiseXorAssign -- ^=
  | LIncrement -- ++
  | LDecrement -- --
  -- other symbols
  | LComma -- ,
  | LArrow -- -> (for structs)
  | LDot -- . (for structs)
  | LVarargs -- ... (for functions with variable number of args)
  | LColon -- : (switch case & goto labels & ternary)
  | LSemiColon -- ;
  | LAssign -- =
  | LTernary -- ?
  -- literals
  | LCharLiteral Char
  | LFloatLiteral Double
  | LIntLiteral Int
  | LStringLiteral String
  -- label
  | LLabel String
  -- whitespace
  | LWhiteSpace
  -- comment
  | LComment
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
-- TEMRINAL LONSTRULTORS --
---------------------------

lexemes :: [(String -> CLexeme, String)]
lexemes =
  [ (const LAuto, "auto")
  , (const LBreak, "break")
  , (const LCase, "case")
  , (const LChar, "char")
  , (const LConst, "const")
  , (const LContinue, "continue")
  , (const LDefault, "default")
  , (const LDo, "do")
  , (const LDouble, "double")
  , (const LElse, "else")
  , (const LEnum, "enum")
  , (const LExtern, "extern")
  , (const LFloat, "float")
  , (const LFor, "for")
  , (const LGoto, "goto")
  , (const LIf, "if")
  , (const LInt, "int")
  , (const LLong, "long")
  , (const LRegister, "register")
  , (const LReturn, "return")
  , (const LShort, "short")
  , (const LSigned, "signed")
  , (const LSizeof, "sizeof")
  , (const LStatic, "static")
  , (const LStruct, "struct")
  , (const LSwitch, "switch")
  , (const LTypedef, "typedef")
  , (const LUnion, "union")
  , (const LUnsigned, "unsigned")
  , (const LVoid, "void")
  , (const LVolatile, "volatile")
  , (const LWhile, "while")

  , (const LParenthesisOpen, "\\(")
  , (const LParenthesisClose, "\\)")
  , (const LBracketOpen, "\\[")
  , (const LBracketClose, "\\]")
  , (const LBraceOpen, "{")
  , (const LBraceClose, "}")

  , (const LNot, "!")
  , (const LEquals, "==")
  , (const LNotEquals, "!=")
  , (const LLT, "<")
  , (const LLTE, "<=")
  , (const LGT, ">")
  , (const LGTE, ">=")
  , (const LAnd, "&&")
  , (const LOr, "\\|\\|")

  , (const LModulo, "%")
  , (const LStar, "\\*")
  , (const LPlus, "+")
  , (const LMinus, "-")
  , (const LDivision, "/")
  , (const LAmp, "&")
  , (const LBitwiseOr, "\\|")
  , (const LBitShiftLeft, "<<")
  , (const LBitShiftRight, ">>")
  , (const LBitwiseXor, "^")
  , (const LBitwiseNot, "~")

  , (const LModuloAssign, "%=")
  , (const LMultiplicationAssign, "\\*=")
  , (const LPlusAssign, "+=")
  , (const LMinusAssign, "-=")
  , (const LDivisionAssign, "/=")
  , (const LBitwiseAndAssign, "&=")
  , (const LBitwiseOrAssign, "\\|=")
  , (const LBitShiftLeftAssign, "<<=")
  , (const LBitShiftRightAssign, ">>=")
  , (const LBitwiseXorAssign, "^=")
  , (const LIncrement, "++")
  , (const LDecrement, "--")

  , (const LComma, ",")
  , (const LArrow, "->")
  , (const LDot, "\\.")
  , (const LVarargs, "\\.\\.\\.")
  , (const LColon, ":")
  , (const LSemiColon, ";")
  , (const LAssign, "=")
  , (const LTernary, "\\?")

  -- digit sequence + exponent + optional suffix
  , ( LFloatLiteral . parseFloatLiteral
    , "[0-9][0-9]*" <> exponentPart <> "(" <> floatingSuffix <> ")?")
  -- fractional + optional exponent + optional suffix
  , ( LFloatLiteral . parseFloatLiteral
    , fractionalConstant <>
      "(" <> exponentPart <> ")?(" <> floatingSuffix <> ")?")
  -- nonzero digit followed by digits
  , (LIntLiteral . parseIntLiteral, "[1-9][0-9]*(" <> integerSuffix <> ")?")
  -- zero followed by octal digits
  , (LIntLiteral . parseOctal, "0[0-7]*(" <> integerSuffix <> ")?")
  -- 0x followd by hexadecimal digits
  , ( LIntLiteral . parseHexadecimal
    , "0x[0-9a-fA-F][0-9a-fA-F]*(" <> integerSuffix <> ")?")
  , (LCharLiteral . parseChar, "L?")
  , ( LStringLiteral . parseString
    , "L?\"([^\"\n\\\\]|(" <> escapeSequence <> "))*\"")

  , (LLabel, "[a-zA-Z_][a-zA-Z0-9_]*")

  , (const LWhiteSpace, "[ \n\t\r]*")

  , (const LComment, "//[^\n\r]*")
  , (const LComment, "/\\*[^*]*\\*/")
  ]
