module Terminal
  ( terminals
  , CTerminal(..)
  ) where

data CTerminal
  -- types
  = CAmp -- like x&
  | CBool
  | CChar
  | CConst
  | CDouble
  | CEnum
  | CExternal
  | CFloat
  | CInt
  | CLong
  | CNull
  | CRegister
  | CShort
  | CSigned
  | CStatic
  | CStruct
  | CUnion
  | CUnsigned
  -- literals
  | CCharLiteral Char
  | CFloatLiteral Double
  | CIntLiteral Int
  | CStringLiteral String
  | CBoolLiteral Bool
  -- symbols
  | CBraceClose
  | CBraceOpen
  | CBracketClose
  | CBracketOpen
  | CParenthesisClose
  | CParenthesisOpen
  | CSemiColon
  | CAssign
  | CColon
  | CArrow -- e.g. struct->field
  | CDot -- e.g. struct.field
  | CComma -- e.g. in function argument list
  -- operators
  | CPlus
  | CMinus
  | CDivision
  | CStar -- pointer or multiplication
  | CModulo
  | CPlusPlus -- e.g. i++
  | CMinusMinus -- e.g. i--
  | CBitwiseOr -- |
  | CBitwiseAnd -- &
  | CBitwiseXor -- ^
  | CBitwiseNot -- ~
  | CBitShiftLeft -- <<
  | CBitShiftRight -- >>
  -- comparison
  | COr -- ||
  | CAnd -- &&
  | CEquals -- ==
  | CNotEquals -- !=
  | CNot -- !
  | CLT -- <
  | CLTE -- <=
  | CGT -- >
  | CGTE -- >=
  -- equals operators
  | CPlusAssignment -- +=
  | CMinusAssignment -- -=
  | CDivisionAssignment -- /=
  | CMultiplicationAssignment -- *=
  | CModuloAssignment -- %=
  | CBitwiseOrAssignment -- |=
  | CBitwiseAndAssignment -- &=
  | CBitwiseXorAssignment -- ^=
  | CShiftLeftAssignment -- <<=
  | CShiftRightAssignment -- >>=
  -- flow control
  | CIf
  | CElse
  | CFor
  | CDo
  | CWhile
  | CBreak
  | CContinue
  | CSwitch
  | CCase
  | CReturn
  -- label
  | CLabel String
  deriving (Show, Eq)

charLiteralToChar :: String -> Char
charLiteralToChar literal =
  if length charString == 1
    then head charString
    else escapeSequence charString
  where
    charString = tail $ init literal
    escapeSequence "\\n" = '\n'
    escapeSequence "\\t" = '\t'
    escapeSequence "\\\\" = '\\'
    escapeSequence "\\'" = '\''
    escapeSequence _ = undefined -- if we get here, something has gone wrong

terminals :: [(String -> CTerminal, String)]
terminals =
  [ (const CAmp, "&")
  , (const CBool, "bool")
  , (const CChar, "char")
  , (const CConst, "const")
  , (const CDouble, "double")
  , (const CEnum, "enum")
  , (const CExternal, "extern")
  , (const CFloat, "float")
  , (const CInt, "double")
  , (const CLong, "long")
  , (const CNull, "null")
  , (const CRegister, "register")
  , (const CShort, "short")
  , (const CSigned, "signed")
  , (const CStatic, "static")
  , (const CStruct, "struct")
  , (const CUnion, "union")
  , (const CUnsigned, "unsigned")

  , (CBoolLiteral . (== "true"), "(true)|(false)")
  , (CStringLiteral . init . tail, "\"([^\"]|(\\\\\"))*\"")
  , (CCharLiteral . charLiteralToChar, "'[^']|(\\\\[nt\\\\'])'")
  , (CIntLiteral . read, "(-[0-9])|[0-9][0-9]*")
  , ( CFloatLiteral . read . (\(a, b) -> a ++ "0" ++ b) . span (== '-')
    , "(-[0-9]*)|([0-9]*)\\.[0-9][0-9]*")

  , (const CBraceOpen, "{")
  , (const CBraceClose, "}")
  , (const CBracketOpen, "\\[")
  , (const CBracketClose, "\\]")
  , (const CParenthesisOpen, "\\(")
  , (const CParenthesisClose, "\\)")
  , (const CSemiColon, ";")
  , (const CAssign, "=")
  , (const CColon, ":")
  , (const CArrow, "->")
  , (const CDot, "\\.")
  , (const CComma, ",")

  , (const CPlus, "+")
  , (const CMinus, "-")
  , (const CDivision, "/")
  , (const CStar, "\\*")
  , (const CModulo, "%")
  , (const CPlusPlus, "++")
  , (const CMinusMinus, "--")

  , (const CBitwiseOr, "\\|")
  , (const CBitwiseAnd, "&")
  , (const CBitwiseXor, "^")
  , (const CBitwiseNot, "~")
  , (const CBitShiftLeft, "<<")
  , (const CBitShiftRight, ">>")

  , (const COr, "\\|\\|")
  , (const CAnd, "&&")
  , (const CEquals, "==")
  , (const CNotEquals, "!=")
  , (const CNot, "!")
  , (const CLT, "<")
  , (const CLTE, "<=")
  , (const CGT, ">")
  , (const CGTE, ">=")

  , (const CPlusAssignment, "+=")
  , (const CMinusAssignment, "-=")
  , (const CDivisionAssignment, "/=")
  , (const CMultiplicationAssignment, "\\*=")
  , (const CModuloAssignment, "%=")
  , (const CBitwiseOrAssignment, "\\|=")
  , (const CBitwiseAndAssignment, "&=")
  , (const CBitwiseXorAssignment, "^=")
  , (const CShiftLeftAssignment, "<<=")
  , (const CShiftRightAssignment, ">>=")

  , (const CIf, "if")
  , (const CElse, "else")
  , (const CFor, "for")
  , (const CDo, "do")
  , (const CWhile, "while")
  , (const CBreak, "break")
  , (const CContinue, "continue")
  , (const CSwitch, "switch")
  , (const CCase, "case")
  , (const CReturn, "return")

  , (CLabel, "[a-zA-Z_][a-zA-Z0-9_]*")
  ]
