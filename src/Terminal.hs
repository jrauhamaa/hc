module Terminal
  ( terminals
  , CTerminal(..)
  ) where

data CTerminal
  = CInt
  | CFloat
  | CDouble
  | CChar
  | CNull
  | CCharLiteral Char
  | CStringLiteral String
  | CIntLiteral Int
  | CFloatLiteral Double
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
  [ (const CInt, "int")
  , (const CFloat, "float")
  , (const CDouble, "double")
  , (const CChar, "char")
  , (const CNull, "null")
  , (CStringLiteral . init . tail, "\"([^\"]|(\\\\\"))*\"")
  , (CCharLiteral . charLiteralToChar, "'[^']|(\\\\[nt\\\\'])'")
  , (CIntLiteral . read, "(-[0-9])|[0-9][0-9]*")
  , ( CFloatLiteral . read . (\(a, b) -> a ++ "0" ++ b) . span (== '-')
    , "(-[0-9]*)|([0-9]*)\\.[0-9][0-9]*")
  , (CLabel, "[a-zA-Z_][a-zA-Z0-9_]*")
  ]
