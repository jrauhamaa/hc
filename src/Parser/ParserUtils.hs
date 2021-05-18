{-# LANGUAGE LambdaCase #-}

{- A recursive descent parser.
   CTranslationUnit becomes the root element of the AST.
   Expects a string of ScanElements terminted by LEndMarker -}

module Parser.ParserUtils where

import Control.Applicative

import Parser.ParseItem
import Utils (Location, Error(..), errorLoc)
import Scanner ( ScanItem(..)
               , CLexeme(..)
               )

-----------
-- TYPES --
-----------

type Input = [ScanItem CLexeme]

newtype Parser a =
  Parser
    { runParser :: Input -> Either Error (Input, a)
    }

type PIParser a = Parser (ParseItem a)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  p1 <*> p2 =
    Parser $ \input -> do
      (input', fab) <- runParser p1 input
      (input'', a) <- runParser p2 input'
      return (input'', fab a)

-- Run first parser. If it fails, run second parser.
instance Alternative Parser where
  empty = Parser $ \_ -> Left $ ParseError ("", (0, 0)) "Error"
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      r@(Right _) -> r
      Left e1 ->
        case runParser p2 input of
          r@(Right _) -> r
          Left e2 ->
            if errorLoc e1 >= errorLoc e2
              then Left e1
              else Left e2

-----------
-- UTILS --
-----------

-- wrap the result of a parser in a ParseItem
parserToPIParser :: Parser a -> PIParser a
parserToPIParser p =
  Parser $ \case
    [] -> Left unexpectedEof
    input@((ScanItem c _ _):_) -> do
      (notParsed, a) <- runParser p input
      return (notParsed, ParseItem c a initialSymbols)

-- TODO: add filename information
unexpectedEof :: Error
unexpectedEof = ParseError ("", (0, 0)) "Unexpected EOF"

unexpectedLexeme :: Location -> String -> String -> Error
unexpectedLexeme c expected encountered =
  ParseError c $
  mconcat ["Expected ", expected, ". Instead encountered ", encountered, "."]

------------------------
-- ELEMENTARY PARSERS --
------------------------

failingP :: Parser a
failingP = Parser $ \_ -> Left $ ParseError ("", (0, 0)) "Error"

-- parse a single lexeme
singleP :: CLexeme -> Parser CLexeme
singleP l =
  Parser $ \case
    [] -> Left unexpectedEof
    (ScanItem c _ lexeme:rest) ->
      if lexeme == l
        then Right (rest, lexeme)
        else Left $ unexpectedLexeme c (show l) (show lexeme)

singlePWithValue :: (ScanItem CLexeme -> Either Error a) -> Parser a
singlePWithValue readValue =
  Parser $ \case
    [] -> Left unexpectedEof
    (item:rest) -> do
      value <- readValue item
      return (rest, value)

intLiteralP :: PIParser Int
intLiteralP =
  singlePWithValue $ \case
    ScanItem { scanLoc = l, scanItem = LIntLiteral x } ->
      return (ParseItem l x initialSymbols)
    ScanItem { scanLoc = l, scanItem = item } ->
      Left . unexpectedLexeme l "LIntLiteral" $ show item

floatLiteralP :: PIParser Double
floatLiteralP =
  singlePWithValue $ \case
    ScanItem { scanLoc = l, scanItem = LFloatLiteral x } ->
      return (ParseItem l x initialSymbols)
    ScanItem { scanLoc = l, scanItem = item } ->
      Left . unexpectedLexeme l "LFloatLiteral" $ show item

charLiteralP :: PIParser Char
charLiteralP =
  singlePWithValue $ \case
    ScanItem { scanLoc = l, scanItem = LCharLiteral x } ->
      return (ParseItem l x initialSymbols)
    ScanItem { scanLoc = l, scanItem = item } ->
      Left . unexpectedLexeme l "LCharLiteral" $ show item

stringLiteralP :: PIParser String
stringLiteralP =
  singlePWithValue $ \case
    ScanItem { scanLoc = l, scanItem = LStringLiteral x } ->
      return (ParseItem l x initialSymbols)
    ScanItem { scanLoc = l, scanItem = item } ->
      Left . unexpectedLexeme l "LStringLiteral" $ show item

labelP :: Parser String
labelP =
  singlePWithValue $ \case
    ScanItem { scanItem = LLabel x } -> return x
    ScanItem { scanLoc = l, scanItem = item } ->
      Left . unexpectedLexeme l "LLabel" $ show item

-- return Nothing instead of error if parsing fails
optionalParser :: Parser a -> Parser (Maybe a)
optionalParser p = fmap Just p <|> pure Nothing

-- wrap parser in parentheses
parenthesisP :: Parser a -> Parser a
parenthesisP p = singleP LParenthesisOpen *> p <* singleP LParenthesisClose

-- wrap parser in brackets
bracketP :: Parser a -> Parser a
bracketP p = singleP LBracketOpen *> p <* singleP LBracketClose

-- wrap parser in braces
braceP :: Parser a -> Parser a
braceP p = singleP LBraceOpen *> p <* singleP LBraceClose

{- Run first parser on the input. Require second parser to succeed parsing
   after the first parser.
   Return the longest successful parse. -}
afterP :: Parser a -> Parser b -> Parser a
afterP before after =
  Parser $ \input -> do
    (afterMaxParse, _) <- runParser before input
    let maxParseLength = length input - length afterMaxParse
        parsers = map splitParser [0 .. maxParseLength]
        p = foldl reduce failingP parsers
    runParser p input
  where
    reduce acc p =
      Parser $ \input ->
        case runParser acc input of
          Left _ -> runParser p input
          resultAcc@(Right (remainingAcc, _)) ->
            case runParser p input of
              Left _ -> resultAcc
              resultP@(Right (remainingP, _)) -> do
                (remainingAccAfter, _) <- runParser after remainingAcc
                (remainingPAfter, _) <- runParser after remainingP
                if length remainingAccAfter <= length remainingPAfter
                  then resultAcc
                  else resultP
    splitParser n =
      Parser $ \input ->
        let (begin, end) = splitAt n input
        in
          case runParser before $
                 begin <> [ScanItem ("", (0, 0)) "" LEndMarker] of
            Right ([ScanItem _ _ LEndMarker], e) ->
              case runParser after end of
                Right _ -> Right (end, e)
                _ -> Left $ ParseError ("", (0, 0)) "Error"
            _ -> Left $ ParseError ("", (0, 0)) "Error"

