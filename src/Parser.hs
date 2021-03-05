{-# LANGUAGE LambdaCase #-}

module Parser where

import Lexeme (CLexeme(..))
import ParseElements
import Scanner (Coordinates, ScanElement)

newtype Parser a =
  Parser
    { runParser :: [ScanElement] -> Either ParseError ( [ScanElement]
                                                      , ParseResult a)
    }

data ParseError =
  ParseError
    { errorCoordinates :: Coordinates
    , errorMsg :: String
    }
  deriving (Eq, Show)

data ParseResult a =
  ParseResult
    { coordinates :: Coordinates
    , result :: a
    }
  deriving (Eq, Show)

instance Functor ParseResult where
  fmap fab (ParseResult c a) = ParseResult c $ fab a

instance Applicative ParseResult where
  pure x = ParseResult (0, 0) x
  (ParseResult _ fab) <*> (ParseResult c2 a) = ParseResult c2 $ fab a

instance Functor Parser where
  fmap fab pa = Parser $ \input -> (fmap . fmap . fmap) fab $ runParser pa input

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, ParseResult (fst $ head input) x)
  p1 <*> p2 =
    Parser $ \input -> do
      (input', pab) <- runParser p1 input
      (input'', pa) <- runParser p2 input'
      return (input'', pab <*> pa)

unexpectedEof :: ParseError
unexpectedEof = ParseError (maxBound, maxBound) "Unexpected EOF"

unexpectedLexeme :: Coordinates -> String -> String -> ParseError
unexpectedLexeme c expected encountered =
  ParseError c $
  mconcat
    [ "Expected "
    , expected
    , ". Instead encountered "
    , encountered
    , "."
    ]

singleP :: CLexeme -> Parser CLexeme
singleP l =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, lexeme):rest) ->
      if lexeme == l
        then Right (rest, ParseResult c lexeme)
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: Parser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LIntLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: Parser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LFloatLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: Parser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LCharLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: Parser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LStringLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: Parser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LLabel x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l
