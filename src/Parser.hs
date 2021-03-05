{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative

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

data ParseResult a =
  ParseResult
    { coordinates :: Coordinates
    , result :: a
    }

instance Functor ParseResult where
  fmap fab (ParseResult c a) = ParseResult c $ fab a

instance Applicative ParseResult where
  pure x = ParseResult (0, 0) x
  (ParseResult c1 fab) <*> (ParseResult c2 a) = ParseResult c2 $ fab a

instance Functor Parser where
  fmap fab pa = Parser $ \input -> (fmap . fmap . fmap) fab $ runParser pa input

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, ParseResult (fst $ head input) x)
  p1 <*> p2 = Parser $ \input -> do
    (input', pab) <- runParser p1 input
    (input'', pa) <- runParser p2 input'
    return (input'', pab <*> pa)

singleP :: CLexeme -> Parser CLexeme
singleP l =
  Parser $ \case
    [] -> Left $ ParseError (maxBound, maxBound) "Unexpected EOF"
    ((c, lexeme):rest) ->
      if lexeme == l
        then Right (rest, ParseResult c lexeme)
        else Left $
             ParseError c $
             mconcat
               ["Expected ", show l, ". Instead encountered ", show lexeme, "."]
