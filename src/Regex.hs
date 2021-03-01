module Regex
  ( RegexValue(..)
  , scanRegex
  ) where

import Control.Applicative

-----------
-- TYPES --
-----------
data RegexValue
  = RegexSequence [RegexValue] -- regex or a subexpression
  | RegexChar Char -- Char in a sequence
  | RegexStar RegexValue -- Repeat this value n times
  | RegexCharSet [Char] -- match against a set of chars
  | RegexNegativeCharSet [Char] -- anything except members
  | RegexAny -- any char (dot)
  | RegexUnion [RegexValue] -- match against any of the regexvalues
  deriving (Eq, Show)

newtype Scanner a =
  Scanner
    { runScanner :: String -> Maybe (String, a)
    }

instance Functor Scanner where
  fmap f (Scanner sf) =
    Scanner $ \input -> do
      (input', x) <- sf input
      return (input', f x)

instance Applicative Scanner where
  pure a = Scanner (\s -> Just (s, a))
  Scanner s1 <*> Scanner s2 =
    Scanner $ \input -> do
      (input', f) <- s1 input
      (input'', a) <- s2 input'
      return (input'', f a)

instance Alternative Scanner where
  empty = Scanner $ const Nothing
  Scanner s1 <|> Scanner s2 = Scanner $ liftA2 (<|>) s1 s2

-------------------
-- SCANNER UTILS --
-------------------
charS :: String -> Scanner Char
charS s = Scanner f
  where
    f (x:xs)
      | x `elem` s = Just (xs, x)
    f _ = Nothing

notCharS :: String -> Scanner Char
notCharS s = Scanner f
  where
    f (x:xs)
      | x `notElem` s = Just (xs, x)
    f _ = Nothing

charRangeS :: Scanner String
charRangeS =
  (\a b -> [a .. b]) <$> (notCharS "^-" <* charS "-") <*> notCharS "-"

charSetSequenceS :: Scanner [String]
charSetSequenceS =
  charS "[" *> many (charRangeS <|> ((: "") <$> notCharS "^]")) <* charS "]"

negativeCharSetSequenceS :: Scanner [String]
negativeCharSetSequenceS =
  charS "[" *> charS "^" *> many (charRangeS <|> ((: "") <$> notCharS "^]")) <*
  charS "]"

starScanners :: Scanner RegexValue
starScanners =
  regexCharS <|> escapeSequenceS <|> regexAnyS <|> regexSubSequenceS <|>
  regexCharSetS <|>
  regexNegativeCharSetS

unionScanners :: Scanner RegexValue
unionScanners = regexStarS <|> starScanners

regexScanners :: Scanner RegexValue
regexScanners = regexUnionS <|> unionScanners

simpleUnionS :: Scanner [RegexValue]
simpleUnionS =
  (\a b -> [a, b]) <$> (unionScanners <* charS "|") <*> unionScanners

unionListS :: Scanner [RegexValue]
unionListS = (++) <$> simpleUnionS <*> many (charS "|" *> unionScanners)

--------------------
-- REGEX SCANNERS --
--------------------
-- normal character
regexCharS :: Scanner RegexValue
regexCharS = RegexChar <$> notCharS "()[].*|\\"

-- dot
regexAnyS :: Scanner RegexValue
regexAnyS = fmap (const RegexAny) (charS ".")

-- escaped character
escapeSequenceS :: Scanner RegexValue
escapeSequenceS = RegexChar <$> (charS "\\" *> charS "[]()*.\\|")

-- the entire regex
regexSequenceS :: Scanner RegexValue
regexSequenceS = RegexSequence <$> many regexScanners

-- subexpression (enclosed in parentheses)
regexSubSequenceS :: Scanner RegexValue
regexSubSequenceS = charS "(" *> regexSequenceS <* charS ")"

-- [a-z] -type of set of chars
regexCharSetS :: Scanner RegexValue
regexCharSetS = RegexCharSet . concat <$> charSetSequenceS

-- [^a-z] -type of set of chars
regexNegativeCharSetS :: Scanner RegexValue
regexNegativeCharSetS =
  RegexNegativeCharSet . concat <$> negativeCharSetSequenceS

-- star expression
regexStarS :: Scanner RegexValue
regexStarS = RegexStar <$> (starScanners <* charS "*")

-- union
regexUnionS :: Scanner RegexValue
regexUnionS = RegexUnion <$> unionListS

------------------
-- MAIN SCANNER --
------------------
scanRegex :: String -> Maybe RegexValue
scanRegex re = do
  (unscanned, scanned) <- runScanner regexSequenceS re
  if null unscanned
    then return scanned
    else Nothing
