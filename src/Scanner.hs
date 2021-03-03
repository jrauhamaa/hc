module Scanner
  ( Scanner(..)
  , fromSpec
  , scanLexeme
  , scanInput
  , ScanElement
  ) where

import Control.Applicative ((<|>))
import qualified Data.Set as S

import qualified NFA as N
import Regex (scanRegex)
import Lexeme (CLexeme(..))

{-
constructors: list of functions mapping scanned strings to lexemes
nfas: nfas of corresponding constructors
state: state of corresponding constructors
-}
data Scanner =
  Scanner
    { constructors :: [String -> Coordinates -> ScanElement]
    , nfas :: [N.NFA]
    , state :: [S.Set Int]
    }

type ScanElement = (CLexeme, Coordinates)

type Row = Int

type Column = Int

type Coordinates = (Row, Column)

fromSpec :: [(String -> CLexeme, String)] -> Maybe Scanner
fromSpec spec = getScanner <$> sequenceA regexes
  where
    regexes = map (scanRegex . snd) spec
    getScanner regexList =
      let nfas' = map N.fromRegexValue regexList
       in Scanner
            { constructors = map (getScanFunction . fst) spec
            , nfas = nfas'
            , state = map N.initialState nfas'
            }

-- add row & col information to scan result
getScanFunction :: (String -> CLexeme) -> String -> Coordinates -> ScanElement
getScanFunction scanf s (oldRow, oldCol) = (scanf s, (nextRow, nextCol))
  where
    nextRow = oldRow + (length . filter (== '\n') $ s)
    nextCol =
      if '\n' `elem` s
        then length . takeWhile (/= '\n') . reverse $ s
        else oldCol + length s

scanLexeme ::
     Scanner -> String -> String -> Coordinates -> Maybe (String, ScanElement)
scanLexeme _ "" _ _ = Nothing
scanLexeme scanner (inputChar:unscanned) scanned (oldRow, oldCol) =
  if finishedScanning
    then newResult
    else scanLexeme newScanner unscanned newScanned (oldRow, oldCol) <|>
         newResult
  where
    newStates =
      zipWith3 N.readInput (nfas scanner) (state scanner) (repeat inputChar)
    accepting = zipWith N.isAccepting (nfas scanner) newStates
    acceptingConstructors =
      map snd $ filter fst (zip accepting $ constructors scanner)
    newScanned = scanned ++ [inputChar]
    newResult =
      if not $ null acceptingConstructors
        then Just
               ( unscanned
               , head acceptingConstructors newScanned (oldRow, oldCol))
        else Nothing
    finishedScanning = all (== S.empty) newStates
    newScanner =
      Scanner
        { constructors = constructors scanner
        , nfas = nfas scanner
        , state = newStates
        }

scanInput :: Scanner -> String -> Coordinates -> Maybe [ScanElement]
scanInput _ "" _ = Just []
scanInput scanner input (row, col) = do
  (unScanned, (lexeme, (newRow, newCol))) <-
    scanLexeme scanner input "" (row, col)
  rest <- scanInput scanner unScanned (newRow, newCol)
  return ((lexeme, (row, col)) : rest)
