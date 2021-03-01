module Scanner
  ( Scanner(..)
  , fromSpec
  , scanTerminal
  , scanInput
  ) where

import Control.Applicative ((<|>))
import qualified Data.Set as S

import qualified NFA as N
import Regex (scanRegex)
import Terminal (CTerminal(..))

{-
constructors: list of functions mapping scanned strings to terminals
nfas: nfas of corresponding constructors
state: state of corresponding constructors
-}
data Scanner =
  Scanner
    { constructors :: [String -> CTerminal]
    , nfas :: [N.NFA]
    , state :: [S.Set Int]
    }

fromSpec :: [(String -> CTerminal, String)] -> Maybe Scanner
fromSpec spec = do
  regexList <- sequenceA regexes
  return
    Scanner
      { constructors = map fst spec
      , nfas = map N.fromRegexValue regexList
      , state = map (const $ S.singleton 1) spec
      }
  where
    regexes = map (scanRegex . snd) spec

scanTerminal :: Scanner -> String -> String -> Maybe (String, CTerminal)
scanTerminal _ "" _ = Nothing
scanTerminal scanner (inputChar:unscanned) scanned =
  if finishedScanning
    then newResult
    else scanTerminal newScanner unscanned newScanned <|> newResult
  where
    newStates =
      zipWith3 N.readInput (nfas scanner) (state scanner) (repeat inputChar)
    accepting = zipWith N.isAccepting (nfas scanner) newStates
    acceptingConstructors =
      map snd $ filter fst (zip accepting $ constructors scanner)
    newScanned = scanned ++ [inputChar]
    newResult =
      if not $ null acceptingConstructors
        then Just (unscanned, head acceptingConstructors newScanned)
        else Nothing
    finishedScanning = all (== S.empty) newStates
    newScanner =
      Scanner
        { constructors = constructors scanner
        , nfas = nfas scanner
        , state = newStates
        }

scanInput :: Scanner -> String -> Maybe [CTerminal]
scanInput _ "" = Just []
scanInput scanner input = do
  (unScanned, terminal) <- scanTerminal scanner input ""
  rest <- scanInput scanner $ dropWhiteSpace unScanned
  return (terminal : rest)
  where
    dropWhiteSpace = dropWhile (`elem` " \n\t")
