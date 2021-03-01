module NFA
  ( NFA(..)
  , Transition
  , fromRegexValue
  , readInput
  , isAccepting
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Regex (RegexValue(..))

----------------
-- DATA TYPES --
----------------
type Transition = M.Map (Char, Int) (S.Set Int)

data NFA =
  NFA
    { nStates :: Int -- 1 is the starting state, nStates is the accepting state
    , transition :: Transition -- \NUL stands for epsilon-transitions
    }
  deriving (Eq, Show)

---------------
-- UTILITIES --
---------------
-- shift state numbers by nShift
shiftTransition :: Transition -> Int -> Transition
shiftTransition oldT nShift =
  M.map (S.map (+ nShift)) . M.mapKeys (fmap (+ nShift)) $ oldT

-- add epsilon transition from accepting state of nfa1
-- to starting state of nfa2
unionTransitions :: NFA -> NFA -> Transition
unionTransitions nfa1 nfa2 =
  M.insert ('\NUL', shiftAmt) (S.singleton $ shiftAmt + 1) union
  where
    trans1 = transition nfa1
    shiftAmt = nStates nfa1
    trans2 = shiftTransition (transition nfa2) shiftAmt
    union = M.unionWith S.union trans1 trans2

-- accepting state of nfa 1 becomes starting state of nfa2
concatNFA :: NFA -> NFA -> NFA
concatNFA nfa1 nfa2 =
  NFA
    { nStates = nStates nfa1 + nStates nfa2
    , transition = unionTransitions nfa1 nfa2
    }

-- list of all recognized chars
allChars :: String
allChars = [' ' .. '~']

-- transition function that moves from src to dest when encouters any of chars
charTransition :: String -> Int -> S.Set Int -> Transition
charTransition chars src dest = M.fromList $ map transitionItem chars
  where
    transitionItem c = ((c, src), dest)

-- Follow epsilon transitions. NOTE: infinite recursion when circular epsilons
epsilonTransitions :: NFA -> Int -> S.Set Int
epsilonTransitions nfa state = S.union nextStates twoEpsilons
  where
    nextStates = M.findWithDefault S.empty ('\NUL', state) (transition nfa)
    twoEpsilons = S.fold S.union S.empty $ S.map (epsilonTransitions nfa) nextStates

---------
-- API --
---------
-- single char
fromRegexValue :: RegexValue -> NFA
fromRegexValue (RegexChar c) =
  NFA {nStates = 2, transition = charTransition [c] 1 acc}
  where
    acc = S.singleton 2
-- any single char
fromRegexValue RegexAny =
  NFA {nStates = 2, transition = charTransition allChars 1 acc}
  where
    acc = S.singleton 2
-- Empty sequence. This should't occur.
fromRegexValue (RegexSequence []) = NFA {nStates = 0, transition = M.empty}
-- sequence of one item
fromRegexValue (RegexSequence [re]) = fromRegexValue re
-- sequence of values
fromRegexValue (RegexSequence (re:res)) =
  foldl concatNFA (fromRegexValue re) $ map fromRegexValue res
-- repeat any number of times
fromRegexValue (RegexStar re) =
  NFA
    { nStates = nStates reNFA + 1
    , transition = M.unionWith S.union (transition reNFA) newTransitions
    }
  where
    reNFA = fromRegexValue re
    oldAcceptingState = nStates reNFA
    newTransitions =
      M.fromList
        [ (('\NUL', 1), S.singleton $ oldAcceptingState + 1)
        , (('\NUL', oldAcceptingState), S.singleton 1)
        ]
-- accept any of chars in the set
fromRegexValue (RegexCharSet charSet) =
  NFA {nStates = 2, transition = M.unions transitions}
  where
    transitions = map (transition . fromRegexValue . RegexChar) charSet
-- accept any char not in the set
fromRegexValue (RegexNegativeCharSet charSet) =
  fromRegexValue $ RegexCharSet $ filter (`notElem` charSet) allChars
-- accept any pattern in the union
fromRegexValue (RegexUnion res) =
  NFA
    { nStates = nStates'
    , transition = foldr (M.unionWith S.union) M.empty allTransitions
    }
  where
    nfas = map fromRegexValue res
    nStates' = sum (map nStates nfas) + 2
    transitionFns = map transition nfas
    shiftAmounts = scanl (+) 1 $ map nStates nfas
    shifted = zipWith shiftTransition transitionFns shiftAmounts
    startStates = map succ $ init shiftAmounts
    startTransitions = M.fromList [(('\NUL', 1), S.fromList startStates)]
    acceptStates = tail shiftAmounts
    acceptTransitions =
      M.fromList $ map (\x -> (('\NUL', x), S.singleton nStates')) acceptStates
    allTransitions = startTransitions : acceptTransitions : shifted

-- move to next state
readInput :: NFA -> S.Set Int -> Char -> S.Set Int
readInput nfa currentStates inputChar = S.union newStates epsilons
  where
    newStates = S.fold S.union S.empty $ S.map find currentStates
    find state = M.findWithDefault S.empty (inputChar, state) (transition nfa)
    epsilons = S.fold S.union S.empty $ S.map (epsilonTransitions nfa) newStates

isAccepting :: NFA -> S.Set Int -> Bool
isAccepting nfa states = S.member (nStates nfa) states
