module PreProcess.PreTransform where

{- Transformations to the code done before any macro expansions -}

import Scanner ( ScanItem(..)
               , scanCLine
               , scanCCode
               , CLexeme(..)
               )
import Utils ( Error(..)
             , Filename
             )

type Line = [ScanItem CLexeme]

{- 1. replace trigraph sequences
   2. concatenate lines ending with backslash
   3. replace comments with a single whitespace
   4. scan the source code one line at the time
   5. merge whitespace with syntactically significant tokens -}
preTransform :: Filename -> String -> Either Error [Line]
preTransform fName sourceCode =
  noWhiteSpace
  where
    preTransformed = unlines . lineSplice . trigraph $ sourceCode
    lexemes = scanCCode fName preTransformed
    locations = map (\x -> (fName, (x, 1))) [1..]
    noComments = concatMap scanStr . removeComments <$> lexemes
    scanned =
      noComments >>= traverse (uncurry scanCLine) . zip locations . lines
    noWhiteSpace = map removeWhitespace <$> scanned

-- replace trigraph sequences in source string
trigraph :: String -> String
trigraph "" = ""
trigraph ('?':'?':'=':s) = '#' : trigraph s
trigraph ('?':'?':'/':s) = '\\' : trigraph s
trigraph ('?':'?':'\'':s) = '^' : trigraph s
trigraph ('?':'?':'(':s) = '[' : trigraph s
trigraph ('?':'?':')':s) = ']' : trigraph s
trigraph ('?':'?':'!':s) = '|' : trigraph s
trigraph ('?':'?':'<':s) = '{' : trigraph s
trigraph ('?':'?':'>':s) = '}' : trigraph s
trigraph ('?':'?':'-':s) = '~' : trigraph s
trigraph (c:s) = c : trigraph s

-- concat lines ending with backslash
lineSplice :: String -> [String]
lineSplice = lineSplice' . lines
  where
    lineSplice' [] = []
    lineSplice' [s] = [s]
    lineSplice' (a:b:rest) =
      if not (null a) && last a == '\\'
        then lineSplice' $ (init a ++ " " ++ b):rest
        else a : lineSplice' (b : rest)

-- replace comments with single spaces
removeComments :: Line -> Line
removeComments [] = []
removeComments
     (ScanItem { scanStr = comment
               , scanLoc = c
               , scanItem = LComment
               }:lineTail) =
  (ScanItem { scanLoc = c
            , scanItem = LWhiteSpace
            {- replace multiline comments with corresponding number
               of empty lines -}
            , scanStr =
                " " ++ filter (== '\n') comment }) : removeComments lineTail
removeComments (item:lineTail) = item : removeComments lineTail

-- merge whitespace tokens with bordering tokens
removeWhitespace :: Line -> Line
removeWhitespace [] = []
removeWhitespace [ScanItem { scanItem = LWhiteSpace }] = []
removeWhitespace [item] = [item]
removeWhitespace (firstItem:secondItem:rest) =
  case (scanItem firstItem, scanItem secondItem) of
    (LWhiteSpace, _) ->
      removeWhitespace
        $ (secondItem
            { scanStr = scanStr firstItem ++ scanStr secondItem }):rest
    (_, LWhiteSpace) ->
      removeWhitespace
        $ (firstItem
            { scanStr = scanStr firstItem ++ scanStr secondItem }):rest
    _ -> firstItem : removeWhitespace (secondItem : rest)
