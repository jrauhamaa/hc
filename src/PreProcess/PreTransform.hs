module PreProcess.PreTransform where

import Scanner (ScanItem(..), scanCLine, scanCCode)
import Lexeme (CLexeme(..))
import Utils (Error(..), Location, Filename)

type Line = [ScanItem CLexeme]

preTransform :: Filename -> String -> Either Error [Line]
preTransform fName sourceCode =
  noWhiteSpace
  where
    preTransformed = unlines . lineSplice . trigraph $ sourceCode
    lexemes = scanCCode fName preTransformed
    locations = map (\x -> (fName, (x, 1))) [1..]
    noComments = concatMap scanStr . removeComments <$> lexemes
    scanned = noComments >>= traverse (uncurry scanCLine) . zip locations . lines
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

readLines :: Filename -> String -> Either Error [Line]
readLines fName sourceCode =
  let sourceLines = lineSplice $ trigraph sourceCode
  in traverse
       (\(num, line) -> scanCLine (fName, (num, 1)) line)
       (zip [1..] sourceLines)

-- replace comments with single spaces
removeComments :: Line -> Line
removeComments [] = []
removeComments (ScanItem { scanStr = comment, scanLoc = c, scanItem = LComment }:lineTail) =
  (ScanItem { scanLoc = c
            , scanItem = LWhiteSpace
            -- replace multiline comments with corresponding number of empty lines
            , scanStr = " " ++ filter (== '\n') comment }) : removeComments lineTail
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
        $ (secondItem { scanStr = scanStr firstItem ++ scanStr secondItem }):rest
    (_, LWhiteSpace) ->
      removeWhitespace
        $ (firstItem { scanStr = scanStr firstItem ++ scanStr secondItem }):rest
    _ -> firstItem : removeWhitespace (secondItem : rest)
