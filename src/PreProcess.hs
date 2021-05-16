{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module PreProcess where

import Control.Applicative
import System.Directory (doesFileExist)
import Data.Char (isSpace)
import Data.List (intercalate)

import qualified Data.Map as M

import Lexeme (CLexeme(..))
import Scanner (ScanItem(..), scanCLine, scanCCode)
import Utils (Error(..), Location, Filename, errorLoc, errorMsg)
import IR (evaluateConstantExpression)
import Parser (Parser(..), cConstantExpressionP)
import ParseItem (ParseItem(..))
import Config (libraryIncludePath)

type Line = [ScanItem CLexeme]

-----------
-- UTILS --
-----------

isPPDirectiveLine :: Line -> Bool
isPPDirectiveLine line =
  not (null line) && scanItem (head line) `elem` ppLexemes

scanCLineNoWS :: Location -> String -> Either Error [ScanItem CLexeme]
scanCLineNoWS c input = removeWhitespace <$> scanCLine c input

evaluateMacroFunction :: [String] -> Line -> [Line] -> String
evaluateMacroFunction argNames body args =
  concatMap scanStr replaced
  where
    replaceArg line (name, replaceLine) =
      concatMap
        (\item ->
           case scanItem item of
             (LLabel label) ->
               if label == name
                 then let precedingWS = takeWhile isSpace $ scanStr item
                          tailingWS =
                            reverse . takeWhile isSpace . reverse . scanStr $ item
                       in [ScanItem { scanLoc = ("", (0, 0)), scanStr = precedingWS, scanItem = LWhiteSpace }]
                          ++ replaceLine
                          ++ [ScanItem { scanLoc = ("", (0, 0)), scanStr = tailingWS, scanItem = LWhiteSpace }]
                 else [item]
             _ -> [item])
        line
    replaced = foldl replaceArg body (zip argNames args)

readMacroArgs :: Location -> Line -> Either Error ([Line], Line)
readMacroArgs c line =
  case lineTail of
    [] -> Left $ PreProcessError c "error in macro function invocation"
    (item:nextItem:rest) ->
      let trailingWhiteSpace = reverse . takeWhile isSpace . reverse . scanStr $ item
       in return ( splitArgs argString
                 , nextItem { scanStr = trailingWhiteSpace ++ scanStr nextItem }:rest
                 )
    [_] -> return (splitArgs argString, [])
  where
    argString = takeWhile ((/= LParenthesisClose) . scanItem) line
    lineTail = dropWhile ((/= LParenthesisClose) . scanItem) line
    splitArgs argLine =
      if null lineTail'
        then [arg]
        else arg : splitArgs (tail lineTail')
      where
        arg = takeWhile ((/= LComma) . scanItem) argLine
        lineTail' = dropWhile ((/= LComma) . scanItem) argLine

readMacro :: Location -> Line -> Either Error ([String], Line)
readMacro c line =
  case line of
    (ScanItem { scanItem = LLabel varName }:ScanItem { scanItem = LComma }:lineTail) -> do
      (varNames, lineTail') <- readMacro c lineTail
      return (varName:varNames, lineTail')
    (ScanItem { scanItem = LLabel varName }:ScanItem { scanItem = LParenthesisClose }:lineTail) -> do
      return ([varName], lineTail)
    (ScanItem { scanItem = LParenthesisClose }:lineTail) ->
      return ([], lineTail)
    _ -> Left $ PreProcessError c "Error parsing a macro definition"

substituteDefined :: Context -> Line -> Either Error Line
substituteDefined _ [] = return []
substituteDefined
     ctx (ScanItem {scanLoc = l, scanItem = LNot}
          :ScanItem {scanItem = LLabel "defined"}
          :rest) =
  case rest of
    (ScanItem {scanItem = LLabel name}:toSubstitute) -> do
      let evaluated =
            case M.lookup name (macroSymbols ctx) of
              Just _ ->
                ScanItem { scanLoc = l
                         , scanStr = " 0 "
                         , scanItem = LIntLiteral 0
                         }
              Nothing ->
                ScanItem { scanLoc = l
                         , scanStr = " 1 "
                         , scanItem = LIntLiteral 1
                         }
      lineTail <- substituteDefined ctx toSubstitute
      return (evaluated : lineTail)
    (ScanItem {scanItem = LParenthesisOpen}
     :ScanItem {scanItem = LLabel name}
     :ScanItem {scanItem = LParenthesisClose}
     :toSubstitute) -> do
      let evaluated =
            case M.lookup name (macroSymbols ctx) of
              Just _ ->
                ScanItem { scanLoc = l
                         , scanStr = " 0 "
                         , scanItem = LIntLiteral 0
                         }
              Nothing ->
                ScanItem { scanLoc = l
                         , scanStr = " 1 "
                         , scanItem = LIntLiteral 1
                         }
      lineTail <- substituteDefined ctx toSubstitute
      return (evaluated : lineTail)
    _ ->
      Left . PreProcessError l $ "error parsing a not defined condition"
substituteDefined
     ctx (ScanItem {scanLoc = l, scanItem = LLabel "defined"}
          :rest) =
  case rest of
    (ScanItem {scanItem = LLabel name}:toSubstitute) -> do
      let evaluated =
            case M.lookup name (macroSymbols ctx) of
              Just _ ->
                ScanItem { scanLoc = l
                         , scanStr = " 1 "
                         , scanItem = LIntLiteral 1
                         }
              Nothing ->
                ScanItem { scanLoc = l
                         , scanStr = " 0 "
                         , scanItem = LIntLiteral 0
                         }
      lineTail <- substituteDefined ctx toSubstitute
      return (evaluated : lineTail)
    (ScanItem {scanItem = LParenthesisOpen}
     :ScanItem {scanItem = LLabel name}
     :ScanItem {scanItem = LParenthesisClose}
     :toSubstitute) -> do
      let evaluated =
            case M.lookup name (macroSymbols ctx) of
              Just _ ->
                ScanItem { scanLoc = l
                         , scanStr = " 1 "
                         , scanItem = LIntLiteral 1
                         }
              Nothing ->
                ScanItem { scanLoc = l
                         , scanStr = " 0 "
                         , scanItem = LIntLiteral 0
                         }
      lineTail <- substituteDefined ctx toSubstitute
      return (evaluated : lineTail)
    _ ->
      Left . PreProcessError l $ "error parsing a not defined condition"
substituteDefined ctx (item:rest) = do
  lineTail <- substituteDefined ctx rest
  return (item : lineTail)

-- 1. replace defined & !defined conditions with 1 (if true) or 0 (if false)
-- 2. expand remaining macros
-- 3. parse transformed expression as a constant expression
-- 4. evaluate the constant expression
evaluateIfCondition ::
     Context -> Line -> Either Error Bool
evaluateIfCondition ctx [] =
  Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) $
    "empty if condition"
evaluateIfCondition ctx line = do
  let endMarker = ScanItem ("", (0, 0)) "" LEndMarker
  toTransform <- substituteDefined ctx line
  toParse <-
    macroTransform (scanLoc $ head line) (macroSymbols ctx) toTransform
  (notParsed, constExpr) <-
    runParser cConstantExpressionP (toParse ++ [endMarker])
  if notParsed == [endMarker]
    then do
      result <- evaluateConstantExpression (parseItem constExpr)
      return (result /= 0)
    else
      Left . PreProcessError (scanLoc $ head notParsed) $
        "unable to parse an if expression"

ltrim :: String -> String
ltrim = dropWhile isSpace

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse

-------------------
-- PRE-TRANSFORM --
-------------------

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

------------
-- PARSER --
------------

ppLexemes :: [CLexeme]
ppLexemes =
  [ LPPDefine
  , LPPUndef
  , LPPInclude
  , LPPIf
  , LPPIfdef
  , LPPIfndef
  , LPPElif
  , LPPElse
  , LPPLine
  , LPPError
  , LPPPragma
  , LPPEmpty
  ]

newtype PPParser a =
  PPParser
    { runPPParser :: [Line] -> Either Error ([Line], a)
    }

instance Functor PPParser where
  fmap fab pa = PPParser $ (fmap . fmap) fab . runPPParser pa

instance Applicative PPParser where
  pure x = PPParser $ \input -> Right (input, x)
  p1 <*> p2 =
    PPParser $ \input -> do
      (input', fab) <- runPPParser p1 input
      (input'', a) <- runPPParser p2 input'
      return (input'', fab a)

instance Monad PPParser where
  return = pure
  pa >>= famb = PPParser $ \input -> do
    (input', a) <- runPPParser pa input
    let pb = famb a
    runPPParser pb input'

instance Alternative PPParser where
  empty = PPParser $ \_ -> Left $ PreProcessError ("", (0, 0)) "Error"
  p1 <|> p2 = PPParser $ \input ->
    case runPPParser p1 input of
      r@(Right _) -> r
      Left e1 ->
        case runPPParser p2 input of
          r@(Right _) -> r
          Left e2 ->
            if errorLoc e1 >= errorLoc e2
              then Left e1
              else Left e2

newtype PPTranslationUnit = PPTranslationUnit [PPSourceLine]
  deriving (Eq, Show)

-- line in source code
data PPSourceLine
  = PPSourceLineCodeLine Line
  | PPSourceLineDirective PPDirective
  deriving (Eq, Show)

-- line starting with #
data PPDirective
  = PPDirectiveDefine PPDefine
  | PPDirectiveUndef PPUndef
  | PPDirectiveInclude PPInclude
  | PPDirectiveIf PPIf
  | PPDirectiveLine PPLine
  | PPDirectiveError PPError
  | PPDirectivePragma
  | PPDirectiveEmpty
  deriving (Eq, Show)

data PPDefine
  -- #define varname String
  = PPDefineConst String String
  -- #define varname (arglist) body
  | PPDefineMacro String [String] Line
  deriving (Eq, Show)

newtype PPUndef
  -- #undef varname
  = PPUndef String
  deriving (Eq, Show)

data PPInclude
  -- #include <something.h>
  = PPIncludeLibrary String
  -- #include "something.h"
  | PPIncludeInternal String
  -- #include [something else]
  | PPIncludeMacro Line
  deriving (Eq, Show)

data PPIf
  -- #ifdef String [PPSourceLine] (Maybe PPElif) #endif
  = PPIfdef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #ifndef String [PPSourceLine] (Maybe PPElif) #endif
  | PPIfndef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #if Line [PPSourceLine] (Maybe PPElif) (Maybe PPElse) #endif
  | PPIf Line [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  deriving (Eq, Show)

data PPElif
  -- #elif Line [PPSourceLine] (Maybe PPElif)
  = PPElif Line [PPSourceLine] (Maybe PPElif)
  deriving (Eq, Show)

newtype PPElse
  -- #else [PPSourceLine]
  = PPElse [PPSourceLine]
  deriving (Eq, Show)

data PPLine
  -- #line linenum
  = PPLine Int
  -- #line linenum filename
  | PPLineFileName Int String
  -- #line anything else
  | PPLineMacro Line
  deriving (Eq, Show)

data PPError
  -- #error error message
  = PPError Error
  -- #error line to be transformed
  | PPErrorMacro Line
  deriving (Eq, Show)

-- TODO: handle file names properly
nonEmptyParser :: PPParser a -> PPParser a
nonEmptyParser p = PPParser $ \input ->
  if null input
    then Left $ PreProcessError ("", (1, 1)) "Unexpected EOF"
    else runPPParser p input

nonEmptyLineParser :: PPParser a -> PPParser a
nonEmptyLineParser p = nonEmptyParser $ PPParser $ \input@(line:_) ->
  if null line
    then Left $ PreProcessError ("", (1, 1)) "Unexpected empty line"
    else runPPParser p input

translationUnitParser :: PPParser PPTranslationUnit
translationUnitParser = PPParser $ \input ->
  if null input
    then return ([], PPTranslationUnit [])
    else do
      (input', line) <- runPPParser sourceLineParser input
      (input'', PPTranslationUnit rest) <-
        runPPParser translationUnitParser input'
      return (input'', PPTranslationUnit $ line:rest)

sourceLineParser :: PPParser PPSourceLine
sourceLineParser = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  if isPPDirectiveLine line
    then runPPParser (PPSourceLineDirective <$> directiveParser) input
    else return (rest, PPSourceLineCodeLine line)

directiveParser :: PPParser PPDirective
directiveParser =
  PPDirectiveDefine <$> defineParser
  <|> PPDirectiveUndef <$> undefParser
  <|> PPDirectiveInclude <$> includeParser
  <|> PPDirectiveIf <$> ifParser
  <|> PPDirectiveLine <$> lineParser
  <|> PPDirectiveError <$> errorParser
  <|> PPDirectivePragma <$ pragmaParser
  <|> PPDirectiveEmpty <$ emptyParser

isMacroFunction :: Location -> Line -> Either Error Bool
isMacroFunction l line = isMacroFunction' l 1 line

isMacroFunction' :: Location -> Int -> Line -> Either Error Bool
isMacroFunction' _ 0 [] = return False
isMacroFunction' _ 0 _ = return True
isMacroFunction' l _ [] =
  Left . PreProcessError l $ "unmatched parentheses in a define directive"
isMacroFunction' l n (ScanItem { scanItem = LParenthesisOpen }:rest) =
  isMacroFunction' l (n + 1) rest
isMacroFunction' l n (ScanItem { scanItem = LParenthesisClose }:rest) =
  isMacroFunction' l (n - 1) rest
isMacroFunction' l n (_:rest) =
  isMacroFunction' l n rest


defineParser :: PPParser PPDefine
defineParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case line of
    [ ScanItem { scanItem = LPPDefine }, ScanItem { scanItem = LLabel varName }] ->
      return (rest, PPDefineConst varName "")
    (ScanItem { scanItem = LPPDefine }:ScanItem { scanItem = LLabel varName }:lineTail) ->
      case lineTail of
        [] -> return (rest, PPDefineConst varName "")
        (ScanItem { scanLoc = l, scanItem = LParenthesisOpen }:listTail) -> do
          isFunction <- isMacroFunction l listTail
          if isFunction
            then do
              (argNames, body) <- readMacro (scanLoc $ head line) listTail
              return (rest, PPDefineMacro varName argNames body)
            else
              return (rest, PPDefineConst varName (concatMap scanStr lineTail))
        l -> return (rest, PPDefineConst varName . concatMap scanStr $ l)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

undefParser :: PPParser PPUndef
undefParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPUndef, LLabel name] -> return (rest, PPUndef name)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

includeParser :: PPParser PPInclude
includeParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPInclude && length line > 1
    then return (rest, PPIncludeMacro $ tail line)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

ifParser :: PPParser PPIf
ifParser = ififParser <|> ifdefParser <|> ifndefParser

ififParser :: PPParser PPIf
ififParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPIf
    then do
      (input', (body, ppelif, ppelse)) <- runPPParser ifBodyParser rest
      return (input', PPIf (tail line) body ppelif ppelse)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

ifdefParser :: PPParser PPIf
ifdefParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPIfdef, LLabel name] -> do
      (input', (body, ppelif, ppelse)) <- runPPParser ifBodyParser rest
      return (input', PPIfdef name body ppelif ppelse)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

ifBodyParser :: PPParser ([PPSourceLine], Maybe PPElif, Maybe PPElse)
ifBodyParser = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  case map scanItem line of
    (LPPEndif:_) -> return (rest, ([], Nothing, Nothing))
    (LPPElif:_) -> do
      (input', ppelif) <- runPPParser elifParser input
      (input'', (_, _, ppelse)) <- runPPParser ifBodyParser input'
      return (input'', ([], Just ppelif, ppelse))
    (LPPElse:_) -> do
      (input', ppelse) <- runPPParser elseParser rest
      return (input', ([], Nothing, Just ppelse))
    _ -> do
      (input', l) <- runPPParser sourceLineParser input
      (input'', (l', ppelif, ppelse)) <- runPPParser ifBodyParser input'
      return (input'', (l:l', ppelif, ppelse))

elifParser :: PPParser PPElif
elifParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if (scanItem . head $ line) == LPPElif
    then do
      (input', lns) <- runPPParser elifBodyParser rest
      if (scanItem . head . head $ input') == LPPElif
        then do
          (input'', ppelif) <- runPPParser elifParser input'
          return (input'', PPElif (tail line) lns (Just ppelif))
        else
          return (input', PPElif (tail line) lns Nothing)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

elifBodyParser :: PPParser [PPSourceLine]
elifBodyParser = nonEmptyParser $ PPParser $ \input@(line:_) ->
  case map scanItem line of
    (LPPEndif:_) ->
      return (input, [])
    (LPPElif:_) ->
      return (input, [])
    (LPPElse:_) ->
      return (input, [])
    _ -> do
      (input', line') <- runPPParser sourceLineParser input
      (input'', lns') <- runPPParser elifBodyParser input'
      return (input'', line':lns')

elseParser :: PPParser PPElse
elseParser = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  if map scanItem line == [LPPEndif]
    then return (rest, PPElse [])
    else do
      (input', line') <- runPPParser sourceLineParser input
      (input'', PPElse lns') <- runPPParser elseParser input'
      return (input'', PPElse $ line':lns')

ifndefParser :: PPParser PPIf
ifndefParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPIfndef, LLabel name] -> do
      (input', (body, ppelif, ppelse)) <- runPPParser ifBodyParser rest
      return (input', PPIfndef name body ppelif ppelse)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

lineParser :: PPParser PPLine
lineParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPLine
    then
      case tail $ map scanItem line of
        [] ->
          Left $
            PreProcessError
              (scanLoc (head line))
              "No arguments given to line directive"
        ((LIntLiteral x):(LStringLiteral fName):lineTail) ->
          if null lineTail
            then return (rest, PPLineFileName x fName)
            else return (rest, PPLineMacro $ tail line)
        ((LIntLiteral x):lineTail) ->
          if null lineTail
            then return (rest, PPLine x)
            else return (rest, PPLineMacro $ tail line)
        _ ->
          return (rest, PPLineMacro $ tail line)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

errorParser :: PPParser PPError
errorParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPError
    then return (rest, PPErrorMacro $ tail line)
    else Left $
           PreProcessError
             (scanLoc $ head line)
             $ "Error trying to parse a preprocess directive: "
               ++ concatMap scanStr line

pragmaParser :: PPParser ()
pragmaParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPPragma
    then return (rest, ())
    else Left $
           PreProcessError
             (scanLoc $ head line)
             $ "Error trying to parse a preprocess directive: "
               ++ concatMap scanStr line

emptyParser :: PPParser ()
emptyParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPEmpty] -> return (rest, ())
    -- ignore warnings
    (LPPEmpty:LLabel "warning":_) -> return (rest, ())
    _ -> Left $
           PreProcessError
             (scanLoc $ head line)
             $ "Error trying to parse a preprocess directive: "
               ++ concatMap scanStr line

------------
-- MACRO --
------------

data Macro
  = MacroConstant String
  | MacroFunction [String] Line
  deriving (Eq, Show)

type MacroDict = M.Map String Macro

-- transform macro symbols contained in the line
macroTransform :: Location -> MacroDict -> Line -> Either Error Line
macroTransform _ _ [] = return []
macroTransform c m line = do
  macroTransformed <- macroTransform' m line
  newLine <- scanCLineNoWS c macroTransformed
  if newLine == line
    then do
      concatenated <- concatTokens newLine
      if concatenated == line
        then return concatenated
        else macroTransform c m concatenated
    else do
      macroTransform c m newLine

macroTransform' :: MacroDict -> Line -> Either Error String
macroTransform' _ [] = return []
macroTransform' m (lItem@ScanItem { scanStr = s, scanItem = LLabel label }:rest) =
  case M.lookup label m of
    Nothing -> do
      lineTail <- macroTransform' m rest
      return $ s ++ lineTail
    -- use label as a placeholder
    Just (MacroConstant replaceStr) -> do
      lineTail <- macroTransform' m rest
      return $ takeWhile isSpace s
               ++ replaceStr
               ++ (reverse . takeWhile isSpace . reverse $ s)
               ++ lineTail
    Just (MacroFunction args body) ->
      case map scanItem rest of
        (LParenthesisOpen:_) -> do
          (args', rest') <- readMacroArgs (scanLoc lItem) (tail rest)
          if length args == length args'
            then do
              lineTail <- macroTransform' m rest'
              return $ evaluateMacroFunction args body args' ++ lineTail
            else
              Left $ PreProcessError
                       (scanLoc lItem)
                       "wrong number of args passed to a macro function"
        _ ->
          Left $ PreProcessError
                   (scanLoc lItem)
                   "no arguments given to a macro function"
macroTransform' m (item:rest) = do
  lineTail <- macroTransform' m rest
  return $ scanStr item ++ lineTail

-- process concat directives
concatTokens :: Line -> Either Error Line
concatTokens [] = return []
concatTokens line
  | LPPConcat `notElem` map scanItem line = return line
  | null beforeConcat || null afterConcat
  = Left . PreProcessError (scanLoc $ head line)
      $ "invalid use of concat directive"
  | otherwise
  = scanCLineNoWS (scanLoc $ head line)
                  (rtrim (concatMap scanStr beforeConcat)
                   ++ ltrim (concatMap scanStr afterConcat))
  where
    beforeConcat = takeWhile ((/= LPPConcat) . scanItem) line
    afterConcat = tail $ dropWhile ((/= LPPConcat) . scanItem) line

--------------------
-- TRANSFORM CODE --
--------------------

data Context = Context { fileName :: String
                       , lineNum :: Int
                       , macroSymbols :: MacroDict
                       }
                       deriving (Show, Eq)

type PreProcessResult = IO (Either Error (Context, [Line]))

type PPTransform a = Context -> a -> PreProcessResult

incrementLineNum :: Context -> Context
incrementLineNum ctx = ctx { lineNum = lineNum ctx + 1 }

setLineContext :: Line -> Context -> Line
setLineContext line ctx =
  map (\item@ScanItem { scanLoc = (_, (_, col)) } ->
          item { scanLoc = (fileName ctx, (lineNum ctx, col)) })
      line

preProcessCode :: String -> String -> IO (Either Error Line)
preProcessCode fName sourceCode = do
  print ("preprocessing " ++ fName)
  ppResults <- preProcessCode' initialContext sourceCode
  return $ concat . snd <$> ppResults
  where
    initialContext =
      Context
        { fileName = fName
        , lineNum = 1
        -- TODO: read documentation and find out the minimal required set of predefined macros
        , macroSymbols =
            M.fromList
              [ ("__GNUC__", MacroConstant "-1")
              , ("__STDC__", MacroConstant "")
              , ("__LDOUBLE_REDIRECTS_TO_FLOAT128_ABI", MacroConstant "0")
              ]
        }

preProcessCode' :: PPTransform String
preProcessCode' ctx sourceCode =
  case parsed of
    Right ([], tUnit) ->
      ppTransformTranslationUnit ctx tUnit
    Right _ ->
      return . Left . InternalError (fileName ctx, (1, 1)) $ "preprocessor failed parsing source code"
    Left e ->
      return . Left $ e
  where
    preTransformed = preTransform (fileName ctx) sourceCode
    parsed = preTransformed >>= runPPParser translationUnitParser

-- TODO: monad transformers
ppTransformTranslationUnit :: PPTransform PPTranslationUnit
ppTransformTranslationUnit ctx (PPTranslationUnit []) = return . return $ (ctx, [])
ppTransformTranslationUnit ctx (PPTranslationUnit (line:rest)) =  do
  transformedLine <- ppTransformSourceLine ctx line
  case transformedLine of
    Right (ctx', transformed) -> do
      listTail <- ppTransformTranslationUnit ctx' (PPTranslationUnit rest)
      case listTail of
        Right (ctx'', transformed') ->
          return . return $ (ctx'', transformed ++ transformed')
        e -> return e
    e -> return e

ppTransformSourceLine :: PPTransform PPSourceLine
ppTransformSourceLine ctx (PPSourceLineCodeLine line) = do
  print "transforming a source line"
  case macroTransform (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) (setLineContext line ctx) of
    Left e -> return . Left $ e
    Right line' -> return . return $ (incrementLineNum ctx, [line'])
ppTransformSourceLine ctx (PPSourceLineDirective directive) =
  ppTransformDirective ctx directive

ppTransformDirective :: PPTransform PPDirective
ppTransformDirective ctx (PPDirectiveDefine ppDefine) =
  ppTransformDefine ctx ppDefine
ppTransformDirective ctx (PPDirectiveUndef ppUndef) =
  ppTransformUndef ctx ppUndef
ppTransformDirective ctx (PPDirectiveInclude ppInclude) =
  ppTransformInclude ctx ppInclude
ppTransformDirective ctx (PPDirectiveIf ppIf) =
  ppTransformIf ctx ppIf
ppTransformDirective ctx (PPDirectiveLine ppLine) =
  ppTransformLine ctx ppLine
ppTransformDirective ctx (PPDirectiveError ppError) =
  ppTransformError ctx ppError
ppTransformDirective ctx PPDirectivePragma =
  return . return $ (incrementLineNum ctx, [])
ppTransformDirective ctx PPDirectiveEmpty =
  return . return $ (incrementLineNum ctx, [])

ppTransformDefine :: PPTransform PPDefine
ppTransformDefine ctx (PPDefineConst name value) = do
  print "transforming a macro definition"
  return . return
    $ ( ctx { macroSymbols = M.insert name
                                     (MacroConstant value)
                                     (macroSymbols ctx)
            , lineNum = lineNum ctx + 1
            }
      , []
      )
ppTransformDefine ctx (PPDefineMacro name args body) = do
  print "transforming a macro definition"
  return . return
    $ ( ctx { macroSymbols = M.insert name
                                     (MacroFunction args body)
                                     (macroSymbols ctx)
            , lineNum = lineNum ctx + 1
            }
      , []
      )

ppTransformUndef :: PPTransform PPUndef
ppTransformUndef ctx (PPUndef name) = do
  print "transforming an undef directive"
  return . return
    $ ( ctx { macroSymbols = M.delete name (macroSymbols ctx)
            , lineNum = lineNum ctx + 1
            }
      , []
      )

ppTransformInclude :: PPTransform PPInclude
ppTransformInclude ctx (PPIncludeLibrary name) = do
  print ("preprocessing <" ++ name ++ ">")
  exists <- traverse doesFileExist (map (++ name) headerFileDirs)
  let containingDirs = map snd $ filter fst (zip exists headerFileDirs)
  if not (null containingDirs)
    then do
      contents <- readFile (head containingDirs ++ name)
      preProcessResult <-
        preProcessCode'
          Context { lineNum = 1
                  , fileName = name
                  , macroSymbols = macroSymbols ctx
                  }
          contents
      case preProcessResult of
        Right (ctx', includedLines) -> do
          print ("done preprocessing " ++ name)
          return . return $ (incrementLineNum (ctx { macroSymbols = macroSymbols ctx' }), includedLines)
        e -> return e
    else
      return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) $
        "header file "
        ++ name
        ++ " doesn't exist. Searched following directories: "
        ++ intercalate ", " headerFileDirs
  where
    headerFileDirs =
      [ "/usr/include/"
      , "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include/"
      ]
ppTransformInclude ctx (PPIncludeInternal name) = do
  print ("preprocessing " ++ name)
  exists <- doesFileExist name
  if exists
    then do
      contents <- readFile name
      preProcessResult <-
        preProcessCode'
          Context { lineNum = 1
                  , fileName = name
                  , macroSymbols = macroSymbols ctx
                  }
          contents
      case preProcessResult of
        Right (ctx', includedLines) -> do
          print ("done preprocessing " ++ name)
          return . return $
            ( incrementLineNum (ctx { macroSymbols = macroSymbols ctx' })
            , includedLines
            )
        e -> return e
    else
      return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) $ "header file " ++ name ++ " doesn't exist"
ppTransformInclude ctx (PPIncludeMacro line) =
  case macroTransform (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
    Left e -> return . Left $ e
    Right line' ->
      case line' of
        [ScanItem { scanItem = LStringLiteral name }] ->
          ppTransformInclude ctx $ PPIncludeInternal name
        (ScanItem { scanItem = LLT } : rest) ->
          if (scanItem . last $ rest) == LGT
            then
              ppTransformInclude ctx . PPIncludeLibrary
                $ concatMap scanStr . init $ rest
            else
              return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1))
                $ "error parsing include directive"
        _ ->
          return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1))
            $ "error parsing include directive"

ppTransformElif :: Maybe PPElse -> PPTransform (Maybe PPElif)
ppTransformElif Nothing ctx Nothing = do
  print "transforming elif"
  return . return $ (incrementLineNum ctx, [])
ppTransformElif (Just (PPElse elseBody)) ctx Nothing = do
  print "transforming elif"
  ppTransformTranslationUnit
    (incrementLineNum ctx)
    (PPTranslationUnit elseBody)
ppTransformElif ppelse ctx (Just (PPElif line body ppelif)) = do
  print "transforming elif"
  case evaluateIfCondition ctx line of
    Right result ->
      if result
        then
          ppTransformTranslationUnit
            (incrementLineNum ctx)
            (PPTranslationUnit body)
        else
          ppTransformElif
            ppelse
            ctx { lineNum = lineNum ctx + 1 + length body }
            ppelif
    Left e ->
      return . Left . PreProcessError (errorLoc e) $ (errorMsg e) -- ++ " ctx: " ++ show ctx

ppTransformIf :: PPTransform PPIf
ppTransformIf ctx (PPIfdef name body ppelif ppelse) = do
  print "transforming if"
  case M.lookup name (macroSymbols ctx) of
    Just _ ->
      ppTransformTranslationUnit ctx . PPTranslationUnit $ body
    Nothing ->
      ppTransformElif ppelse ctx ppelif
ppTransformIf ctx (PPIfndef name body ppelif ppelse) = do
  print "transforming if"
  case M.lookup name (macroSymbols ctx) of
    Just _ ->
      ppTransformElif ppelse ctx ppelif
    Nothing ->
      ppTransformTranslationUnit ctx . PPTranslationUnit $ body
ppTransformIf ctx (PPIf conditionLine body ppelif ppelse) = do
  print "transforming if"
  case evaluateIfCondition ctx conditionLine of
    Right result ->
      if result
        then
          ppTransformTranslationUnit ctx . PPTranslationUnit $ body
        else
          ppTransformElif ppelse ctx ppelif
    Left e ->
      return . Left . PreProcessError (errorLoc e) $ (errorMsg e) -- ++ " ctx: " ++ show ctx

ppTransformLine :: PPTransform PPLine
ppTransformLine ctx (PPLine n) = do
  print "transforming line"
  return . return $ (ctx { lineNum = n + 1 }, [])
ppTransformLine ctx (PPLineFileName n name) = do
  print "transforming line"
  return . return $ (ctx { lineNum = n + 1, fileName = name }, [])
ppTransformLine ctx (PPLineMacro line) = do
  print "transforming line"
  case macroTransform (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
    Right [ScanItem { scanItem = LIntLiteral n }] ->
      ppTransformLine ctx (PPLine $ n + 1)
    Right (ScanItem { scanItem = LIntLiteral n }:rest) ->
      ppTransformLine ctx (PPLineFileName n (concatMap scanStr rest))
    Right _ ->
      return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1))
        $ "illegal line preprocessor directive"
    Left e -> return . Left $ e

ppTransformError :: PPTransform PPError
ppTransformError _ (PPError e) = return . Left $ e
ppTransformError ctx (PPErrorMacro line) = do
  print "transforming error"
  case macroTransform (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
    Right line' ->
      return .  Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) . concatMap scanStr $ line'
    Left e -> return . Left $ e

