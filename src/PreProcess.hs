{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module PreProcess where

import Control.Monad (foldM)
import Control.Applicative
import System.Directory (doesFileExist)
import Data.Char (isSpace)

import qualified Data.Map as M

import Lexeme (CLexeme(..))
import Scanner (ScanItem(..), scanCLine, scanCCode)
import Utils (Error(..), Location, Filename, errorLoc)
import IR (evaluateConstantExpression)
import Parser (Parser(..), cConstantExpressionP)
import ParseItem (CConstantExpression, ParseItem(..))
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

evaluateIfCondition ::
     Context -> PPIfConditionTernary -> Either Error Bool
evaluateIfCondition ctx (PPIfConditionTernary orCondition Nothing) =
  evaluateIfConditionOr ctx orCondition
evaluateIfCondition
     ctx (PPIfConditionTernary
            orCondition
            (Just (orCondition', ternaryCondition))) = do
  condition <- evaluateIfConditionOr ctx orCondition
  if condition
    then evaluateIfConditionOr ctx orCondition'
    else evaluateIfCondition ctx ternaryCondition

evaluateIfConditionOr ::
     Context -> PPIfConditionOr -> Either Error Bool
evaluateIfConditionOr ctx (PPIfConditionOr andConditions) =
  if not (null andConditions)
    then
      foldM
        (\acc current ->
          if acc
            then
              return True
            else
              evaluateIfConditionAnd ctx current)
        False
        andConditions
    else
      Left . PreProcessError (fileName ctx, (lineNum ctx, 1))
        $ "empty if condition"

evaluateIfConditionAnd ::
     Context -> PPIfConditionAnd -> Either Error Bool
evaluateIfConditionAnd ctx (PPIfConditionAnd expressions) =
  if not (null expressions)
    then
      foldM
        (\acc current -> do
          if acc
            then
              evaluateIfConditionExpression ctx current
            else
              return False)
        True
        expressions
    else
      Left . PreProcessError (fileName ctx, (lineNum ctx, 1))
        $ "empty if condition"

evaluateIfConditionExpression ::
     Context -> PPIfConditionExpression -> Either Error Bool
evaluateIfConditionExpression ctx (PPIfConditionDefined name) =
  case M.lookup name (macroSymbols ctx) of
    Just _ -> return True
    _ -> return False
evaluateIfConditionExpression ctx (PPIfConditionNotDefined name) =
  case M.lookup name (macroSymbols ctx) of
    Nothing -> return True
    _ -> return False
evaluateIfConditionExpression _ (PPIfConditionExpression expr) = do
  result <- evaluateConstantExpression expr
  return (result /= 0)
evaluateIfConditionExpression ctx (PPIfConditionParenthesized expr) =
  evaluateIfCondition ctx expr

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
  -- #if PPIfConditionTernary [PPSourceLine] (Maybe PPElif) #endif
  = PPIf PPIfConditionTernary [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- not macro-expanded
  | PPIfMacro Line [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  deriving (Eq, Show)

-- optional ternary expression
data PPIfConditionTernary =
  PPIfConditionTernary
    PPIfConditionOr
    (Maybe (PPIfConditionOr, PPIfConditionTernary))
  deriving (Eq, Show)

newtype PPIfConditionOr =
  PPIfConditionOr [PPIfConditionAnd]
  deriving (Eq, Show)

newtype PPIfConditionAnd =
  PPIfConditionAnd [PPIfConditionExpression]
  deriving (Eq, Show)

data PPIfConditionExpression
  = PPIfConditionDefined String
  | PPIfConditionNotDefined String
  | PPIfConditionExpression CConstantExpression
  | PPIfConditionParenthesized PPIfConditionTernary
  deriving (Eq, Show)

data PPElif
  -- #elif Line [PPSourceLine] (Maybe PPElif)
  = PPElif PPIfConditionTernary [PPSourceLine] (Maybe PPElif)
  -- not macro-expanded
  | PPElifMacro Line [PPSourceLine] (Maybe PPElif)
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

isMacroFunction :: Line -> Bool
isMacroFunction line = isMacroFunction' 1 line

isMacroFunction' :: Int -> Line -> Bool
isMacroFunction' 0 [] = False
isMacroFunction' 0 _ = True
isMacroFunction' _ [] = False
isMacroFunction' n (ScanItem { scanItem = LParenthesisOpen }:rest) =
  isMacroFunction' (n + 1) rest
isMacroFunction' n (ScanItem { scanItem = LParenthesisClose }:rest) =
  isMacroFunction' (n - 1) rest
isMacroFunction' n (_:rest) =
  isMacroFunction' n rest


defineParser :: PPParser PPDefine
defineParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case line of
    [ ScanItem { scanItem = LPPDefine }, ScanItem { scanItem = LLabel varName }] ->
      return (rest, PPDefineConst varName "")
    (ScanItem { scanItem = LPPDefine }:ScanItem { scanItem = LLabel varName }:lineTail) ->
      case lineTail of
        [] -> return (rest, PPDefineConst varName "")
        (ScanItem { scanItem = LParenthesisOpen }:listTail) ->
          if isMacroFunction listTail
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

parseIfCondition :: MacroDict -> Line -> Either Error PPIfConditionTernary
parseIfCondition m line = do
  (_, notParsed, orCondition) <-
    runParser (parseIfConditionTernary m) line
  if null notParsed
    then return orCondition
    else
      Left . PreProcessError (scanLoc $ head notParsed) $
        "error parsing if condition: notparsed: " ++ concatMap scanStr notParsed ++ "\n line : " ++ concatMap scanStr line ++ "\n result: " ++ show orCondition

parseIfConditionTernary :: MacroDict -> Parser PPIfConditionTernary
parseIfConditionTernary m =
  Parser $ \input -> do
    (parsed, notParsed, orCondition) <- runParser (parseIfConditionOr m) input
    if not (null notParsed) && scanItem (head notParsed) == LTernary
      then do
        (parsed', notParsed', orCondition') <-
          runParser (parseIfConditionOr m) (tail notParsed)
        if not (null notParsed') && scanItem (head notParsed') == LColon
          then do
            (parsed'', notParsed'', ternaryCondition) <-
              runParser (parseIfConditionTernary m) (tail notParsed')
            return ( parsed ++ [head notParsed] ++ parsed' ++ [head notParsed'] ++ parsed''
                   , notParsed''
                   , PPIfConditionTernary orCondition (Just (orCondition', ternaryCondition))
                   )
          else
            if null input
              then Left . PreProcessError ("", (0, 0)) $ "unexpected EOL"
              else
                Left . PreProcessError (scanLoc (head input)) $
                  "Error parsing a ternary if condition: " ++ concatMap show notParsed'
      else
        return (parsed, notParsed, PPIfConditionTernary orCondition Nothing)



parseIfConditionOr :: MacroDict -> Parser PPIfConditionOr
parseIfConditionOr m =
  Parser $ \input -> do
    (parsed, notParsed, andCondition) <- runParser (parseIfConditionAnd m) input
    case notParsed of
      (ScanItem { scanItem = LOr }:_) -> do
        (parsed', notParsed', PPIfConditionOr andConditions)
          <- runParser (parseIfConditionOr m) (tail notParsed)
        return ( parsed ++ parsed'
               , notParsed'
               , PPIfConditionOr (andCondition : andConditions)
               )
      _ ->
        return (parsed, notParsed, PPIfConditionOr [andCondition])

parseIfConditionAnd :: MacroDict -> Parser PPIfConditionAnd
parseIfConditionAnd m =
  Parser $ \input -> do
    (parsed, notParsed, expression) <- runParser (parseIfConditionExpression m) input
    case notParsed of
      (ScanItem { scanItem = LAnd }:_) -> do
        (parsed', notParsed', PPIfConditionAnd expressions)
          <- runParser (parseIfConditionAnd m) (tail notParsed)
        return ( parsed ++ parsed'
               , notParsed'
               , PPIfConditionAnd (expression : expressions)
               )
      _ ->
        return (parsed, notParsed, PPIfConditionAnd [expression])

parseIfConditionExpression :: MacroDict -> Parser PPIfConditionExpression
parseIfConditionExpression m =
  parseIfDefined m <|>
  parseIfNotDefined m <|>
  parseIfConstantExpression m <|>
  parseIfParenthesized m

parseIfDefined :: MacroDict -> Parser PPIfConditionExpression
parseIfDefined _ =
  Parser $ \case
    input@(ScanItem {scanItem = LLabel "defined"}
           :ScanItem {scanItem = LLabel name}
           :rest
           ) ->
      return (take 2 input, rest, PPIfConditionDefined name)
    [] ->
      Left . PreProcessError ("", (0, 0)) $ "unexpected EOF"
    (item:_) ->
      Left . PreProcessError (scanLoc item) $
        "error trying to parse if defined condition"

parseIfNotDefined :: MacroDict -> Parser PPIfConditionExpression
parseIfNotDefined _ =
  Parser $ \case
    input@(ScanItem {scanItem = LNot}
           :ScanItem {scanItem = LLabel "defined"}
           :ScanItem {scanItem = LLabel name}
           :rest
           ) ->
      return (take 3 input, rest, PPIfConditionNotDefined name)
    [] ->
      Left . PreProcessError ("", (0, 0)) $ "unexpected EOF"
    (item:_) ->
      Left . PreProcessError (scanLoc item) $
        "error trying to parse if not defined condition"

parseIfParenthesized :: MacroDict -> Parser PPIfConditionExpression
parseIfParenthesized m =
  Parser $ \case
    [] ->
      Left . PreProcessError ("", (0, 0)) $ "unexpected EOF"
    input@(ScanItem {scanItem = LParenthesisOpen}
           :rest
           ) -> do
      (parsed, notParsed, ternaryCondition) <-
        runParser (parseIfConditionTernary m) rest
      case notParsed of
        (ScanItem {scanItem = LParenthesisClose}:_) ->
          return ( take 1 input ++ parsed ++ take 1 notParsed
                 , tail notParsed
                 , PPIfConditionParenthesized ternaryCondition
                 )
        [] ->
          Left . PreProcessError ("", (0, 0)) $ "unexpected EOF"
        (item:_) ->
          Left . PreProcessError (scanLoc item) $
            "error trying to parse if condition expression: notparsed:" ++ concatMap scanStr notParsed ++ "\n defined: " ++ concatMap (++ ", ") (M.keys m)
    (item:_) ->
      Left . PreProcessError (scanLoc item) $
        "error trying to parse if condition expression"

minimalTransform :: Location -> MacroDict -> Line -> Line -> Either Error Line
minimalTransform l m transformed toTransform =
  foldl
    (\acc (t, nt) ->
      case acc of
        Left _ -> do
          transformed' <- macroTransform l m t
          if transformed' == transformed
            then Right nt
            else Left . PreProcessError l $ "error expanding a macro"
        x -> x)
    (Left (PreProcessError l ""))
    splits
  where
    splits = map (`splitAt` toTransform) [0 .. (length toTransform)]

parseIfConstantExpression :: MacroDict -> Parser PPIfConditionExpression
parseIfConstantExpression m =
  Parser $ \case
    [] ->
      Left . PreProcessError ("", (0, 0)) $ "unexpected EOF"
    input -> do
      let l = scanLoc $ head input
          terminateParse = [LOr, LAnd, LTernary, LColon]
          maximumParse = takeWhile ((`notElem` terminateParse) . scanItem) input
          afterMaximumParse = dropWhile ((`notElem` terminateParse) . scanItem) input
          endMarker = ScanItem { scanLoc = ("", (0, 0))
                               , scanStr = ""
                               , scanItem = LEndMarker
                               }
      transformed <- macroTransform l m maximumParse
      (parsed, _, constExpr) <-
        runParser cConstantExpressionP
          $ transformed ++ [endMarker]
      afterParse <- minimalTransform l m parsed maximumParse
      return ( parsed
             , afterParse ++ afterMaximumParse
             , PPIfConditionExpression (parseItem constExpr)
             )

{-
parseIfConstantExpression :: MacroDict -> Parser PPIfConditionExpression
parseIfConstantExpression m =
  Parser $ \case
    [] ->
      Left . PreProcessError ("", (0, 0)) $ "unexpected EOF"
    input -> do
      let terminateParse = [LOr, LAnd, LParenthesisClose]
          toParse = takeWhile ((`notElem` terminateParse) . scanItem) input
          afterParse = dropWhile ((`notElem` terminateParse) . scanItem) input
          l = scanLoc $ head input
      transformed <- macroTransform l m toParse
      (_, notParsed, constExpr) <-
        runParser cConstantExpressionP
          $ transformed ++ [ScanItem { scanLoc = ("", (0, 0))
                                    , scanStr = ""
                                    , scanItem = LEndMarker
                                    }]
      if map scanItem notParsed == [LEndMarker]
        then
          return ( toParse
                 , afterParse
                 , PPIfConditionExpression (parseItem constExpr)
                 )
        else
          Left . PreProcessError l $ "error parsing an if expression: notparsed: " ++ concatMap scanStr notParsed

parseIfCondition :: Location -> MacroDict -> Line -> Either Error PPIfConditionOr
parseIfCondition l _ [] = Left . PreProcessError l $ "empty if condition"
parseIfCondition l m line = do
  (notParsed, condition) <- parseIfConditionOr l m line
  if null notParsed
    then return condition
    else
      Left . PreProcessError l $ "error parsing an if condition"

parseIfConditionOr :: Location -> MacroDict -> Line -> Either Error (Line, PPIfConditionOr)
parseIfConditionOr l _ [] = Left . PreProcessError l $ "empty if condition"
parseIfConditionOr l m line = do
  (rest, andCondition) <- parseIfConditionAnd l m line
  case rest of
    (ScanItem { scanItem = LOr }:_) -> do
      (rest', PPIfConditionOr andConditions) <- parseIfConditionOr l m (tail rest)
      return (rest', PPIfConditionOr (andCondition : andConditions))
    _ ->
      return (rest, PPIfConditionOr [andCondition])

parseIfConditionAnd :: Location -> MacroDict -> Line -> Either Error (Line, PPIfConditionAnd)
parseIfConditionAnd l _ [] = Left . PreProcessError l $ "empty if condition"
parseIfConditionAnd l m line = do
  (rest, expression) <- parseIfConditionExpression l m line
  case rest of
    (ScanItem { scanItem = LAnd }:_) -> do
      (rest', PPIfConditionAnd expressions) <- parseIfConditionAnd l m (tail rest)
      return (rest', PPIfConditionAnd (expression : expressions))
    _ ->
      return (rest, PPIfConditionAnd [expression])

parseIfConditionExpression :: Location -> MacroDict -> Line -> Either Error (Line, PPIfConditionExpression)
parseIfConditionExpression l _ [] = Left . PreProcessError l $ "empty if condition"
parseIfConditionExpression l m (ScanItem { scanItem = LParenthesisOpen }:lineTail) = do
  (rest, orCondition) <- parseIfConditionOr l m lineTail
  if not (null rest) && scanItem (head rest) == LParenthesisClose
    then
      return (tail rest, PPIfConditionParenthesized orCondition)
    else
      Left . PreProcessError l $
        "error parsing an if condition expression: rest: " ++ concatMap scanStr rest
parseIfConditionExpression l _ (ScanItem { scanItem = LLabel "defined" }:lineTail) =
  case lineTail of
    (ScanItem { scanItem = LLabel name }:rest) ->
      return (rest, PPIfConditionDefined name)
    _ ->
      Left . PreProcessError l $ "error parsing an if defined condition"
parseIfConditionExpression l _ (ScanItem { scanItem = LNot }
                                :ScanItem { scanItem = LLabel "defined" }
                                :lineTail) =
  case lineTail of
    (ScanItem { scanItem = LLabel name }:rest) ->
      return (rest, PPIfConditionNotDefined name)
    _ ->
      Left . PreProcessError l $ "error parsing an if not defined condition"
parseIfConditionExpression l m line = do
  let terminateParse = [LOr, LAnd, LParenthesisClose]
      toParse = takeWhile ((`notElem` terminateParse) . scanItem) line
      afterParse = dropWhile ((`notElem` terminateParse) . scanItem) line
  transformed <- macroTransform l m toParse
  (_, notParsed, constExpr) <-
    runParser cConstantExpressionP
      $ transformed ++ [ScanItem { scanLoc = ("", (0, 0))
                                , scanStr = ""
                                , scanItem = LEndMarker
                                }]
  if map scanItem notParsed == [LEndMarker]
    then
      return (afterParse, PPIfConditionExpression (parseItem constExpr))
    else
      Left . PreProcessError l $ "error parsing an if expression: notparsed: " ++ concatMap scanStr notParsed
-}

ifParser :: PPParser PPIf
ifParser = ififParser <|> ifdefParser <|> ifndefParser

ififParser :: PPParser PPIf
ififParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPIf
    then do
      (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
      return (input', PPIfMacro (tail line) body ppelif ppelse)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

ifdefParser :: PPParser PPIf
ifdefParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPIfdef, LLabel varName] -> do
      (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
      let condition =
            PPIfConditionTernary
              (PPIfConditionOr
                [PPIfConditionAnd
                  [PPIfConditionDefined varName]])
              Nothing
      return (input', PPIf condition body ppelif ppelse)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

readIfBody :: PPParser ([PPSourceLine], Maybe PPElif, Maybe PPElse)
readIfBody = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  case map scanItem line of
    (LPPEndif:_) -> return (rest, ([], Nothing, Nothing))
    (LPPElif:_) -> do
      (input', ppelif) <- runPPParser readElif input
      (input'', (_, _, ppelse)) <- runPPParser readIfBody input'
      return (input'', ([], Just ppelif, ppelse))
    (LPPElse:_) -> do
      (input', ppelse) <- runPPParser readElse rest
      return (input', ([], Nothing, Just ppelse))
    _ -> do
      (input', l) <- runPPParser sourceLineParser input
      (input'', (l', ppelif, ppelse)) <- runPPParser readIfBody input'
      return (input'', (l:l', ppelif, ppelse))

readElif :: PPParser PPElif
readElif = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if (scanItem . head $ line) == LPPElif
    then do
      (input', lns) <- runPPParser readElifBody rest
      if (scanItem . head . head $ input') == LPPElif
        then do
          (input'', ppelif) <- runPPParser readElif input'
          return (input'', PPElifMacro (tail line) lns $ Just ppelif)
        else
          return (input', PPElifMacro (tail line) lns Nothing)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

readElifBody :: PPParser [PPSourceLine]
readElifBody = nonEmptyParser $ PPParser $ \input@(line:_) ->
  case map scanItem line of
    (LPPEndif:_) ->
      return (input, [])
    (LPPElif:_) ->
      return (input, [])
    (LPPElse:_) ->
      return (input, [])
    _ -> do
      (input', line') <- runPPParser sourceLineParser input
      (input'', lns') <- runPPParser readElifBody input'
      return (input'', line':lns')

readElse :: PPParser PPElse
readElse = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  if map scanItem line == [LPPEndif]
    then return (rest, PPElse [])
    else do
      (input', line') <- runPPParser sourceLineParser input
      (input'', PPElse lns') <- runPPParser readElse input'
      return (input'', PPElse $ line':lns')

ifndefParser :: PPParser PPIf
ifndefParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPIfndef, LLabel varName] -> do
      (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
      let condition =
            PPIfConditionTernary
              (PPIfConditionOr
                [PPIfConditionAnd
                  [PPIfConditionNotDefined varName]])
              Nothing
      return (input', PPIf condition body ppelif ppelse)
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
  ppResults <- preProcessCode' initialContext sourceCode
  return $ concat . snd <$> ppResults
  where
    initialContext =
      Context
        { fileName = fName
        , lineNum = 1
        , macroSymbols = M.fromList [("__GNUC__", MacroConstant "0"), ("__STDC__", MacroConstant "")]
        }
    {-
    initialContext =
      Context
        { fileName = fName
        , lineNum = 1
        , macroSymbols = M.empty
        }
    -}

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
ppTransformSourceLine ctx (PPSourceLineCodeLine line) =
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
ppTransformDefine ctx (PPDefineConst name value) =
  return . return
    $ ( ctx { macroSymbols = M.insert name
                                     (MacroConstant value)
                                     (macroSymbols ctx)
            , lineNum = lineNum ctx + 1
            }
      , []
      )
ppTransformDefine ctx (PPDefineMacro name args body) =
  return . return
    $ ( ctx { macroSymbols = M.insert name
                                     (MacroFunction args body)
                                     (macroSymbols ctx)
            , lineNum = lineNum ctx + 1
            }
      , []
      )

ppTransformUndef :: PPTransform PPUndef
ppTransformUndef ctx (PPUndef name) =
  return . return
    $ ( ctx { macroSymbols = M.delete name (macroSymbols ctx)
            , lineNum = lineNum ctx + 1
            }
      , []
      )

ppTransformInclude :: PPTransform PPInclude
ppTransformInclude ctx (PPIncludeLibrary name) = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      preProcessResult <-
        preProcessCode'
          Context { lineNum = 1
                  , fileName = name
                  , macroSymbols = macroSymbols ctx
                  }
          contents
      case preProcessResult of
        Right (_, includedLines) ->
          return . return $ (incrementLineNum ctx, includedLines)
        e -> return e
    else
      return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) $ "header file " ++ path ++ " doesn't exist"
  where path = libraryIncludePath ++ name
ppTransformInclude ctx (PPIncludeInternal name) = do
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
        Right (ctx', includedLines) ->
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
ppTransformElif Nothing ctx Nothing =
  return . return $ (incrementLineNum ctx, [])
ppTransformElif (Just (PPElse elseBody)) ctx Nothing =
  ppTransformTranslationUnit
    (incrementLineNum ctx)
    (PPTranslationUnit elseBody)
ppTransformElif ppelse ctx (Just (PPElifMacro line body ppelif)) =
  case parseIfCondition (macroSymbols ctx) line of
    Right condition ->
      ppTransformElif ppelse ctx (Just (PPElif condition body ppelif))
    Left e -> return (Left e)
ppTransformElif ppelse ctx (Just (PPElif condition body ppelif)) =
  case evaluateIfCondition ctx condition of
    Left e -> return . Left $ e
    Right conditionValue ->
      if conditionValue
        then ppTransformTranslationUnit
               (incrementLineNum ctx)
               (PPTranslationUnit body)
        else ppTransformElif
               ppelse
               ctx { lineNum = lineNum ctx + 1 + length body }
               ppelif

ppTransformIf :: PPTransform PPIf
ppTransformIf ctx (PPIfMacro conditionLine body ppelif ppelse) =
  if not (null conditionLine)
    then
      let condition = parseIfCondition (macroSymbols ctx) conditionLine
       in case condition of
         Right cond -> ppTransformIf ctx (PPIf cond body ppelif ppelse)
         Left e -> return (Left e)
    else
      return . Left . PreProcessError (fileName ctx, (lineNum ctx, 1))
        $ "empty if condition"

ppTransformIf ctx (PPIf condition body ppelif ppelse) =
  case evaluateIfCondition ctx condition of
    Left e -> return . Left $ e
    Right conditionValue ->
      if conditionValue
        then ppTransformTranslationUnit ctx . PPTranslationUnit $ body
        else ppTransformElif ppelse ctx ppelif

ppTransformLine :: PPTransform PPLine
ppTransformLine ctx (PPLine n) =
  return . return $ (ctx { lineNum = n + 1 }, [])
ppTransformLine ctx (PPLineFileName n name) =
  return . return $ (ctx { lineNum = n + 1, fileName = name }, [])
ppTransformLine ctx (PPLineMacro line) =
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
ppTransformError ctx (PPErrorMacro line) =
  case macroTransform (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
    Right line' ->
      return .  Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) . concatMap scanStr $ line'
    Left e -> return . Left $ e

