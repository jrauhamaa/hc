{-# LANGUAGE TupleSections #-}

module PreProcess where

import Control.Applicative
import System.Directory (doesFileExist)
import Data.Char (isSpace)

import qualified Data.Map as M

import Lexeme (CLexeme(..))
import Scanner (ScanItem(..), scanCLine)
import Utils (Error(..), Coordinates, errorLoc)
import IR (evaluateConstantIntExpression)
import Parser (Parser(..), cConstantExpressionP)
import ParseItem (ParseItem(..))
import Config (libraryIncludePath)

---------------
-- DATATYPES --
---------------


type Line = [ScanItem CLexeme]
type Path = String

data Macro
  = MacroConstant String
  | MacroFunction [String] Line
  deriving (Eq, Show)

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
  empty = PPParser $ \_ -> Left $ PreProcessError (0, 0) "Error"
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
  -- #if Line [PPSourceLine] (Maybe PPElif) #endif
  = PPIf Line [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #ifdef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse) #endif
  | PPIfdef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #ifndef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse) #endif
  | PPIfndef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
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

-------------------
-- PRE-TRANSFORM --
-------------------

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
      if last a == '\\'
        then lineSplice' $ (init a ++ " " ++ b):rest
        else a : lineSplice' (b : rest)

readLines :: String -> Either Error [Line]
readLines sourceCode =
  let sourceLines = lineSplice $ trigraph sourceCode
  in traverse
       (\(lineNum, line) -> scanCLine (lineNum, 1) line)
       (zip [1..] sourceLines)

-- replace comments with single spaces
removeComments :: Line -> Line
removeComments [] = []
removeComments (ScanItem { scanLoc = c, scanItem = LComment }:lineTail) =
  (ScanItem { scanLoc = c
            , scanItem = LWhiteSpace
            , scanStr = " " }) : removeComments lineTail
removeComments (item:lineTail) = item : removeComments lineTail

-- merge whitespace tokens with bordering tokens
removeWhitespace :: Line -> Line
removeWhitespace [] = []
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

-----------
-- UTILS --
-----------

scanCLineNoWS :: Coordinates -> String -> Either Error [ScanItem CLexeme]
scanCLineNoWS c input = removeWhitespace <$> scanCLine c input

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

evaluateMacroFunction :: [String] -> Line -> [Line] -> Line
evaluateMacroFunction argNames body args =
  foldl
  (\line (name, arg) ->
    concatMap
      (\item ->
         case scanItem item of
           (LLabel label) ->
             if label == name
               then arg
               else [item]
           _ ->
             [item])
      line)
    body
    (zip argNames args)

readMacroArgs :: Coordinates -> Line -> Either Error ([Line], Line)
readMacroArgs c line =
  if null lineTail
    then Left $ PreProcessError c "error in macro function invocation"
    else
      return (splitArgs argString, tail lineTail)
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

readMacro :: Coordinates -> Line -> Either Error ([String], Line)
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
     Coordinates -> MacroDict -> Line -> Either Error Bool
evaluateIfCondition c _ [] =
  Left $ PreProcessError c "if expression doesn't have a condition"
evaluateIfCondition c m (ScanItem { scanItem = LLabel "defined" }:rest) =
  case rest of
    [ScanItem { scanItem = LLabel name }] ->
      case M.lookup name m of
        Just _ -> return True
        Nothing -> return False
    _ ->
      Left $ PreProcessError c "illegal if defined condition"
evaluateIfCondition
  c m (ScanItem { scanItem = LNot }
       :ScanItem { scanItem = LLabel "defined" }
       :rest ) =
  case rest of
    [ScanItem { scanItem = LLabel name }] ->
      case M.lookup name m of
        Just _ -> return False
        Nothing -> return True
    _ ->
      Left $ PreProcessError c "illegal if defined condition"
evaluateIfCondition c m line = do
  transformed <- macroTransform m line
  if transformed == line
    then do
      (_, notParsed, constExpr) <- runParser cConstantExpressionP transformed
      if null notParsed
        then do
          result <- evaluateConstantIntExpression $ parseItem constExpr
          return (result /= 0)
        else
          Left $ PreProcessError c "failed to evaluate if condition"
    else
      evaluateIfCondition c m transformed

-------------
-- PARSERS --
-------------

nonEmptyParser :: PPParser a -> PPParser a
nonEmptyParser p = PPParser $ \input ->
  if null input
    then Left $ PreProcessError (1, 1) "Unexpected EOF"
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

isPPDirectiveLine :: Line -> Bool
isPPDirectiveLine line =
  not (null line) && scanItem (head line) `elem` ppLexemes

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

defineParser :: PPParser PPDefine
defineParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  case line of
    [ ScanItem { scanItem = LPPDefine }, ScanItem { scanItem = LLabel varName }] ->
      return (rest, PPDefineConst varName "")
    (ScanItem { scanItem = LPPDefine }:ScanItem { scanItem = LLabel varName }:lineTail) ->
      case lineTail of
        [] -> return (rest, PPDefineConst varName "")
        (ScanItem { scanItem = LParenthesisOpen }:listTail) -> do
          (argNames, body) <- readMacro (scanLoc $ head line) listTail
          return (rest, PPDefineMacro varName argNames body)
        l -> return (rest, PPDefineConst varName . concatMap scanStr $ l)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

undefParser :: PPParser PPUndef
undefParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPUndef, LLabel name] -> return (rest, PPUndef name)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

includeParser :: PPParser PPInclude
includeParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPInclude
    then return (rest, PPIncludeMacro $ tail line)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

ifParser :: PPParser PPIf
ifParser = ififParser <|> ifdefParser <|> ifndefParser

ififParser :: PPParser PPIf
ififParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  case line of
    (ScanItem { scanItem = LPPIf }
     : ScanItem { scanItem = LLabel "defined" }
     : lineTail) ->
       runPPParser
         ifdefParser $
         ((ScanItem { scanItem = LPPIfdef
                    , scanLoc = scanLoc $ head line
                    , scanStr = "" }
          ):lineTail
         ):rest
    (ScanItem { scanItem = LPPIf }
     : ScanItem { scanItem = LNot }
     : ScanItem { scanItem = LLabel "defined" }
     : lineTail) ->
       runPPParser
         ifndefParser $
         ((ScanItem { scanItem = LPPIfndef
                    , scanLoc = scanLoc $ head line
                    , scanStr = "" }
          ):lineTail
         ):rest
    (ScanItem { scanItem = LPPIf } : lineTail) -> do
       (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
       return (input', PPIf lineTail body ppelif ppelse)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

ifdefParser :: PPParser PPIf
ifdefParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPIfdef, LLabel varName] -> do
      (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
      return (input', PPIfndef varName body ppelif ppelse)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

readIfBody :: PPParser ([PPSourceLine], Maybe PPElif, Maybe PPElse)
readIfBody = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  case scanItem $ head line of
    LPPEndif -> return (rest, ([], Nothing, Nothing))
    LPPElif -> do
      (input', ppelif) <- runPPParser readElif input
      (input'', (_, _, ppelse)) <- runPPParser readIfBody input'
      return (input'', ([], Just ppelif, ppelse))
    LPPElse -> do
      (input', ppelse) <- runPPParser readElse rest
      return (input', ([], Nothing, Just ppelse))
    _ -> do
      (input', l) <- runPPParser sourceLineParser input
      (input'', (l', ppelif, ppelse)) <- runPPParser readIfBody input'
      return (input'', (l:l', ppelif, ppelse))

readElif :: PPParser PPElif
readElif = nonEmptyParser $ PPParser $ \(line:rest) ->
  if (scanItem . head $ line) == LPPElif
    then do
      (input', lns) <- runPPParser readElifBody rest
      if (scanItem . head . head $ input') == LPPElif
        then do
          (input'', ppelif) <- runPPParser readElif input'
          return (input'', PPElif (tail line) lns $ Just ppelif)
        else
          return (input', PPElif (tail line) lns Nothing)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

readElifBody :: PPParser [PPSourceLine]
readElifBody = nonEmptyParser $ PPParser $ \input@(line:_) ->
  case scanItem . head $ line of
    LPPEndif ->
      return (input, [])
    LPPElif ->
      return (input, [])
    LPPElse ->
      return (input, [])
    _ -> do
      (input', line') <- runPPParser sourceLineParser input
      (input'', lns') <- runPPParser readElifBody input'
      return (input'', line':lns')

readElse :: PPParser PPElse
readElse = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  if (scanItem . head $ line) == LPPEndif
    then return (rest, PPElse [])
    else do
      (input', line') <- runPPParser sourceLineParser input
      (input'', PPElse lns') <- runPPParser readElse input'
      return (input'', PPElse $ line':lns')

ifndefParser :: PPParser PPIf
ifndefParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  case map scanItem line of
    [LPPIfndef, LLabel varName] -> do
      (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
      return (input', PPIfndef varName body ppelif ppelse)
    _ ->
      Left $
        PreProcessError
          (scanLoc $ head line)
          "Error trying to parse a preprocess directive"

lineParser :: PPParser PPLine
lineParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPLine
    then
      case tail $ map scanItem line of
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
          "Error trying to parse a preprocess directive"

errorParser :: PPParser PPError
errorParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPPragma
    then return (rest, PPErrorMacro $ tail line)
    else Left $
           PreProcessError
             (scanLoc $ head line)
             "Error trying to parse a preprocess directive"

pragmaParser :: PPParser ()
pragmaParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPPragma
    then return (rest, ())
    else Left $
           PreProcessError
             (scanLoc $ head line)
             "Error trying to parse a preprocess directive"

emptyParser :: PPParser ()
emptyParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if map scanItem line == [LPPEmpty]
    then return (rest, ())
    else Left $
           PreProcessError
             (scanLoc $ head line)
             "Error trying to parse a preprocess directive"

--------------------
-- TRANSFORM CODE --
--------------------

preTransform :: String -> Either Error [Line]
preTransform sourceCode =
  noWhiteSpace
  where
    preTransformed = lineSplice . trigraph $ sourceCode
    withCoordinates = zip (map (, 1) [1..]) preTransformed
    scanned = traverse (uncurry scanCLine) withCoordinates
    noComments = map removeComments <$> scanned
    noWhiteSpace = map removeWhitespace <$> noComments

preProcessCode :: MacroDict -> String -> PreProcessResult
preProcessCode m sourceCode =
  case parsed of
    Right ([], tUnit) ->
      ppTransformTranslationUnit (1, 1) m tUnit
    Right _ ->
      return . Left . InternalError (1, 1) $ "preprocessor failed parsing source code"
    Left e ->
      return . Left $ e
  where
    preTransformed = preTransform sourceCode
    parsed = preTransformed >>= runPPParser translationUnitParser

ltrim :: String -> String
ltrim = dropWhile isSpace

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse

concatTokens :: Line -> Either Error Line
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

macroTransform :: MacroDict -> Line -> Either Error Line
macroTransform _ [] = return []
macroTransform m line = do
  macroTransformed <- macroTransform' m line
  concatenated <- concatTokens macroTransformed
  if concatenated == line
    then return concatenated
    else do
      line' <-
        scanCLineNoWS
          (scanLoc $ head line)
          (concatMap scanStr concatenated)
      macroTransform m line'

macroTransform' :: MacroDict -> Line -> Either Error Line
macroTransform' _ [] = return []
macroTransform' m (lItem@ScanItem { scanItem = LLabel label }:rest) =
  case M.lookup label m of
    Nothing -> do
      lineTail <- macroTransform' m rest
      return $ lItem : lineTail
    -- use label as a placeholder
    Just (MacroConstant s) -> do
      lineTail <- macroTransform' m rest
      return $ (lItem { scanItem = LLabel s }) : lineTail
    Just (MacroFunction args body) ->
      if scanItem (head rest) == LParenthesisOpen
        then do
          (args', rest') <- readMacroArgs (scanLoc lItem) (tail rest)
          if length args == length args'
            then do
              lineTail <- macroTransform m rest'
              return $ evaluateMacroFunction args body args' ++ lineTail
            else
              Left $ PreProcessError
                       (scanLoc lItem)
                       "wrong number of args passed to a macro function"
        else
          Left $ PreProcessError
                   (scanLoc lItem)
                   "no arguments given to a macro function"
macroTransform' m (item:rest) = do
  lineTail <- macroTransform' m rest
  return $ item : lineTail

type MacroDict = M.Map String Macro

type PreProcessResult = IO (Either Error (MacroDict, [Line]))

type PPTransform a =
     Coordinates
  -> MacroDict
  -> a
  -> PreProcessResult

{-
ppTransformTranslationUnit :: PPTransform PPTranslationUnit
ppTransformTranslationUnit _ m (PPTranslationUnit []) = return (m, [])
ppTransformTranslationUnit c m (PPTranslationUnit (line:rest)) = do
  (m', translated) <- ppTransformSourceLine c m line
  (m'', translated') <- ppTransformTranslationUnit c m' (PPTranslationUnit rest)
  return (m'', translated ++ translated')
-}

ppTransformTranslationUnit :: PPTransform PPTranslationUnit
ppTransformTranslationUnit _ m (PPTranslationUnit []) = return . return $ (m, [])
ppTransformTranslationUnit c m (PPTranslationUnit (line:rest)) =  do
  transformedLine <- ppTransformSourceLine c m line
  case transformedLine of
    Right (m', transformed) -> do
      listTail <- ppTransformTranslationUnit c m' (PPTranslationUnit rest)
      case listTail of
        Right (m'', transformed') ->
          return . return $ (m'', transformed ++ transformed')
        e -> return e
    e -> return e

ppTransformSourceLine :: PPTransform PPSourceLine
ppTransformSourceLine _ m (PPSourceLineCodeLine line) =
  case macroTransform m line of
    Left e -> return . Left $ e
    Right line' -> return . return $ (m, [line'])
ppTransformSourceLine c m (PPSourceLineDirective directive) =
  ppTransformDirective c m directive

ppTransformDirective :: PPTransform PPDirective
ppTransformDirective c m (PPDirectiveDefine ppDefine) =
  ppTransformDefine c m ppDefine
ppTransformDirective c m (PPDirectiveUndef ppUndef) =
  ppTransformUndef c m ppUndef
ppTransformDirective c m (PPDirectiveInclude ppInclude) =
  ppTransformInclude c m ppInclude
ppTransformDirective c m (PPDirectiveIf ppIf) =
  ppTransformIf c m ppIf
ppTransformDirective c m (PPDirectiveLine ppLine) =
  ppTransformLine c m ppLine
ppTransformDirective c m (PPDirectiveError ppError) =
  ppTransformError c m ppError
ppTransformDirective _ m PPDirectivePragma =
  return . return $ (m, [])
ppTransformDirective _ m PPDirectiveEmpty =
  return . return $ (m, [])

ppTransformDefine :: PPTransform PPDefine
ppTransformDefine _ m (PPDefineConst name value) =
  return . return $ (M.insert name (MacroConstant value) m, [])
ppTransformDefine _ m (PPDefineMacro name args body) =
  return . return $ (M.insert name (MacroFunction args body) m, [])

ppTransformUndef :: PPTransform PPUndef
ppTransformUndef _ m (PPUndef name) =
  return . return $ (M.delete name m, [])

ppTransformInclude :: PPTransform PPInclude
ppTransformInclude c m (PPIncludeLibrary name) = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      preProcessCode m contents
    else
      return . Left . PreProcessError c $ "header file " ++ path ++ " doesn't exist"
  where path = libraryIncludePath ++ name
ppTransformInclude c m (PPIncludeInternal name) = do
  exists <- doesFileExist name
  if exists
    then do
      contents <- readFile name
      preProcessCode m contents
    else
      return . Left . PreProcessError c $ "header file " ++ name ++ " doesn't exist"
ppTransformInclude c m (PPIncludeMacro line) =
  case macroTransform m line of
    Left e -> return . Left $ e
    Right line' ->
      if line /= line'
        then
          ppTransformInclude c m $ PPIncludeMacro line'
        else
          case line of
            [ScanItem { scanItem = LStringLiteral name }] ->
              ppTransformInclude c m $ PPIncludeInternal name
            (ScanItem { scanItem = LLT } : rest) ->
              if (scanItem . last $ rest) == LGT
                then
                  ppTransformInclude c m . PPIncludeLibrary
                    $ concatMap scanStr . init $ rest
                else
                  return . Left . PreProcessError c
                    $ "error parsing include directive"
            _ ->
              return . Left . PreProcessError c
                $ "error parsing include directive"

ppTransformElif :: Maybe PPElse -> PPTransform (Maybe PPElif)
ppTransformElif Nothing _ m Nothing =
  return . return $ (m, [])
ppTransformElif (Just (PPElse elseBody)) c m Nothing =
  ppTransformTranslationUnit c m $ PPTranslationUnit elseBody
ppTransformElif ppelse c m (Just (PPElif condition body ppelif)) =
  case evaluateIfCondition c m condition of
    Left e -> return . Left $ e
    Right conditionValue ->
      if conditionValue
        then ppTransformTranslationUnit c m $ PPTranslationUnit body
        else ppTransformElif ppelse c m ppelif

ppTransformIf :: PPTransform PPIf
ppTransformIf c m (PPIf condition body ppelif ppelse) =
  case evaluateIfCondition c m condition of
    Left e -> return . Left $ e
    Right conditionValue ->
      if conditionValue
        then ppTransformTranslationUnit c m . PPTranslationUnit $ body
        else ppTransformElif ppelse c m ppelif
ppTransformIf c m (PPIfdef name body ppelif ppelse) =
  case M.lookup name m of
    Just _ -> ppTransformTranslationUnit c m $ PPTranslationUnit body
    Nothing -> ppTransformElif ppelse c m ppelif
ppTransformIf c m (PPIfndef name body ppelif ppelse) =
  case M.lookup name m of
    Nothing -> ppTransformTranslationUnit c m $ PPTranslationUnit body
    Just _ -> ppTransformElif ppelse c m ppelif

ppTransformLine :: PPTransform PPLine
ppTransformLine = undefined

ppTransformError :: PPTransform PPError
ppTransformError _ _ (PPError e) = return . Left $ e
ppTransformError c m (PPErrorMacro line) =
  case macroTransform m line of
    Right line' ->
      return .  Left . PreProcessError c . concatMap scanStr $ line'
    Left e -> return . Left $ e

