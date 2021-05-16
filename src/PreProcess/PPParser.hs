module PreProcess.PPParser where

{- A recursive descent parser for the C preprocessor.
   This is implemented separately from the main parser since the preprocessor
   reads the code a line at the time rather than token at the time.  -}

import Control.Applicative (Alternative(..))

import Lexeme (CLexeme(..))
import Scanner (ScanItem(..), Line)
import Utils (Location, Error(..), errorLoc)

{- Read a list of lines (lists of lexemes) and return a tuple of
   unprocessed lines and the first preprocess unit (a preprocess directive or
   a line of plain C code). -}
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
  -- Run the first parser. If it fails, run the second.
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

------------------
-- AST ELEMENTS --
------------------

-- The root of the syntax tree
newtype PPTranslationUnit = PPTranslationUnit [PPSourceLine]
  deriving (Eq, Show)

-- A preprocess directive or a line of C code
data PPSourceLine
  = PPSourceLineCodeLine Line
  | PPSourceLineDirective PPDirective
  deriving (Eq, Show)

-- A line starting with #
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

-- #define
data PPDefine
  -- #define varname String
  = PPDefineConst String String
  -- #define varname (arglist) body
  | PPDefineMacro String [String] Line
  deriving (Eq, Show)

-- #undef
newtype PPUndef
  = PPUndef String
  deriving (Eq, Show)

-- #include
data PPInclude
  -- #include <something.h>
  = PPIncludeLibrary String
  -- #include "something.h"
  | PPIncludeInternal String
  -- #include <something else to be macro-expanded into one of the above forms>
  | PPIncludeMacro Line
  deriving (Eq, Show)

-- #ifdef, #ifndef or #if
data PPIf
  -- #ifdef <name of the macro> <if body> <optional elif> <optional else> #endif
  = PPIfdef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #ifndef <name of the macro> <if body> <optional elif> <optional else> #endif
  | PPIfndef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #if <condition> <if body> <optional elif> <optional else> #endif
  | PPIf Line [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  deriving (Eq, Show)

-- #elif
data PPElif
  -- #elif <condition> <body> <optional second elif>
  = PPElif Line [PPSourceLine] (Maybe PPElif)
  deriving (Eq, Show)

-- #else
newtype PPElse
  -- #else <body>
  = PPElse [PPSourceLine]
  deriving (Eq, Show)

data PPLine
  -- #line <line number>
  = PPLine Int
  -- #line <line number> <filename>
  | PPLineFileName Int String
  -- #line <something else to be macro-expanded into one of the above forms>
  | PPLineMacro Line
  deriving (Eq, Show)

data PPError
  -- #error error message
  = PPError Error
  -- #error <something to be macro-expanded into an error message>
  | PPErrorMacro Line
  deriving (Eq, Show)

------------
-- PARSER --
------------

-- Does the line contain a preprocessor directive
isPPDirectiveLine :: Line -> Bool
isPPDirectiveLine line =
  not (null line) && scanItem (head line) `elem` ppDirectiveLexemes
  where
    ppDirectiveLexemes =
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

{- Read a macro function definition. Return a list of argument names and the
   function body. The function body is a list of tokens. When called, tokens
   that are labels corresponding to the argument names are replaced with the
   respective argument values.  -}
readMacroFunction :: Location -> Line -> Either Error ([String], Line)
readMacroFunction l [] =
  Left . PreProcessError l $ "Error parsing a macro definition"
readMacroFunction l line = readMacroFunction' l (tail line)

readMacroFunction' :: Location -> Line -> Either Error ([String], Line)
readMacroFunction' l line =
  case line of
    -- arglist consisting of more than 1 arg
    (ScanItem { scanItem = LLabel varName }
     :ScanItem { scanItem = LComma }
     :lineTail) -> do
      (varNames, lineTail') <- readMacroFunction' l lineTail
      return (varName:varNames, lineTail')
    -- the last argument in the arglist
    (ScanItem { scanItem = LLabel varName }
     :ScanItem { scanItem = LParenthesisClose }
     :lineTail) -> do
      return ([varName], lineTail)
    -- a function with no args
    (ScanItem { scanItem = LParenthesisClose }
     :lineTail) ->
      return ([], lineTail)
    _ -> Left $ PreProcessError l "Error parsing a macro definition"

-- A parser that doesn't accept empty input
-- TODO: handle file names properly
nonEmptyParser :: PPParser a -> PPParser a
nonEmptyParser p = PPParser $ \input ->
  if null input
    then Left $ PreProcessError ("", (1, 1)) "Unexpected EOF"
    else runPPParser p input

-- A parser that doesn't accept empty line as the first input line
nonEmptyLineParser :: PPParser a -> PPParser a
nonEmptyLineParser p = nonEmptyParser $ PPParser $ \input@(line:_) ->
  if null line
    then Left $ PreProcessError ("", (1, 1)) "Unexpected empty line"
    else runPPParser p input

-- The main parser
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

{- Is given macro definition a definition of a macro constant or a macro
   function. Takes as input the line defining the macro starting from the
   first open parenthesis. The input is considered a macro function definition
   if the first closed parenthesis (the end of the parameter list) is followed
   by any token (the start of the function body).  -}
isMacroFunction :: Line -> Bool
isMacroFunction line = isMacroFunction' (tail line)

isMacroFunction' :: Line -> Bool
isMacroFunction' [] = False
isMacroFunction' (ScanItem { scanItem = LParenthesisOpen }:_) = False
isMacroFunction' [ScanItem { scanItem = LParenthesisClose }] = False
isMacroFunction' (ScanItem { scanItem = LParenthesisClose }:_) = True
isMacroFunction' (_:rest) =
  isMacroFunction' rest


defineParser :: PPParser PPDefine
defineParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  case line of
    -- empty definition
    [ ScanItem { scanItem = LPPDefine },
      ScanItem { scanItem = LLabel varName } ] ->
      return (rest, PPDefineConst varName "")
    -- a const or function definition
    (ScanItem { scanItem = LPPDefine }
     :ScanItem { scanItem = LLabel varName }
     :definitionBody) ->
      case definitionBody of
        (ScanItem { scanLoc = l, scanItem = LParenthesisOpen }:_) -> do
          if isMacroFunction definitionBody
            then do
              (argNames, body) <- readMacroFunction l definitionBody
              return (rest, PPDefineMacro varName argNames body)
            else
              return
                ( rest
                , PPDefineConst
                    varName
                    (concatMap scanStr definitionBody)
                )
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

-- parse all include directives as ones that need to be macro-expanded
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

-- #if
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

-- #ifdef
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

-- #ifndef
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

-- Parse the body of the if directive & possible elif & else directives
ifBodyParser :: PPParser ([PPSourceLine], Maybe PPElif, Maybe PPElse)
ifBodyParser = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  case map scanItem line of
    (LPPEndif:_) -> return (rest, ([], Nothing, Nothing))
    (LPPElif:_) -> do
      (input', ppelif) <- runPPParser elifParser input
      -- at this point, input starts with either #else or #endif
      (input'', (_, _, ppelse)) <- runPPParser ifBodyParser input'
      return (input'', ([], Just ppelif, ppelse))
    (LPPElse:_) -> do
      (input', ppelse) <- runPPParser elseParser rest
      return (input', ([], Nothing, Just ppelse))
    -- a line that's a part of the if body
    _ -> do
      (input', l) <- runPPParser sourceLineParser input
      (input'', (l', ppelif, ppelse)) <- runPPParser ifBodyParser input'
      return (input'', (l:l', ppelif, ppelse))

{- Parse a sequence of elif conditions, until encouter a line starting with
   #else or #endif -}
elifParser :: PPParser PPElif
elifParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if (scanItem . head $ line) == LPPElif
    then do
      (input', body) <- runPPParser elifBodyParser rest
      if (scanItem . head . head $ input') == LPPElif
        then do
          (input'', ppelif) <- runPPParser elifParser input'
          return (input'', PPElif (tail line) body (Just ppelif))
        else
          return (input', PPElif (tail line) body Nothing)
    else
      Left $
        PreProcessError
          (scanLoc $ head line)
          $ "Error trying to parse a preprocess directive: "
            ++ concatMap scanStr line

{- Parse elif body, until encounter a line starting with
   #elif, #else or #endif -}
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

-- Parse else body, until encounter a line starting with #endif
elseParser :: PPParser PPElse
elseParser = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  if map scanItem line == [LPPEndif]
    then return (rest, PPElse [])
    else do
      (input', line') <- runPPParser sourceLineParser input
      (input'', PPElse lns') <- runPPParser elseParser input'
      return (input'', PPElse $ line':lns')

-- #line
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

-- #error
errorParser :: PPParser PPError
errorParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPError
    then return (rest, PPErrorMacro $ tail line)
    else Left $
           PreProcessError
             (scanLoc $ head line)
             $ "Error trying to parse a preprocess directive: "
               ++ concatMap scanStr line

-- #pragma. Ignore pragmas altogether.
pragmaParser :: PPParser ()
pragmaParser = nonEmptyLineParser $ PPParser $ \(line:rest) ->
  if scanItem (head line) == LPPPragma
    then return (rest, ())
    else Left $
           PreProcessError
             (scanLoc $ head line)
             $ "Error trying to parse a preprocess directive: "
               ++ concatMap scanStr line

-- # or #warning
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

