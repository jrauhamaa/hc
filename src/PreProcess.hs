module PreProcess where

import Control.Applicative
import qualified Data.Map as M

import Lexeme (CLexeme(..))
import Scanner (ScanItem(..), scanCLine)
import Utils (Error(..), Coordinates, errorLoc)

---------------
-- DATATYPES --
---------------

type Line = [ScanItem CLexeme]
type Path = String

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
            if (errorLoc e1) >= (errorLoc e2)
              then Left e1
              else Left e2

data PPTranslationUnit = PPTranslationUnit [PPSourceLine]

-- line in source code
data PPSourceLine
  = PPSourceLineCodeLine Line
  | PPSourceLineDirective PPDirective

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

data PPDefine
  -- #define varname String
  = PPDefineConst String String
  -- #define varname (arglist) body
  | PPDefineMacro [String] Line

data PPUndef
  -- #undef varname
  = PPUndef String
  -- #undef something else
  | PPUndefMacro Line

data PPInclude
  -- #include <something.h>
  = PPIncludeLibrary String
  -- #include "something.h"
  | PPIncludeInternal String
  -- #include [something else]
  | PPIncludeMacro Line

data PPIf
  -- #if Line [PPSourceLine] (Maybe PPElif) #endif
  = PPIf Line [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #ifdef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse) #endif
  | PPIfdef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)
  -- #ifndef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse) #endif
  | PPIfndef String [PPSourceLine] (Maybe PPElif) (Maybe PPElse)

data PPElif
  -- #elif Line [PPSourceLine] (Maybe PPElif)
  = PPElif Line [PPSourceLine] (Maybe PPElif)

data PPElse
  -- #else [PPSourceLine]
  = PPElse [PPSourceLine]

data PPLine
  -- #line linenum
  = PPLine Int
  -- #line linenum filename
  | PPLineFileName Int String
  -- #line anything else
  | PPLineMacro Line

data PPError
  -- #error error message
  = PPError Error
  -- #error line to be transformed
  | PPErrorMacro Line

data Macro =
  Macro
    { macroConstants :: M.Map String String
    , macroFunctions :: M.Map String (Int, Line)
    }

-------------------
-- PRE-TRANSFORM --
-------------------

-- replace trigraph sequences in source string
trigraph :: String -> String
trigraph ('?':'?':'=':s) = '#':s
trigraph ('?':'?':'/':s) = '\\':s
trigraph ('?':'?':'\'':s) = '^':s
trigraph ('?':'?':'(':s) = '[':s
trigraph ('?':'?':')':s) = ']':s
trigraph ('?':'?':'!':s) = '|':s
trigraph ('?':'?':'<':s) = '{':s
trigraph ('?':'?':'>':s) = '}':s
trigraph ('?':'?':'-':s) = '~':s
trigraph s = s

-- concat lines ending with backslash
lineSplice :: String -> String
lineSplice = concat . lineSplice' . lines
  where
    lineSplice' [] = []
    lineSplice' [s] = [s]
    lineSplice' (a:b:rest) =
      if last a == '\\'
        then lineSplice' $ (init a ++ b):rest
        else lineSplice' $ a:(lineSplice' $ b:rest)

readLines :: String -> Either Error [Line]
readLines sourceCode = do
  let sourceLines = lines $ lineSplice $ trigraph sourceCode
  traverse
    (\(lineNum, line) -> scanCLine (lineNum, 1) line)
    (zip [1..] sourceLines)

-- replace comments with single spaces
removeComments :: Line -> Line
removeComments [] = []
removeComments ((ScanItem { scanLoc = c, scanItem = LComment }):lineTail) =
  (ScanItem { scanLoc = c
            , scanItem = LWhiteSpace
            , scanStr = " " }):(removeComments lineTail)
removeComments (item:lineTail) = item:(removeComments lineTail)

-----------
-- UTILS --
-----------

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
  , LPPConcat
  , LPPEmpty
  ]

filterWS :: Line -> Line
filterWS = filter ((/= LWhiteSpace) . scanItem)

dropWS :: Line -> Line
dropWS = dropWhile ((/= LWhiteSpace) . scanItem)

toString :: Line -> String
toString = concat . map scanStr

readMacro :: Coordinates -> Line -> Either Error ([String], Line)
readMacro c line =
  let noWS = dropWS line
   in case noWS of
     ((ScanItem { scanItem = LLabel varName }):lineTail) ->
       case dropWS lineTail of
         ((ScanItem { scanItem = LComma }):lineTail') -> do
           (varNames, lineTail'') <- readMacro c lineTail'
           return (varName:varNames, lineTail'')
         ((ScanItem { scanItem = LParenthesisClose }):lineTail') ->
           return ([varName], lineTail')
         _ -> Left $ PreProcessError c "Error parsing a macro definition"
     ((ScanItem { scanItem = LParenthesisClose }):lineTail) ->
       return ([], lineTail)
     _ -> Left $ PreProcessError c "Error parsing a macro definition"

-------------
-- PARSERS --
-------------

nonEmptyParser :: PPParser a -> PPParser a
nonEmptyParser p = PPParser $ \input ->
  if input == []
    then Left $ PreProcessError (1, 1) "Unexpected EOF"
    else runPPParser p input

translationUnitParser :: PPParser PPTranslationUnit
translationUnitParser = PPParser $ \input ->
  if input == []
    then return ([], PPTranslationUnit [])
    else do
      (input', line) <- runPPParser sourceLineParser input
      (input'', PPTranslationUnit rest) <- runPPParser translationUnitParser input'
      return (input'', PPTranslationUnit $ line:rest)

isPPDirectiveLine :: Line -> Bool
isPPDirectiveLine line =
  case dropWS line of
    [] -> False
    l -> (scanItem $ head l) `elem` ppLexemes

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
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPDefine
        then
          case dropWS $ tail noWS of
            [(ScanItem { scanItem = LStringLiteral varName })] ->
              return (rest, PPDefineConst varName "")
            ((ScanItem { scanItem = LStringLiteral varName }):lineTail) ->
              case dropWS lineTail of
                [] -> return (rest, PPDefineConst varName "")
                ((ScanItem { scanItem = LParenthesisOpen }):listTail) -> do
                  (varNames, body) <- readMacro (scanLoc $ head line) listTail
                  return (rest, PPDefineMacro varNames body)
                l -> return (rest, PPDefineConst varName $ toString l)
            _ ->
              Left $
                PreProcessError
                  (scanLoc $ head line)
                  "Error parsing a define preprocess directive"
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

undefParser :: PPParser PPUndef
undefParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPUndef
        then return (rest, PPUndefMacro $ tail noWS)
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

includeParser :: PPParser PPInclude
includeParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPInclude
        then return (rest, PPIncludeMacro $ tail noWS)
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

ifParser :: PPParser PPIf
ifParser = ififParser <|> ifdefParser <|> ifndefParser

ififParser :: PPParser PPIf
ififParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPIf
        then
          case filterWS $ tail noWS of
            ((ScanItem { scanItem = LLabel "defined" }):lineTail) ->
              runPPParser
                ifdefParser $
                ((ScanItem { scanItem = LPPIfdef
                           , scanLoc = (scanLoc $ head line)
                           , scanStr = "" }
                 ):lineTail
                ):rest
            ((ScanItem { scanItem = LNot }):(ScanItem { scanItem = LLabel "defined" }):lineTail) ->
              runPPParser
                ifndefParser $
                ((ScanItem { scanItem = LPPIfndef
                           , scanLoc = (scanLoc $ head line)
                           , scanStr = "" }
                 ):lineTail
                ):rest
            lineTail -> do
              (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
              return (input', PPIf lineTail body ppelif ppelse)
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

ifdefParser :: PPParser PPIf
ifdefParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPIfdef
        then
          case filterWS $ tail noWS of
            [(ScanItem { scanItem = (LLabel varName) })] -> do
              (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
              return (input', PPIfndef varName body ppelif ppelse)
            _ ->
              Left $
                PreProcessError
                (scanLoc $ head line)
                "ifdef name not specified"
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

readIfBody :: PPParser ([PPSourceLine], (Maybe PPElif), (Maybe PPElse))
readIfBody = nonEmptyParser $ PPParser $ \input@(line:rest) ->
  case scanItem $ head $ dropWS line of
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
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPElif
        then do
          (input', lns) <- runPPParser readElifBody rest
          if (scanItem $ head $ dropWS $ head input') == LPPElif
            then do
              (input'', ppelif) <- runPPParser readElif input'
              return (input'', PPElif (tail noWS) lns $ Just ppelif)
            else
              return (input', PPElif (tail noWS) lns Nothing)
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

readElifBody :: PPParser [PPSourceLine]
readElifBody = nonEmptyParser $ PPParser $ \input@(line:_) ->
  case scanItem $ head $ dropWS line of
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
  case scanItem $ head $ dropWS line of
    LPPEndif ->
      return (rest, PPElse [])
    _ -> do
      (input', line') <- runPPParser sourceLineParser input
      (input'', PPElse lns') <- runPPParser readElse input'
      return (input'', (PPElse $ line':lns'))

ifndefParser :: PPParser PPIf
ifndefParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  let noWS = dropWS line
   in if (scanItem $ head noWS) == LPPIfndef
        then
          case filterWS $ tail noWS of
            [(ScanItem { scanItem = (LLabel varName) })] -> do
              (input', (body, ppelif, ppelse)) <- runPPParser readIfBody rest
              return (input', PPIfndef varName body ppelif ppelse)
            _ ->
              Left $
                PreProcessError
                (scanLoc $ head line)
                "ifndef name not specified"
        else
          Left $
            PreProcessError
              (scanLoc $ head line)
              "Error trying to parse a preprocess directive"

lineParser :: PPParser PPLine
lineParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  let noWS = filterWS line
   in if (scanItem $ head noWS) == LPPLine
        then
          case tail $ map scanItem noWS of
            ((LIntLiteral x):(LStringLiteral fName):lineTail) ->
              if lineTail == []
                then return (rest, PPLineFileName x fName)
                else return (rest, PPLineMacro $ tail line)
            ((LIntLiteral x):lineTail) ->
              if lineTail == []
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
  if (scanItem $ head $ dropWS line) == LPPPragma
    then return (rest, PPErrorMacro $ tail $ dropWS line)
    else Left $
           PreProcessError
             (scanLoc $ head line)
             "Error trying to parse a preprocess directive"

pragmaParser :: PPParser ()
pragmaParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if (scanItem $ head $ filterWS line) == LPPPragma
    then return (rest, ())
    else Left $
           PreProcessError
             (scanLoc $ head line)
             "Error trying to parse a preprocess directive"

emptyParser :: PPParser ()
emptyParser = nonEmptyParser $ PPParser $ \(line:rest) ->
  if (scanItem $ head $ filterWS line) == LPPEmpty
    then return (rest, ())
    else Left $
           PreProcessError
             (scanLoc $ head line)
             "Error trying to parse a preprocess directive"

