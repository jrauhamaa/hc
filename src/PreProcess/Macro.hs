module PreProcess.Macro where

{- macro-expand preprocessor code -}

import qualified Data.Map as M
import Data.Char (isSpace)

import PreProcess.PreTransform (removeWhitespace)
import Scanner ( ScanItem(..)
               , Line
               , scanCLine
               , CLexeme(..)
               )
import Utils ( Error(..)
             , Location
             )

data Macro
  = MacroConstant String
  | MacroFunction [String] Line
  deriving (Eq, Show)

type MacroDict = M.Map String Macro

scanCLineNoWS :: Location -> String -> Either Error [ScanItem CLexeme]
scanCLineNoWS c input = removeWhitespace <$> scanCLine c input

{- Read arguments of macro function call.
   Takes as an argument the part of the line starting with
     the open parenthesis that begins the argument list
   Returns a tuple of a list of argument values and the part of
     the line after the arglist -}
readMacroArgs :: Location -> Line -> Either Error ([Line], Line)
readMacroArgs l (ScanItem {scanItem = LParenthesisOpen}:line) =
  case line of
    (ScanItem {scanItem = LParenthesisClose}:rest) ->
      return ([], rest)
    [] ->
      Left . PreProcessError l $ "a macro function called without arguments"
    _ -> do
      (arg, rest) <- readSingleMacroArg l line
      case rest of
        (ScanItem {scanItem = LComma}:rest') -> do
          (argsTail, lineTail) <- readMacroArgs l rest'
          return (arg:argsTail, lineTail)
        (ScanItem {scanItem = LParenthesisClose}:lineTail) ->
          return ([arg], lineTail)
        _ ->
          Left . PreProcessError l $
            "a macro function called without arguments"
readMacroArgs l _ =
  Left . PreProcessError l $ "a macro function called without arguments"

{- Read a single argument in a macro argument list
   Takes as an argument the part of the line starting with
     the macro argument.
   Returns a tuple of the tokens making up the argument and
     the rest of the line -}
readSingleMacroArg :: Location -> Line -> Either Error (Line, Line)
readSingleMacroArg l [] =
  Left . PreProcessError l $ "error parsing a macro argument"
readSingleMacroArg _ line@(ScanItem {scanItem = LParenthesisClose}:_) =
  return ([], line)
readSingleMacroArg _ line@(ScanItem {scanItem = LComma}:_) = return ([], line)
readSingleMacroArg l line@(ScanItem {scanItem = LParenthesisOpen}:_) = do
  (argHead, line') <- readMacroArgParens l line
  (argTail, lineTail) <- readSingleMacroArg l line'
  return (argHead ++ argTail, lineTail)
readSingleMacroArg l (item:rest) = do
  (argTail, lineTail) <- readSingleMacroArg l rest
  return (item:argTail, lineTail)

{- Read from open parenthesis into matching closing parenthesis.
   Returns a tuple of the tokens making up the parenthesized part and
     the rest of the line -}
readMacroArgParens :: Location -> Line -> Either Error (Line, Line)
readMacroArgParens l (item@ScanItem {scanItem = LParenthesisOpen}:rest) = do
  (argTail, rest') <- readMacroArgParens' l 1 rest
  return (item:argTail, rest')
readMacroArgParens l _ =
  Left . PreProcessError l $ "error parsing a macro argument"

readMacroArgParens' :: Location -> Int -> Line -> Either Error (Line, Line)
readMacroArgParens' _ 0 line = return ([], line)
readMacroArgParens' l _ [] =
  Left . PreProcessError l $ "error parsing a macro argument"
readMacroArgParens' l n (item@ScanItem{scanItem = LParenthesisOpen}:rest) = do
  (argTail, rest') <- readMacroArgParens' l (n + 1) rest
  return (item:argTail, rest')
readMacroArgParens' l n (item@ScanItem{scanItem = LParenthesisClose}:rest) = do
  (argTail, rest') <- readMacroArgParens' l (n - 1) rest
  return (item:argTail, rest')
readMacroArgParens' l n (item:rest) = do
  (argTail, rest') <- readMacroArgParens' l n rest
  return (item:argTail, rest')

-- evaluate ## (concat) directives
concatTokens :: Line -> Either Error Line
concatTokens [] = return []
concatTokens line
  | LPPConcat `notElem` map scanItem line = return line
  | null beforeConcat || null afterConcat
      = Left . PreProcessError (scanLoc $ head line) $
          "invalid use of concat directive"
  | otherwise
      = scanCLineNoWS (scanLoc $ head line)
                      (rtrim (concatMap scanStr beforeConcat)
                       ++ ltrim (concatMap scanStr afterConcat))
  where
    beforeConcat = takeWhile ((/= LPPConcat) . scanItem) line
    afterConcat = tail $ dropWhile ((/= LPPConcat) . scanItem) line
    ltrim = dropWhile isSpace
    rtrim = reverse . dropWhile isSpace . reverse

-- expand the macro symbols contained in the line
macroExpand :: Location -> MacroDict -> Line -> Either Error Line
macroExpand _ _ [] = return []
macroExpand l m line = do
  macroTransformed <- macroExpand' m line
  newLine <- scanCLineNoWS l macroTransformed
  -- the expanded line might have to be expanded again
  if newLine == line
    then do
      -- only expand concat directives after all macros have been expanded
      concatenated <- concatTokens newLine
      if concatenated == line
        then return concatenated
        else macroExpand l m concatenated
    else do
      macroExpand l m newLine

macroExpand' :: MacroDict -> Line -> Either Error String
macroExpand' _ [] = return []
macroExpand' m (lItem@ScanItem { scanStr = s, scanItem = LLabel label }:rest) =
  case M.lookup label m of
    Nothing -> do
      lineTail <- macroExpand' m rest
      return $ s ++ lineTail
    Just (MacroConstant replaceStr) -> do
      lineTail <- macroExpand' m rest
      -- preserve whitespace on both sides of the macroconstant
      return $ takeWhile isSpace s
               ++ replaceStr
               ++ (reverse . takeWhile isSpace . reverse $ s)
               ++ lineTail
    Just func@(MacroFunction args _) ->
      case map scanItem rest of
        (LParenthesisOpen:_) -> do
          (args', afterMacro) <- readMacroArgs (scanLoc lItem) rest
          if length args == length args'
            then do
              lineTail <- macroExpand' m afterMacro
              return $ evaluateMacroFunction func args' ++ lineTail
            else
              Left $ PreProcessError
                       (scanLoc lItem)
                       "wrong number of args passed to a macro function"
        _ ->
          Left $ PreProcessError
                   (scanLoc lItem)
                   "no arguments given to a macro function"
macroExpand' m (item:rest) = do
  lineTail <- macroExpand' m rest
  return $ scanStr item ++ lineTail

-- Expand the macro function into a string
evaluateMacroFunction :: Macro -> [Line] -> String
evaluateMacroFunction (MacroConstant s) _ = s -- this shouldn't happen
evaluateMacroFunction (MacroFunction argNames body) args =
  concatMap scanStr replaced
  where
    -- replace single argument
    replaceArg line (name, replaceLine) =
      concatMap
        (\item ->
           case scanItem item of
             LLabel label ->
               if label == name
                 then let precedingWS = takeWhile isSpace (scanStr item)
                          tailingWS =
                            reverse . takeWhile isSpace . reverse . scanStr $
                              item
                       in [ScanItem { scanLoc = ("", (0, 0))
                                    , scanStr = precedingWS
                                    , scanItem = LWhiteSpace }]
                          ++ replaceLine
                          ++ [ScanItem { scanLoc = ("", (0, 0))
                                       , scanStr = tailingWS
                                       , scanItem = LWhiteSpace }]
                 else [item]
             _ -> [item])
        line
    replaced = foldl replaceArg body (zip argNames args)

