module PreProcess.PPTransform where

{- Transform the preprocessor parse tree into C source code. -}

import qualified Data.Map as M
import System.Directory (doesFileExist)
import Data.List (intercalate)

import PreProcess.PPParser
  ( PPTranslationUnit(..)
  , PPSourceLine(..)
  , PPDirective(..)
  , PPDefine(..)
  , PPUndef(..)
  , PPInclude(..)
  , PPIf(..)
  , PPElif(..)
  , PPElse(..)
  , PPLine(..)
  , PPError(..)
  , translationUnitParser
  , PPParser(..)
  )
import PreProcess.PreTransform (preTransform)
import PreProcess.Macro (MacroDict, macroExpand, Macro(..))

import Lexeme (CLexeme(..))
import Scanner (ScanItem(..), Line)
import Utils (Error(..), errorLoc, errorMsg)
import ParseItem (ParseItem(..))
import Parser (Parser(..), cConstantExpressionP)
import IR (evaluateConstantExpression)

-- Context for the line being preprocessed
data Context = Context { fileName :: String
                       , lineNum :: Int
                       , macroSymbols :: MacroDict
                       }
                       deriving (Show, Eq)

type PreProcessResult = IO (Either Error (Context, [Line]))

type PPTransform a = Context -> a -> PreProcessResult

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
    macroExpand (scanLoc $ head line) (macroSymbols ctx) toTransform
  (notParsed, constExpr) <-
    runParser cConstantExpressionP (toParse ++ [endMarker])
  if notParsed == [endMarker]
    then do
      result <- evaluateConstantExpression (parseItem constExpr)
      return (result /= 0)
    else
      Left . PreProcessError (scanLoc $ head notParsed) $
        "unable to parse an if expression"

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
  case macroExpand (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) (setLineContext line ctx) of
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
  case macroExpand (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
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
  case macroExpand (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
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
  case macroExpand (fileName ctx, (lineNum ctx, 1)) (macroSymbols ctx) line of
    Right line' ->
      return .  Left . PreProcessError (fileName ctx, (lineNum ctx, 1)) . concatMap scanStr $ line'
    Left e -> return . Left $ e

