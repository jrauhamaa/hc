{-# LANGUAGE TupleSections #-}

module Symbols.TypeReader where

{- This module contains functions for parsing type definitions from
   the parse tree -}

-- TODO: fix error locations

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust)

import Parser.ParseItem
import IR.ConstantExpression (evaluateConstantExpression, doubleToInteger)
import Utils ( Location
             , Error(..)
             , DataType(..)
             , StorageClass(..)
             , TypeQualifier(..)
             , CType(..)
             , SymbolTable(..)
             )

{- Find value associated with label if contained in the symbol table
   or one of its parents or return Nothing if no symbol found -}
lookupSymbols ::
     (SymbolTable
  -> M.Map String CType)
  -> String
  -> SymbolTable
  -> Maybe CType
lookupSymbols extract s sym =
  case parent sym of
    Just sym' -> v <|> lookupSymbols extract s sym'
    Nothing -> v
  where
    v = M.lookup s (extract sym)

lookupStruct :: String -> SymbolTable -> Maybe CType
lookupStruct = lookupSymbols structs

lookupUnion :: String -> SymbolTable -> Maybe CType
lookupUnion = lookupSymbols unions

lookupEnum :: String -> SymbolTable -> Maybe CType
lookupEnum = lookupSymbols enums

lookupTypedef :: String -> SymbolTable -> Maybe CType
lookupTypedef = lookupSymbols typedef

toQualifier :: CTypeQualifier -> TypeQualifier
toQualifier CTypeQualifierConst = TQConst
toQualifier CTypeQualifierVolatile = TQVolatile

toStorageClass :: CStorageClassSpecifier -> StorageClass
toStorageClass CStorageClassSpecifierAuto = SCAuto
toStorageClass CStorageClassSpecifierRegister = SCRegister
toStorageClass CStorageClassSpecifierStatic = SCStatic
toStorageClass CStorageClassSpecifierExtern = SCExtern
toStorageClass CStorageClassSpecifierTypedef = SCTypedef

---------------------
-- SPECIFIER LISTS --
---------------------

-- check if storage classes conform to syntax
validateStorageClasses :: Location -> [StorageClass] -> Either Error ()
validateStorageClasses l classes
  | length classes > 1 =
      Left . SyntaxError l $ "Multiple storage classes"
  | otherwise = return ()

{- Check if type specifiers conform to syntax and construct type
   from the specifier list -}
readDatatype ::
     Location -> SymbolTable -> [CTypeSpecifier] -> Either Error DataType
readDatatype l sym spec
  | null spec = Left . SyntaxError l $ "Type unspecified"
  | lengthSpec `notElem` [ [CTypeSpecifierLong, CTypeSpecifierLong]
                         , [CTypeSpecifierLong]
                         , [CTypeSpecifierShort]
                         , []] =
      Left . SyntaxError l $ "Illegal type length specification"
  | length dataTypes > 1 =
      Left . SyntaxError l $ "Too many datatype specifiers"
  | length signedSpec > 1 =
      Left . SyntaxError l $ "Too many datatype sign specifiers"
  -- long long int
  | lengthSpec == [CTypeSpecifierLong, CTypeSpecifierLong] =
      if null dataTypes || dataTypes == [CTypeSpecifierInt]
        then if signedSpec == [CTypeSpecifierUnsigned]
               then Right TULongLong
               else Right TLongLong
        else Left . SyntaxError l $ "Long long can only be used with integers"
  -- short int
  | lengthSpec == [CTypeSpecifierShort] =
      if null dataTypes || head dataTypes == CTypeSpecifierInt
        then if signedSpec == [CTypeSpecifierUnsigned]
               then Right TUShort
               else Right TShort
        else Left . SyntaxError l $ "Short can only be used with integers"
  -- long int or long double
  | lengthSpec == [CTypeSpecifierLong] =
      case dataTypes of
        [] ->
          if signedSpec == [CTypeSpecifierUnsigned]
            then Right TULong
            else Right TLong
        [CTypeSpecifierInt] ->
          if signedSpec == [CTypeSpecifierUnsigned]
            then Right TULong
            else Right TLong
        [CTypeSpecifierDouble] ->
          if null signedSpec
            then Right TLongDouble
            else Left . SyntaxError l $ "Can't specify sign of double"
        _ -> Left . SyntaxError l $
               "Long can only be used with integers and doubles"
  -- (un)signed int or char
  | not (null signedSpec) =
      case dataTypes of
        [CTypeSpecifierInt] ->
          return $ if signedSpec == [CTypeSpecifierUnsigned]
                     then TUShort
                     else TShort
        [] ->
          return $ if signedSpec == [CTypeSpecifierUnsigned]
                     then TUShort
                     else TShort
        [CTypeSpecifierChar] ->
          return $ if signedSpec == [CTypeSpecifierUnsigned]
                     then TUChar
                     else TChar
        _ ->
          Left . SyntaxError l $
            "Bad combination of datatype sign specifier & type"
  | otherwise =
      case head dataTypes of
        CTypeSpecifierChar -> Right TChar
        CTypeSpecifierInt -> Right TShort
        CTypeSpecifierFloat -> Right TFloat
        CTypeSpecifierDouble -> Right TDouble
        CTypeSpecifierEnum enumSpec ->
          readEnumSpecifier (parseLoc enumSpec) sym (parseItem enumSpec)
        CTypeSpecifierStructOrUnion structOrUnionSpec ->
          readStructOrUnionSpecifier sym (parseItem structOrUnionSpec)
        CTypeSpecifierTypedef tdName ->
          readTypedefName sym (parseItem tdName)
        _ -> Left . InternalError l $
               "Internal error reading type"  -- this shoudn't happen
  where
    lengthSpec = filter (`elem` [CTypeSpecifierLong, CTypeSpecifierShort]) spec
    dataTypes =
      filter
        (`notElem` [ CTypeSpecifierLong
                   , CTypeSpecifierShort
                   , CTypeSpecifierSigned
                   , CTypeSpecifierUnsigned
                   ])
        spec
    signedSpec =
      filter (`elem` [CTypeSpecifierSigned, CTypeSpecifierUnsigned]) spec

-- check if type qualifiers conform to syntax
validateTypeQualifiers :: Location -> [TypeQualifier] -> Either Error ()
validateTypeQualifiers l spec
  | length spec > 2 = Left . SyntaxError l $ "Repeated type qualifiers"
  | spec == [TQConst, TQConst] =
      Left . SyntaxError l $ "Repeated type qualifiers"
  | spec == [TQVolatile, TQVolatile] =
      Left . SyntaxError l $ "Repeated type qualifiers"
  | otherwise = return ()

-- check if storage classes of a function conform to syntax
validateFunctionStorageClasses :: Location -> [StorageClass] -> Either Error ()
validateFunctionStorageClasses l spec =
  if spec `elem` [[], [SCExtern], [SCStatic]]
    then return ()
    else
      Left .  SyntaxError l $
        "Illegal storage class specifier in function definition"

{- Transform TypeQualifierList node in parse tree to an actual list
   of type qualifiers -}
readTypeQualifiers ::
     Location -> CTypeQualifierList -> Either Error [TypeQualifier]
readTypeQualifiers l qualifierList = do
  let qualifiers = readTypeQualifiers' qualifierList
  validateTypeQualifiers l qualifiers
  return qualifiers

readTypeQualifiers' :: CTypeQualifierList -> [TypeQualifier]
readTypeQualifiers' (CTypeQualifierList qualifier listOpt) =
  case listOpt of
    Nothing -> [toQualifier (parseItem qualifier)]
    Just list ->
      toQualifier (parseItem qualifier) : readTypeQualifiers' (parseItem list)

{- Transform SpecifierQualifierList node in parse tree to actual lists
   of type specifiers & qualifiers -}
readSpecifierQualifierList ::
     Location
  -> CSpecifierQualifierList
  -> Either Error ([CTypeSpecifier], [TypeQualifier])
readSpecifierQualifierList l list = do
  let (specifiers, qualifiers) = readSpecifierQualifierList' list
  validateTypeQualifiers l qualifiers
  return (specifiers, qualifiers)

readSpecifierQualifierList' ::
     CSpecifierQualifierList -> ([CTypeSpecifier], [TypeQualifier])
readSpecifierQualifierList' (CSpecifierQualifierListSpecifier spec Nothing) =
  ([parseItem spec], [])
readSpecifierQualifierList'
     (CSpecifierQualifierListSpecifier spec (Just list)) =
  (parseItem spec : specifiers, qualifiers)
  where (specifiers, qualifiers) = readSpecifierQualifierList' (parseItem list)
readSpecifierQualifierList'
    (CSpecifierQualifierListQualifier qualifier Nothing) =
  ([], [toQualifier (parseItem qualifier)])
readSpecifierQualifierList'
     (CSpecifierQualifierListQualifier qualifier (Just list)) =
  (specifiers, toQualifier (parseItem qualifier) : qualifiers)
  where (specifiers, qualifiers) = readSpecifierQualifierList' (parseItem list)

readDeclarationSpecifierList ::
     Location
  -> CDeclarationSpecifiers
  -> Either Error ([StorageClass], [TypeQualifier], [CTypeSpecifier])
readDeclarationSpecifierList l spec = do
  let (storageClasses, qualifiers, specifiers) =
        readDeclarationSpecifierList' spec
  validateStorageClasses l storageClasses
  validateTypeQualifiers l qualifiers
  return (storageClasses, qualifiers, specifiers)


{- Transform SpecifierQualifierList node in parse tree to actual lists
   of storage classes, type qualifiers & specifiers -}
readDeclarationSpecifierList' ::
     CDeclarationSpecifiers
  -> ([StorageClass], [TypeQualifier], [CTypeSpecifier])
readDeclarationSpecifierList'
     (CDeclarationSpecifiersStorageClass spec Nothing) =
  ([toStorageClass (parseItem spec)], [], [])
readDeclarationSpecifierList'
     (CDeclarationSpecifiersStorageClass spec (Just nextSpec)) =
  (toStorageClass (parseItem spec) : storageCls, qualifiers, specifiers)
  where
    (storageCls, qualifiers, specifiers) =
      readDeclarationSpecifierList' (parseItem nextSpec)
readDeclarationSpecifierList'
     (CDeclarationSpecifiersTypeQualifier spec Nothing) =
  ([], [toQualifier (parseItem spec)], [])
readDeclarationSpecifierList'
     (CDeclarationSpecifiersTypeQualifier spec (Just nextSpec)) =
  (storageCls, toQualifier (parseItem spec) : qualifiers, specifiers)
  where
    (storageCls, qualifiers, specifiers) =
      readDeclarationSpecifierList' (parseItem nextSpec)
readDeclarationSpecifierList'
     (CDeclarationSpecifiersTypeSpecifier spec Nothing) =
  ([], [], [parseItem spec])
readDeclarationSpecifierList'
     (CDeclarationSpecifiersTypeSpecifier spec (Just nextSpec)) =
  (storageCls, qualifiers, parseItem spec : specifiers)
  where
    (storageCls, qualifiers, specifiers) =
      readDeclarationSpecifierList' (parseItem nextSpec)

-- transform CDeclarationSpecifiers into CType
readCType ::
     Location -> SymbolTable -> CDeclarationSpecifiers -> Either Error CType
readCType l sym spec = do
  (storageClasses, qualifiers, specifiers) <-
    readDeclarationSpecifierList l spec
  dType <- readDatatype l sym specifiers
  return
    CType
      { storageClass = storageClasses
      , typeQualifier = qualifiers
      , dataType = dType
      }

----------------------
-- STRUCTURED TYPES --
----------------------

{- Read a single value from an enumerator declaration together with
   its associated integer value if defined -}
readEnumerator :: CEnumerator -> Either Error (String, Maybe Int)
readEnumerator (CEnumerator identifier) = return (i, Nothing)
  where (CIdentifier i) = parseItem identifier
readEnumerator (CEnumeratorAssign identifier constExpr) = do
  arrayLength <-
    evaluateConstantExpression (parseItem constExpr)
      >>= doubleToInteger (parseLoc constExpr)
  return (i, Just arrayLength)
  where (CIdentifier i) = parseItem identifier

{- Read a list of enumerator values. Return a map of labels and their
   associated integer values -}
readEnumeratorList :: CEnumeratorList -> Either Error (M.Map String Int)
readEnumeratorList (CEnumeratorList enum list) = do
  listHead <- readEnumerator (parseItem enum)
  listTail <- readEnumeratorList' (parseItem list)
  let enumList = listHead : listTail
      maxValue =
        fmap (foldl max 0)
        . sequenceA
        . filter isJust
        . map snd
        $ listHead : listTail
      definedValues = case maxValue of
                     Nothing -> [1..]
                     Just x -> [x+1..]
      enumValues = zipWith extractMaybe definedValues . map snd $ enumList
      fieldNames = map fst enumList
  return . M.fromList . zip fieldNames $ enumValues
  where
    extractMaybe x Nothing = x
    extractMaybe _ (Just x) = x

readEnumeratorList' :: CEnumeratorList' -> Either Error [(String, Maybe Int)]
readEnumeratorList' CEnumeratorList'Empty = return []
readEnumeratorList' (CEnumeratorList' enum list) = do
  listHead <- readEnumerator (parseItem enum)
  listTail <- readEnumeratorList' (parseItem list)
  return (listHead : listTail)

-- Read an enum specification.
-- TODO: handle enum specifiers without definition properly.
readEnumSpecifier ::
     Location -> SymbolTable -> CEnumSpecifier -> Either Error DataType
readEnumSpecifier l sym (CEnumSpecifier cId) =
  case lookupEnum identifier sym of
    Just t -> Right $ dataType t
    Nothing -> Left . TypeError l $ "Undefined enum '" ++ identifier ++ "'."
  where CIdentifier identifier = parseItem cId
readEnumSpecifier _ _ (CEnumSpecifierList cId enumList) = do
  enumValues <- readEnumeratorList (parseItem enumList)
  let identifier = case cId of
                     Nothing -> Nothing
                     Just ParseItem { parseItem = CIdentifier i } -> Just i
  return . TEnum identifier $ enumValues

-- Read a struct or union specification.
-- TODO: handle specifications without definition properly.
readStructOrUnionSpecifier ::
     SymbolTable
  -> CStructOrUnionSpecifier
  -> Either Error DataType
readStructOrUnionSpecifier sym (CStructOrUnionSpecifier structOrUnion cId) =
  case symbolLookup of
    Nothing ->
      Left . TypeError (parseLoc structOrUnion) $
        "Undefined struct or union '" ++ identifier ++ "'."
    Just t -> return (dataType t)
  where CIdentifier identifier = parseItem cId
        symbolLookup = case parseItem structOrUnion of
                         CStructOrUnionStruct -> lookupStruct identifier sym
                         CStructOrUnionUnion -> lookupUnion identifier sym
readStructOrUnionSpecifier
     sym (CStructOrUnionSpecifierList structOrUnion identifierOpt decl) = do
  declarations <- readStructDeclarationList sym (parseItem decl)
  let identifiers = filter (/= Nothing) . map (\(_, i, _) -> i) $ declarations
      name = case identifierOpt of
               Nothing -> Nothing
               Just ParseItem { parseItem = CIdentifier i } -> Just i
  if length (S.fromList identifiers) /= length identifiers
    then Left . TypeError (parseLoc structOrUnion) $ "Conflicting identifiers"
    else case parseItem structOrUnion of
           CStructOrUnionStruct -> return (TStruct name declarations)
           CStructOrUnionUnion ->
             case traverse (\(_, identifier, _) -> identifier) declarations of
               Nothing ->
                 Left . TypeError (parseLoc structOrUnion) $
                   "Union identifier not specified"
               Just identifiers' ->
                 let unionTypes =
                       M.fromList
                       . zip identifiers'
                       . map (\(t, _, _) -> t)
                       $ declarations
                 in return (TUnion name unionTypes)

{- Read declaration of a single field inside a struct definition.
   Return a tuple of field type, field name (if defined) &
   field width (if defined). -}
readStructDeclarator ::
     SymbolTable
  -> CType
  -> CStructDeclarator
  -> Either Error (CType, Maybe String, Maybe Int)
readStructDeclarator sym partialType (CStructDeclarator decl) = do
  (identifier, t) <- readDeclarator sym partialType (parseItem decl)
  return (t, Just identifier, Nothing)
readStructDeclarator
     sym partialType (CStructDeclaratorField declOpt constExpr) =
  case declOpt of
    Nothing -> do
      fieldWidth <-
        evaluateConstantExpression (parseItem constExpr)
          >>= doubleToInteger (parseLoc constExpr)
      return (partialType, Nothing, Just fieldWidth)
    (Just decl) -> do
      fieldWidth <-
        evaluateConstantExpression (parseItem constExpr)
          >>= doubleToInteger (parseLoc constExpr)
      (identifier, t') <- readDeclarator sym partialType (parseItem decl)
      return (t', Just identifier, Just fieldWidth)

{- Read a list of struct field labels. Return a list of tuples of
   field type, label (if defined) & field width (if defined). -}
readStructDeclaratorList ::
     SymbolTable
  -> CType
  -> CStructDeclaratorList
  -> Either Error [(CType, Maybe String, Maybe Int)]
readStructDeclaratorList sym partialType (CStructDeclaratorList decl list) = do
  d <- readStructDeclarator sym partialType (parseItem decl)
  listTail <- readStructDeclaratorList' sym partialType (parseItem list)
  return (d : listTail)

readStructDeclaratorList' ::
     SymbolTable
  -> CType
  -> CStructDeclaratorList'
  -> Either Error [(CType, Maybe String, Maybe Int)]
readStructDeclaratorList' _ _ CStructDeclaratorList'Empty = return []
readStructDeclaratorList'
     sym partialType (CStructDeclaratorList' decl list) = do
  d <- readStructDeclarator sym partialType (parseItem decl)
  listTail <- readStructDeclaratorList' sym partialType (parseItem list)
  return (d : listTail)

{- Read a struct field declaration of single type (possibly multiple labels).
   Return a list of tupes of field type, label (if defined) &
   field width (if defined) -}
readStructDeclaration ::
     SymbolTable
  -> CStructDeclaration
  -> Either Error [(CType, Maybe String, Maybe Int)]
readStructDeclaration sym (CStructDeclaration specList declList) = do
  (specifiers, qualifiers) <-
    readSpecifierQualifierList (parseLoc specList) (parseItem specList)
  dType <- readDatatype (parseLoc specList) sym specifiers
  let t = CType { storageClass = []
                , typeQualifier = qualifiers
                , dataType = dType
                }
  readStructDeclaratorList sym t (parseItem declList)

{- Read a list of struct field declarations.  Return a list of tupes of
   field type, label (if defined) & field width (if defined) -}
readStructDeclarationList ::
     SymbolTable
  -> CStructDeclarationList
  -> Either Error [(CType, Maybe String, Maybe Int)]
readStructDeclarationList sym (CStructDeclarationList decl Nothing) =
  readStructDeclaration sym (parseItem decl)
readStructDeclarationList sym (CStructDeclarationList decl (Just list')) = do
  decl' <- readStructDeclaration sym (parseItem decl)
  declarations <- readStructDeclarationList sym (parseItem list')
  return (decl' ++ declarations)

{- Return the datatype associated with the typedef name or error
   if type not defined -}
readTypedefName :: SymbolTable -> CTypedefName -> Either Error DataType
readTypedefName sym (CTypedefName cId) =
  case lookupTypedef identifier sym of
    Just t -> Right (dataType t)
    Nothing ->
      Left . TypeError (parseLoc cId) $
        "Unknown typedef name '" ++ identifier ++ "'."
  where CIdentifier identifier = parseItem cId

---------------
-- FUNCTIONS --
---------------

{- read a list of function parameter types. Return a tuple of a boolean value
   indicating if the function takes a variable number of arguments &
   list of argument types -}
readParamTypeList ::
     SymbolTable
  -> CParameterTypeList
  -> Either Error (Bool, [(Maybe String, CType)])
readParamTypeList sym (CParameterTypeList paramList varargs) = do
  params <- readParamList sym (parseItem paramList)
  return (parseItem varargs /= CVarArgsOptionalEmpty, params)

{- Read a list of parameter declarations. Return a list of tuples of
   parameter name (if defined) & parameter type -}
readParamList ::
     SymbolTable -> CParameterList -> Either Error [(Maybe String, CType)]
readParamList sym (CParameterList decl list) = do
  declaration <- readParamDeclaration sym (parseItem decl)
  listTail <- readParamList' sym (parseItem list)
  return $ declaration : listTail

readParamList' ::
     SymbolTable -> CParameterList' -> Either Error [(Maybe String, CType)]
readParamList' _ CParameterList'Empty = return []
readParamList' sym (CParameterList' decl list) = do
  declaration <- readParamDeclaration sym (parseItem decl)
  listTail <- readParamList' sym (parseItem list)
  return $ declaration : listTail

{- Read declaration of a single function parameter.
   Return a tuple of parameter name (if defined) & parameter type. -}
readParamDeclaration ::
     SymbolTable -> CParameterDeclaration -> Either Error (Maybe String, CType)
readParamDeclaration sym (CParameterDeclaration specifiers paramDecl) =
  case parseItem paramDecl of
    CParameterDeclaration' decl -> do
      partialType <- readCType (parseLoc specifiers) sym (parseItem specifiers)
      (identifier, paramType) <-
        readDeclarator sym partialType (parseItem decl)
      return (Just identifier, paramType)
    CParameterDeclaration'Abstract Nothing -> do
      paramType <- readCType (parseLoc specifiers) sym (parseItem specifiers)
      return (Nothing, paramType)
    CParameterDeclaration'Abstract (Just abstractDecl) -> do
      partialType <- readCType (parseLoc specifiers) sym (parseItem specifiers)
      paramType <- readAbstractDeclarator partialType (parseItem abstractDecl)
      return (Nothing, paramType)

-- Check if function return types conforms to specification
validateFunctionReturnType :: Location -> CType -> Either Error ()
validateFunctionReturnType l returnType =
  case dataType returnType of
    TFunction {} -> Left . SyntaxError l $ "invalid function return type"
    TStruct _ _ -> Left . SyntaxError l $ "invalid function return type"
    _ -> return ()

readFunctionDeclarationNewStyle ::
     Location
  -> SymbolTable
  -> CType
  -> String
  -> CDirectDeclarator'
  -> Either Error (String, CType, [String])
readFunctionDeclarationNewStyle
     l sym returnType fName (CDirectDeclarator'ParamTypeList typeList decl') =
  case parseItem decl' of
    CDirectDeclarator'Empty -> do
      validateFunctionReturnType l returnType
      (varargs, params) <- readParamTypeList sym (parseItem typeList)
      let functionType =
            CType
              { storageClass = []
              , typeQualifier = []
              , dataType =
                  TFunction
                    fName
                    returnType
                    (map snd params)
                    varargs
              }
      case traverse fst params of
        Nothing -> Left . SyntaxError l $ "Unnamed function parameter(s)"
        Just paramNames ->
          if length (S.fromList paramNames) == length paramNames
            then return (fName, functionType, paramNames)
            else Left . TypeError l $ "Conflicting parameter names"
    _ -> Left . SyntaxError l $ "Invalid function declaration"
readFunctionDeclarationNewStyle
     l
     _
     returnType
     fName
     (CDirectDeclarator'IdList
        Nothing
        ParseItem { parseItem = CDirectDeclarator'Empty }) = do
  validateFunctionReturnType l returnType
  let functionType =
        CType
          { storageClass = []
          , typeQualifier = []
          , dataType = TFunction fName returnType [] False
          }
  return (fName, functionType, [])
readFunctionDeclarationNewStyle l _ _ _ _ =
  Left . SyntaxError l $ "Invalid function declaration"

readFunctionDeclarationOldStyle ::
     Location
  -> SymbolTable
  -> CType
  -> String
  -> CDirectDeclarator'
  -> CDeclarationList
  -> Either Error (String, CType, [String])
readFunctionDeclarationOldStyle
     l
     sym
     returnType
     fName
     (CDirectDeclarator'IdList
       idList
       ParseItem { parseItem = CDirectDeclarator'Empty })
     declList = do
   params <- readDeclarationList sym declList
   validateFunctionReturnType l returnType
   let paramNames = case idList of
                      Nothing -> []
                      Just idList' -> readIdentifierList (parseItem idList')
   let paramTypeLookup =
         M.fromList $ map (\(_, k, v) -> (k, v)) params
       paramTypes = map (`M.lookup` paramTypeLookup) paramNames
   case sequenceA paramTypes of
     Nothing ->
       Left . InternalError l $ -- this shouldn't happen
         "Internal error parsing function parameter types"
     Just paramTypes' ->
       if (S.fromList paramNames == S.fromList (map (\(_, k, _) -> k) params))
          && (length (S.fromList paramNames) == length paramNames)
          && all (\(initialized, _, _) -> not initialized) params
         then return
                ( fName
                , CType
                    { storageClass = []
                    , typeQualifier = []
                    , dataType =
                        TFunction
                          fName
                          returnType
                          paramTypes'
                          False
                    }
                , paramNames
                )
         else Left . SyntaxError l $ "Invalid parameter declaration"
readFunctionDeclarationOldStyle l _ _ _ _ _ =
  Left . SyntaxError l $ "Invalid function declaration"

{- Read a function declaration. Return a tuple of funtion name,
   function type (return type & argument types) & list of argument names -}
readFunctionDeclaration ::
     Location
  -> SymbolTable
  -> CType
  -> CFunctionDefinition
  -> Either Error (String, CType, [String])
-- new style
readFunctionDeclaration
     l
     sym
     partialReturnType
     (CFunctionDefinition
       _
       ParseItem
         { parseItem =
             CDeclarator
               pointerOpt
               directDeclarator
         }
       Nothing
       _) =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left . SyntaxError l $ "Invalid function declaration"
    CDirectDeclaratorId cId decl' -> do
      returnType <- readPointerOptional partialReturnType pointerOpt
      let CIdentifier fName = parseItem cId
      readFunctionDeclarationNewStyle l sym returnType fName (parseItem decl')
-- old style
readFunctionDeclaration
     l
     sym
     partialReturnType
     (CFunctionDefinition
        _
        ParseItem
          { parseItem =
              CDeclarator
                pointerOpt
                directDeclarator
          }
        (Just declList)
        _) =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left . SyntaxError l $ "Invalid function declaration"
    CDirectDeclaratorId cId decl' -> do
      returnType <- readPointerOptional partialReturnType pointerOpt
      let CIdentifier fName = parseItem cId
      readFunctionDeclarationOldStyle
        l sym returnType fName (parseItem decl') (parseItem declList)

{- Read a function definition. Return a tuple of function name, function type
   (return type & argument types) & a list of argument names -}
readFunctionDefinition ::
     Location
  -> SymbolTable
  -> CFunctionDefinition
  -> Either Error (String, CType, [String])
readFunctionDefinition l sym item@(CFunctionDefinition Nothing _ _ _) =
  readFunctionDeclaration l sym returnType item
  where
    returnType =
      CType {storageClass = [], typeQualifier = [], dataType = TShort}
readFunctionDefinition
     l sym item@(CFunctionDefinition (Just specifiers) _ _ _) = do
  returnType <- readCType (parseLoc specifiers) sym (parseItem specifiers)
  readFunctionDeclaration l sym returnType item

------------------
-- DECLARATIONS --
------------------

{- Read init declarator. Return a tuple of a boolean value indicating if this
   code line initializes the variable, varlable name & variable type. -}
readInitDeclarator ::
     SymbolTable
  -> CType
  -> CInitDeclarator
  -> Either Error (Bool, String, CType)
readInitDeclarator sym partialType (CInitDeclarator decl initOpt) = do
  (identifier, variableType) <- readDeclarator sym partialType (parseItem decl)
  return ( parseItem initOpt /= CAssignInitializerOptionalEmpty
         , identifier
         , variableType
         )

{- Read a list of init declarators. Return a list of tuples of
   a boolean value indicating if this code line initializes the variable,
   variable name & variable type. -}
readInitDeclaratorList ::
     SymbolTable
  -> CType
  -> CInitDeclaratorList
  -> Either Error [(Bool, String, CType)]
readInitDeclaratorList sym partialType (CInitDeclaratorList decl list) = do
  d <- readInitDeclarator sym partialType (parseItem decl)
  listTail <- readInitDeclaratorList' sym partialType (parseItem list)
  return (d : listTail)

readInitDeclaratorList' ::
     SymbolTable
  -> CType
  -> CInitDeclaratorList'
  -> Either Error [(Bool, String, CType)]
readInitDeclaratorList' _ _ CInitDeclaratorList'Empty = return []
readInitDeclaratorList' sym partialType (CInitDeclaratorList' decl list) = do
  d <- readInitDeclarator sym partialType (parseItem decl)
  listTail <- readInitDeclaratorList' sym partialType (parseItem list)
  return (d : listTail)

-- Read typedef type & label from CInitDeclaratorList
readTypedefLabel ::
     CType -> CInitDeclaratorList -> Either Error (CType, String)
readTypedefLabel partialType (CInitDeclaratorList decl list) =
  if parseItem list == CInitDeclaratorList'Empty
    then readTypedefLabelInitDeclarator partialType (parseItem decl)
    else Left . SyntaxError (parseLoc list) $
           "Multiple typedef labels specified"


readTypedefLabelInitDeclarator ::
     CType -> CInitDeclarator -> Either Error (CType, String)
readTypedefLabelInitDeclarator partialType (CInitDeclarator decl initializer) =
  if parseItem initializer == CAssignInitializerOptionalEmpty
    then readTypedefLabelDeclarator partialType (parseItem decl)
    else Left . SyntaxError (parseLoc initializer) $
           "Typedef and assignment in same statement"

readTypedefLabelDeclarator ::
     CType -> CDeclarator -> Either Error (CType, String)
readTypedefLabelDeclarator
     partialType (CDeclarator pointerOpt directDecl) = do
  typedefType <- readPointerOptional partialType pointerOpt
  case parseItem directDecl of
    CDirectDeclaratorId
         cId ParseItem { parseItem = CDirectDeclarator'Empty } ->
      let CIdentifier identifier = parseItem cId
      in return (typedefType, identifier)
    _ -> Left . SyntaxError (parseLoc directDecl) $ "Invalid typedef statement"


{- Check if the declaration specifier list specifies a type
   associated with a typedef label -}
isTypeDef :: Location -> CDeclarationSpecifiers -> Either Error Bool
isTypeDef l spec = do
  (storageCls, _, _) <- readDeclarationSpecifierList l spec
  return (storageCls == [SCTypedef])

{- Read a declaration. Return a tuple of a boolean value indicating if the
   declaration declares a typedef label and a list of tuples of
   a boolean value indicating if this code line initializes the variable,
   variable name & variable type. -}
readDeclaration ::
     SymbolTable
  -> CDeclaration
  -> Either Error (Bool, [(Bool, String, CType)])
readDeclaration sym (CDeclaration spec Nothing) = do
  isTDef <- isTypeDef (parseLoc spec) (parseItem spec)
  if isTDef
    then Left . SyntaxError (parseLoc spec) $ "Invalid typedef statement"
    else do
      declaredType <- readCType (parseLoc spec) sym (parseItem spec)
      case dataType declaredType of
        TUnion (Just label) _ -> return (False, [(False, label, declaredType)])
        TStruct (Just label) _ ->
          return (False, [(False, label, declaredType)])
        TEnum (Just label) _ -> return (False, [(False, label, declaredType)])
        TFunction label _ _ _ -> return (False, [(False, label, declaredType)])
        _ -> Left . SyntaxError (parseLoc spec) $ "No identifier specified"
readDeclaration sym (CDeclaration spec (Just list)) = do
  isTDef <- isTypeDef (parseLoc spec) (parseItem spec)
  if isTDef
    then do
      partialType <- readCType (parseLoc spec) sym (parseItem spec)
      (typedefType, tdLabel) <-
        readTypedefLabel partialType { storageClass = [] } (parseItem list)
      return (True, [(False, tdLabel, typedefType)])
    else do
      partialType <- readCType (parseLoc spec) sym (parseItem spec)
      d <- readInitDeclaratorList sym partialType (parseItem list)
      return (False, d)

{- Read a list of declarations. Return a list of tuples of
   declaration declares a typedef label and a list of tuples of
   a boolean value indicating if this code line initializes the variable,
   variable name & variable type. -}
readDeclarationList ::
     SymbolTable -> CDeclarationList -> Either Error [(Bool, String, CType)]
readDeclarationList sym (CDeclarationList decl Nothing) = do
  d <- readDeclaration sym (parseItem decl)
  if fst d
    then Left . SyntaxError (parseLoc decl) $
           "Typedef declaration inside a declaration list"
    else return (snd d)
readDeclarationList sym (CDeclarationList decl (Just list)) = do
  listHead <- readDeclaration sym (parseItem decl)
  listTail <- readDeclarationList sym (parseItem list)
  if fst listHead
    then Left . SyntaxError (parseLoc decl) $
           "Typedef declaration inside a declaration list"
    else return (snd listHead ++ listTail)

-----------------
-- DECLARATORS --
-----------------

{- Read a direct declarator (a declaration to a variable
   that isn't a pointer) -}
readDirectDeclarator ::
     SymbolTable -> CType -> CDirectDeclarator -> Either Error (String, CType)
readDirectDeclarator
     sym partialType (CDirectDeclaratorId cId directDecl') = do
  declaredType <-
    readDirectDeclarator' sym partialType identifier (parseItem directDecl')
  return (identifier, declaredType)
  where CIdentifier identifier = parseItem cId
readDirectDeclarator
     sym partialType (CDirectDeclaratorParen decl directDecl') = do
  (identifier, partialType') <- readDeclarator sym partialType (parseItem decl)
  declaredType <-
    readDirectDeclarator' sym partialType' identifier (parseItem directDecl')
  return (identifier, declaredType)

readDirectDeclarator' ::
     SymbolTable -> CType -> String -> CDirectDeclarator' -> Either Error CType
readDirectDeclarator' _ t _ CDirectDeclarator'Empty =
  return t
readDirectDeclarator'
     sym
     arrayType
     label
     (CDirectDeclarator'ConstExpr (Just constExpr) directDecl') = do
  arrayLength <-
    evaluateConstantExpression (parseItem constExpr)
      >>= doubleToInteger (parseLoc constExpr)
  let partialType = CType { storageClass = []
                          , typeQualifier = []
                          , dataType = TArray arrayType (Just arrayLength)
                          }
  readDirectDeclarator' sym partialType label (parseItem directDecl')
readDirectDeclarator'
     sym arrayType label (CDirectDeclarator'ConstExpr Nothing directDecl') =
  readDirectDeclarator' sym partialType label (parseItem directDecl')
  where
    partialType = CType { storageClass = []
                        , typeQualifier = []
                        , dataType = TArray arrayType Nothing
                        }
readDirectDeclarator'
     sym
     returnType
     label
     (CDirectDeclarator'ParamTypeList typeList directDecl') = do
  (varArgs, types) <- readParamTypeList sym (parseItem typeList)
  let partialType =
        CType { storageClass = []
              , typeQualifier = []
              , dataType =
                  TFunction label returnType (map snd types) varArgs
              }
  validateFunctionReturnType (parseLoc typeList) returnType
  readDirectDeclarator' sym partialType label (parseItem directDecl')
readDirectDeclarator'
     sym returnType label (CDirectDeclarator'IdList idList directDecl') =
  case idList of
    Just list ->
      Left . SyntaxError (parseLoc list) $
        "No parameter types given in function declaration"
    Nothing ->
      readDirectDeclarator' sym partialType label (parseItem directDecl')
      where partialType =
              CType { storageClass = []
                    , typeQualifier = []
                    , dataType = TFunction label returnType [] True
                    }

{- Read a declarator. Return a tuple of the name
   of the variable declared & its type. -}
readDeclarator ::
     SymbolTable -> CType -> CDeclarator -> Either Error (String, CType)
readDeclarator sym partialType (CDeclarator pointerOpt directDecl) = do
  partialType' <- readPointerOptional partialType pointerOpt
  readDirectDeclarator sym partialType' (parseItem directDecl)

{- Read a direct abstract declarator (direct declarator without a label -}
readDirectAbstractDeclarator ::
     CType -> CDirectAbstractDeclarator -> Either Error CType
readDirectAbstractDeclarator
     arrayType (CDirectAbstractDeclaratorIndexed Nothing decl) =
  readDirectAbstractDeclarator' partialType (parseItem decl)
  where partialType =
          CType { storageClass = []
                , typeQualifier = []
                , dataType = TArray arrayType Nothing
                }
readDirectAbstractDeclarator
     arrayType
     (CDirectAbstractDeclaratorIndexed (Just constExpr) decl) = do
  arrayLength <-
    evaluateConstantExpression (parseItem constExpr)
      >>= doubleToInteger (parseLoc constExpr)
  let partialType =
        CType { storageClass = []
              , typeQualifier = []
              , dataType = TArray arrayType (Just arrayLength)
              }
  readDirectAbstractDeclarator' partialType (parseItem decl)
-- TODO: Find out what this is supposed to do
readDirectAbstractDeclarator _ (CDirectAbstractDeclaratorParen decl _) =
  Left . InternalError (parseLoc decl) $ "unimplemented functionality"
-- TODO: Find out what this is supposed to do
readDirectAbstractDeclarator _ (CDirectAbstractDeclaratorParams _ decl) =
  Left . InternalError (parseLoc decl) $ "unimplemented functionality"

readDirectAbstractDeclarator' ::
     CType -> CDirectAbstractDeclarator' -> Either Error CType
readDirectAbstractDeclarator' t CDirectAbstractDeclarator'Empty =
  return t
readDirectAbstractDeclarator'
     arrayType (CDirectAbstractDeclarator'Const Nothing decl) =
  readDirectAbstractDeclarator' partialType (parseItem decl)
  where partialType =
          CType { storageClass = []
                , typeQualifier = []
                , dataType = TArray arrayType Nothing
                }
readDirectAbstractDeclarator'
     arrayType
     (CDirectAbstractDeclarator'Const (Just constExpr) decl) = do
  arrayLength <-
    evaluateConstantExpression (parseItem constExpr)
      >>= doubleToInteger (parseLoc constExpr)
  let partialType =
        CType { storageClass = []
              , typeQualifier = []
              , dataType = TArray arrayType (Just arrayLength)
              }
  readDirectAbstractDeclarator' partialType (parseItem decl)
-- TODO: find out what this is actually supposed to do
readDirectAbstractDeclarator' _ (CDirectAbstractDeclarator'Params _ decl) =
  Left . SyntaxError (parseLoc decl) $ "Unexpected parameter list"

{- Read an abstract declarator (declarator without a label) -}
readAbstractDeclarator :: CType -> CAbstractDeclarator -> Either Error CType
readAbstractDeclarator pointerType (CAbstractDeclaratorPointer pointer) =
  readPointer pointerType (parseItem pointer)
readAbstractDeclarator
     partialType (CAbstractDeclaratorDirect pointerOpt directDecl) = do
  partialType' <- readPointerOptional partialType pointerOpt
  readDirectAbstractDeclarator partialType' (parseItem directDecl)

-------------
-- HELPERS --
-------------

-- Read the type of a pointer to a variable
readPointer :: CType -> CPointer -> Either Error CType
readPointer pointerType (CPointer Nothing pointerOpt) =
  readPointerOptional partialType pointerOpt
  where partialType =
          CType { storageClass = []
                , typeQualifier = []
                , dataType = TPointer pointerType
                }
readPointer pointerType (CPointer (Just qualifierList) pointerOpt) = do
  qualifiers <-
    readTypeQualifiers (parseLoc qualifierList) (parseItem qualifierList)
  let partialType =
        CType { storageClass = []
              , typeQualifier = qualifiers
              , dataType = TPointer pointerType
              }
  readPointerOptional partialType pointerOpt

readPointerOptional ::
     CType -> Maybe (ParseItem CPointer) -> Either Error CType
readPointerOptional pointerType Nothing = return pointerType
readPointerOptional pointerType (Just pointer) =
  readPointer pointerType (parseItem pointer)

-- Extract a list of labels from CIdentifierList
readIdentifierList :: CIdentifierList -> [String]
readIdentifierList
     (CIdentifierList
       ParseItem { parseItem = (CIdentifier identifier) }
       idList') =
  identifier : readIdentifierList' (parseItem idList')

readIdentifierList' :: CIdentifierList' -> [String]
readIdentifierList' CIdentifierList'Empty = []
readIdentifierList'
     (CIdentifierList'
       ParseItem { parseItem = CIdentifier identifier }
       idList') =
  identifier : readIdentifierList' (parseItem idList')

