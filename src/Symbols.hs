module IR where

import Control.Applicative (<|>)
import qualified Data.Set as S
import qualified Data.Map as M

import Scanner (Coordinates)
import ParseItem

addSymbols :: ParseItem a -> SymbolTable -> ParseItem a
addSymbols (ParseItem l s i _) = ParseItem l s i

type TypeAnnotate a
  = ParseItem a -> SymbolTable -> Either TypeError (ParseItem a)

data TypeError =
  TypeError
    { errorLoc :: Coordinates
    , errorMsg :: String
    }

lookupSymbols :: String -> SymbolTable -> Maybe CType
lookupSymbols s sym =
  case parent sym of
    Just sym' -> v <|> lookupSymbols sym'
    Nothing -> v
  where
    v = snd <$> M.lookup s (symbols sym) <|> M.lookup s (typedef sym)


---------------------
-- SPECIFIER LISTS --
---------------------

validateStorageClasses ::
     Coordinates
  -> [CStorageClassSpecifier]
  -> Either TypeError [CStorageClassSpecifier]
validateStorageClasses c spec
  | length spec > 1 = Left $ TypeError c "Multiple storage classes"
  | otherwise = Right spec

validateTypeSpecifiers ::
     Coordinates
  -> [CTypeSpecifier]
  -> Either TypeError CDataType
validateTypeSpecifiers c spec
  | length spec == 0 = Left $ TypeError c "Type unspecified"
  | length lengthSpec > 2 = Left $ TypeError c "Too many type size specifiers"
  | length dataTypes > 1 = Left $ TypeError c "Too many datatype specifiers"
  | length signedSpec > 1 = Left $ TypeError c "Too many datatype sign specifiers"
  -- long long int
  | lengthSpec == [CTypeSpecifierLong, CTypeSpecifierLong] =
      if dataTypes == [] || dataTypes == [CTypeSpecifierInt]
        then
          if signedSpec == [CTypeSpecifierUnsigned]
            then Right TULongLong
            else Right TLongLong
        else Left $ TypeError c "Long long can only be used with integers"
  -- short int
  | lengthSpec == [CTypeSpecifierShort] =
      if dataTypes == [] || first dataTypes == CTypeSpecifierInt
        then
          if signedSpec == [CTypeSpecifierUnsigned]
            then Right TUShort
            else Right TShort
        else Left $ TypeError c "Short can only be used with integers"
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
          if signedSpec == []
            then Right TLongDouble
            else Left $ TypeError c "Can't specify sign of double"
        _ -> Left $ TypeError c "Long can only be used with integers and doubles"
  -- (un)signed int
  | length signedSpec > 0 && length dataTypes == 0 =
      case first signedSpec of
        CTypeSpecifierSigned -> Right TShort
        _ -> Right TUShort
  -- bad combination of (un)signed & type
  | length signedSpec > 0 && (first dataTypes) `notElem` [ CTypeSpecifierChar
                                                         , CTypeSpecifierInt ] =
      Left $ TypeError c "Bad combination of datatype sign specifier & type"
  -- unsigned type
  | signedSpec == [CTypeSpecifierUnsigned] =
      case first dataTypes of
        CTypeSpecifierChar -> Right TUChar
        _ -> Right TUShort
  | otherwise =
      case first dataTypes of
        CTypeSpecifierChar -> Right TChar
        CTypeSpecifierInt -> Right TShort
        CTypeSpecifierFloat -> Right TFloat
        CTypeSpecifierDouble -> Right TDouble
        t -> readStructuredType t
  where
    readStructuredType (CTypeSpecifierEnum spec) =
      readEnumSpecifier $ parseItem spec
    readStructuredType (CTypeSpecifierStructOrUnion spec) =
      readStructOrUnionSpecifier $ parseItem spec
    readStructuredType (CTypeSpecifierTypedef t) =
      readTypedefName $ parseItem t
    lengthSpec = filter (`elem` [CTypeSpecifierLong, CTypeSpecifierShort]) spec
    dataTypes = filter (`notElem` [ CTypeSpecifierLong
                                  , CTypeSpecifierShort
                                  , CTypeSpecifierSigned
                                  , CTypeSpecifierUnsigned
                                  ]) spec
    signedSpec = filter (`elem` [CTypeSpecifierSigned, CTypeSpecifierUnsigned]) spec

validateTypeQualifiers ::
     Coordinates
  -> [CTypeQualifier]
  -> Either TypeError [CTypeQualifier]
validateTypeQualifiers c spec
  | length spec > 2 = Left $ TypeError c "Repeated type qualifiers"
  | spec == [CTypeQualifierConst, CTypeQualifierConst]
      = Left $ TypeError c "Repeated type qualifiers"
  | spec == [CTypeQualifierVolatile, CTypeQualifierVolatile]
      = Left $ TypeError c "Repeated type qualifiers"
  | otherwise = Right spec

validateFunctionStorageClasses ::
     Coordinates
  -> [CStorageClassSpecifier]
  -> Either TypeError [CStorageClassSpecifier]
validateFunctionStorageClasses c spec =
  if spec `elem` [[], [CStorageClassSpecifierExtern], [CStorageClassSpecifierStatic]]
    then Right spec
    else Left $ TypeError c "Illegal storage class specifier in function definition"

readTypeQualifiers :: CTypeQualifierListOptional -> [CTypeQualifier]
readTypeQualifiers CTypeQualifierListOptionalEmpty = []
readTypeQualifiers (CTypeQualifierListOptional (ParseItem _ _ (CTypeQualifierList qualifier list) _)) =
  (parseItem qualifier) : (readTypeQualifiers $ parseItem list)

readSpecifierQualifierList ::
    CSpecifierQualifierList -> ([CTypeSpecifier], [CTypeQualifier])
readSpecifierQualifierList (CSpecifierQualifierListSpecifier spec list) =
  case parseItem list of
    CSpecifierQualifierListOptional list' ->
      let (specifiers, qualifiers) = readSpecifierQualifierList list'
       in (parseItem spec : specifiers, qualifiers)
    CSpecifierQualifierListOptionalEmpty ->
      ([spec], [])
readSpecifierQualifierList (CSpecifierQualifierListQualifier qualifier list)
  case parseItem list of
    CSpecifierQualifierListOptional list' ->
      let (specifiers, qualifiers) = readSpecifierQualifierList list'
       in (specifiers, parseItem qualifier : qualifiers)
    CSpecifierQualifierListOptionalEmpty ->
      ([], [parseItem qualifier])

readType :: Coordinates -> CDeclarationSpecifiers
readType c spec = do
  let (storageClasses, typeQualifiers, typeSpecifiers) =
    readDeclarationSpecifiers $ parseItem spec
  storageClasses' <- validateStorageClasses c storageClasses
  typeQualifiers' <- validateTypeQualifiers c typeQualifiers
  typeSpecifier' <- validateTypeSpecifiers c typeSpecifiers
  return CType
    { storageClass = storageClasses'
    , typeQualifier = typeQualifiers'
    , dataType = typeSpecifier'
    }

----------------------
-- STRUCTURED TYPES --
----------------------

readEnumSpecifier :: Coordinates -> SymbolTable -> CEnumSpecifier -> Either TypeError CDataType
readEnumSpecifier c sym (CEnumSpecifier (ParseItem _ _ (CIdentifier identifier) _)) =
  case lookupSymbols identifier sym of
    Just t -> Right t
    Nothing -> Left $ TypeError c "Undefined enum '" ++ identifier ++ "'."

readStructOrUnionSpecifier ::
     Coordinates
  -> SymbolTable
  -> CStructOrUnionSpecifier
  -> Either TypeError (Maybe String, CType)
readStructOrUnionSpecifier c sym (CStructOrUnionSpecifier structOrUnion (ParseItem _ _ (CIdentifier identifier) _)) =
  case M.lookup identifier $ symbols sym of
    Just (_, t) ->
      case t of
        TUnion _ -> if parseItem structOrUnion == CStructOrUnionUnion
                      then Right t
                      else Left $ TypeError c "conflicting type name: '" ++ identifier "'."
        TStruct _ -> if parseItem structOrUnion == CStructOrUnionStruct
                       then Right t
                       else Left $ TypeError c "conflicting type name: '" ++ identifier "'."
    Nothing -> Left $ TypeError c "Undefined struct or union '" ++ identifier ++ "'."
readStructOrUnionSpecifier c sym (CStructOrUnionSpecifier structOrUnion identifierOpt decl) = do
  declarations <- readStructDeclarationList decl
  let identifiers = map fst declarations
  let name = case parseItem identifierOpt of
    CIdentifierOptionalEmpty -> Nothing
    CIdentifierOptional (ParseItem _ _ (CIdentifier i) _) -> Just i
  if count $ S.fromList identifiers = count identifiers
    then
      case parseItem structOrUnion of
        CStructOrUnionStruct -> Right (name, TStruct declarations)
        CStructOrUnionUnion -> Right (name, TUnion $ M.fromList declarations)
    else Left $ TypeError c "Conflicting identifiers"

readStructDeclaratorList :: Coordinates -> CType -> CStructDeclaratorList -> [(CDataType, Maybe String, Maybe Int)]

readStructDeclaration ::
     Coordinates
  -> CStructDeclaration
  -> Either TypeError [(CDataType, Maybe String, Maybe Int)]
readStructDeclaration c (CStructDeclaration specList declList) = do
  let (typeSpecifiers, typeQualifiers) = readSpecifierQualifierList eClasses c storageClasses
  typeQualifiers' <- validateTypeQualifiers c typeQualifiers
  typeSpecifier' <- validateTypeSpecifiers c typeSpecifiers
  let t = CType
    { storageClass = storageClasses'
    , typeQualifier = typeQualifiers'
    , dataType = typeSpecifier'
    }
  readStructDeclaratorList c t declList

readStructDeclarationList ::
     Coordinates
  -> CStructDeclarationList
  -> Either TypeError [(CDataType, Maybe String, Maybe Int)]
readStructDeclarationList c (CStructDeclarationList decl list) =
  case parseItem list of
    CStructDeclarationListOptional list' -> do
      decl' <- readStructDeclaration c $ parseItem decl
      declarations <- readStructDeclarationList c $ parseItem list'
      return $ decl' : declarations
    CStructDeclarationListOptionalEmpty -> do
      decl' <- readStructDeclaration c $ parseItem decl
      return [decl']

readTypedefName ::
     Coordinates -> SymbolTable -> CTypedefName -> Either TypeError CType
readTypedefName c sym (CTypedefName (ParseItem _ _ (CIdentifier identifier) _)) =
  case M.lookup identifier $ typedef sym of
    Just (_, t) -> Right t
    Nothing -> Left $ TypeError c "Unknown typedef name '" ++ identifier ++ "'."

---------------
-- FUNCTIONS --
---------------

functionReturnType ::
     PI CDeclarationSpecifiersOptional -> Either TypeError CType
functionReturnType (ParseItem _ _ CDeclarationSpecifiersOptionalEmpty _) =
  return CType
    { storageClass = []
    , typeQualifier = []
    , dataType = TShort
    }
functionReturnType (ParseItem c _ (CDeclarationSpecifiersOptional spec) _) = do
  t <- readType c $ parseItem spec
  validateFunctionStorageClasses c $ storageClass t
  return t

readDeclarationSpecifiers ::
     CDeclarationSpecifiers
  -> ([CStorageClassSpecifier], [CTypeQualifier], [CTypeSpecifier])
readDeclarationSpecifiers (CDeclarationSpecifiersStorageClass spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([parseItem spec], [], [])
    (CDeclarationSpecifiersOptional spec') -> (parseItem spec : storageCls, qualifiers, specifiers)
      where (storageCls, qualifiers, specifiers) = readDeclarationSpecifiers $ parseItem spec'
readDeclarationSpecifiers (CDeclarationSpecifiersTypeQualifier spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([], [parseItem spec], [])
    (CDeclarationSpecifiersOptional spec') -> (storageCls, parseItem spec : qualifiers, specifiers)
      where (storageCls, qualifiers, specifiers) = readDeclarationSpecifiers $ parseItem spec'
readDeclarationSpecifiers (CDeclarationSpecifiersTypeSpecifier spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([], [], [parseItem spec])
    (CDeclarationSpecifiersOptional spec') -> (storageCls, qualifiers, parseItem spec : specifiers)
      where (storageCls, qualifiers, specifiers) = readDeclarationSpecifiers $ parseItem spec'


readPointerOptional :: CType -> CPointerOptional -> CType
readPointerOptional t CPointerOptionalEmpty = t
readPointerOptional t (CPointerOptional (ParseItem _ _ (CPointer qualifierList pointer) _)) =
  readPointerOptional t' $ parseItem pointer
  where
    qualifiers = readTypeQualifiers $ parseItem qualifierList
    t' = CType
      { storageClass = []
      , typeQualifier = qualifiers
      , dataType = TPointer t
      }

readParamDeclaration :: CParameterDeclaration -> Either TypeError CType
readParamDeclaration (CParameterDeclaration specifiers decl) = do
  t <- readType specifiers

readParamList' :: CParameterList' -> [(String, CType)]
readParamList' CParameterList'Empty = []
readParamList' (CParameterList' decl list) =

readParamList :: CParameterList -> [(String, CType)]
readParamList (CParameterList decl list) = undefined

-- (varargs?, [(param name, param type)])
readParamTypes :: CParameterTypeList -> (Bool, [(String, CType)])
readParamTypes (CParameterTypeList list varargs) =
  ( parseItem varargs == CVarArgsOptionalEmpty
  , readParamList $ parseItem list
  )


functionDeclarator
     :: Coordinates
  -> CDeclarator
  -> CDeclarationListOptional
  -> CType
  -> Either TypeError (String, CType)
-- new style
functionDeclarator c (CDeclarator pointer directDeclarator) CDeclarationListOptionalEmpty t =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ -> Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'ParamTypeList typeList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty ->
              let (varargs, params) = readParamTypes $ parseItem typeList
               in if varargs
                    then CType { storageClass :: []
                               , typeQualifier :: []
                               , dataType :: TVarArgFunction returnType params
                               }
                    else CType { storageClass :: []
                               , typeQualifier :: []
                               , dataType :: TFunction returnType params
                               }
            _ -> Left $ TypeError c "Invalid function declaration"
        _ -> Left $ TypeError c "Invalid function declaration"
  where
    returnType = readPointerOptional t $ parseItem pointer

functionType ::
     PI CDeclarationSpecifiersOptional
  -> PI CDeclarator
  -> PI CDeclarationListOptional
  -> SymbolTable
  -> Either TypeError (String, CType)
functionType = undefined

addSymbol :: SymbolTable -> String -> CType -> Either TypeError SymbolTable
addSymbol = undefined

