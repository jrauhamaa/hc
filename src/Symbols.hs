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

readDeclarationSpecifiers :: Coordinates -> CDeclarationSpecifiers
readDeclarationSpecifiers c spec = do
  let (storageClasses, typeQualifiers, typeSpecifiers) =
    readDeclarationSpecifierList $ parseItem spec
  storageClasses' <- validateStorageClasses c storageClasses
  typeQualifiers' <- validateTypeQualifiers c typeQualifiers
  typeSpecifier' <- validateTypeSpecifiers c typeSpecifiers
  return CType
    { storageClass = storageClasses'
    , typeQualifier = typeQualifiers'
    , dataType = typeSpecifier'
    }

readDeclarationSpecifierList ::
     CDeclarationSpecifiers
  -> ([CStorageClassSpecifier], [CTypeQualifier], [CTypeSpecifier])
readDeclarationSpecifierList (CDeclarationSpecifiersStorageClass spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([parseItem spec], [], [])
    (CDeclarationSpecifiersOptional spec') -> (parseItem spec : storageCls, qualifiers, specifiers)
      where (storageCls, qualifiers, specifiers) = readDeclarationSpecifierList $ parseItem spec'
readDeclarationSpecifierList (CDeclarationSpecifiersTypeQualifier spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([], [parseItem spec], [])
    (CDeclarationSpecifiersOptional spec') -> (storageCls, parseItem spec : qualifiers, specifiers)
      where (storageCls, qualifiers, specifiers) = readDeclarationSpecifierList $ parseItem spec'
readDeclarationSpecifierList (CDeclarationSpecifiersTypeSpecifier spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([], [], [parseItem spec])
    (CDeclarationSpecifiersOptional spec') -> (storageCls, qualifiers, parseItem spec : specifiers)
      where (storageCls, qualifiers, specifiers) = readDeclarationSpecifierList $ parseItem spec'

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
  t <- readDeclarationSpecifiers c $ parseItem spec
  validateFunctionStorageClasses c $ storageClass t
  return t

-- (varargs? [argument types])
readParamTypeList :: CParameterTypeList -> Either TypeError (Bool, [CType])
readParamTypeList (CParameterTypeList paramList varargs) = do
  params <- readParamList paramList
  let varargs' = parseItem varargs == CVarArgsOptionalEmpty
  return (varargs', params)

readParamDeclaration :: CParameterDeclaration -> Either TypeError ((Maybe String), CType)
readParamDeclaration (CParameterDeclaration specifiers paramDecl) =
  case parseItem paramDecl of
    CParameterDeclaration' decl -> do
      t <- readDeclarationSpecifiers specifiers
      (identifier, t') <- readDeclarator t $ parseItem decl
      return (Just identifier, t')
    CParameterDeclaration'Abstract abstractDeclOpt ->
      case parseItem abstractDeclOpt of
        CAbstractDeclaratorOptionalEmpty -> readDeclarationSpecifiers specifiers
        CAbstractDeclaratorOptional abstractDecl -> do
          t <- readDeclarationSpecifiers specifiers
          t' <- readAbstractDeclarator t $ parseItem abstractDecl
          return (Nothing, t')

readParamList' :: CParameterList' -> Either TypeError [((Maybe String), CType)]
readParamList' CParameterList'Empty = []
readParamList' (CParameterList' decl list) = do
  t <- readParamDeclaration decl
  return $ t : readParamList' list

readParamList :: CParameterList -> Either TypeError [((Maybe String), CType)]
readParamList (CParameterList decl list) = do
  t <- readParamDeclaration decl
  return $ t : readParamList' list

-- (varargs?, [(param name, param type)])
readParamTypes :: CParameterTypeList -> Either TypeError (Bool, [((Maybe String), CType)])
readParamTypes (CParameterTypeList list varargs) =
  paramList <- readParamList $ parseItem list
  return ( parseItem varargs == CVarArgsOptionalEmpty
         , paramList
         )

-- returns (function name, function type, param names)
readFunctionDeclaration ::
     Coordinates
  -> CDeclarator
  -> CDeclarationListOptional
  -> CType
  -> Either TypeError (String, CType, [String])
-- new style
readFunctionDeclaration c (CDeclarator pointer directDeclarator) CDeclarationListOptionalEmpty t =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ -> Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'ParamTypeList typeList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              (varargs, params) <- readParamTypes $ parseItem typeList
              let functionType = if varargs
                then CType { storageClass :: []
                           , typeQualifier :: []
                           , dataType :: TVarArgFunction returnType $ map snd params
                           }
                else CType { storageClass :: []
                           , typeQualifier :: []
                           , dataType :: TFunction returnType $ map snd params
                           }
              case traverse fst params of
                Nothing -> Left c "Unnamed function parameter(s)"
                Just paramNames ->
                  if count $ S.fromList paramNames == count paramNames
                    then return (fName, functionType, paramNames)
                    else Left $ TypeError c "Conflicting parameter names"
            _ -> Left $ TypeError c "Invalid function declaration"
        _ -> Left $ TypeError c "Invalid function declaration"
  where
    returnType = readPointerOptional t $ parseItem pointer
-- old style
readFunctionDeclaration c (CDeclarator pointer directDeclarator) (CDeclarationListOptional declList) t =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ -> Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'IdList idList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              let paramNames = case parseItem idList of
                CIdentifierListOptionalEmpty -> []
                CIdentifierListOptional idList' -> readIdentifierList $ parseItem idList'
              params <- readDeclarationList $ parseItem declList
              let paramTypeLookup = M.fromList $ map (\(_, k, v) -> (k, v)) params
              let paramTypes = map (\k -> M.lookup k paramTypeLookup) paramNames
              case sequenceA paramTypes of
                Nothing -> TypeError c "Internal error parsing function parameter types"  -- this shouldn't happen
                Just paramTypes' ->
                  let fType =
                    CType { storageClass :: []
                          , typeQualifier :: []
                          , dataType :: TFunction returnType paramTypes'
                          }
                  if (S.fromList paramNames == S.fromList paramNames')
                       && ((count $ S.fromList paramNames) == count paramNames)
                       && all (\(initialized, _, _) -> not initialized) params
                    then return (fName, fType, paramNames)
                    else Left $ TypeError c "Invalid parameter declaration"
            _ -> Left $ TypeError c "Invalid function declaration"
        _ -> Left $ TypeError c "Invalid function declaration"
  where
    returnType = readPointerOptional t $ parseItem pointer

readFunctionDefinition ::
     PI CDeclarationSpecifiersOptional
  -> PI CDeclarator
  -> PI CDeclarationListOptional
  -> SymbolTable
  -> Either TypeError (String, CType, [String])
readFunctionDefinition specifiersOpt declarator declarationList sym =
  case parseItem specifiersOpt of
    CDeclarationSpecifiersOptionalEmpty ->
      Left (parseLoc specifiersOpt) "Function return type not specified"
    CDeclarationSpecifiersOptional specifiers -> do
      returnType <- readDeclarationSpecifiers
                      (parseLoc specifiers)
                      (parseItem specifiers)
      readFunctionDeclaration
        (parseLoc declarator)
        (parseItem declarator)
        (parseItem declarationList)
        returnType

------------------
-- DECLARATIONS --
------------------

-- (assigned?, identifier, type)
readInitDeclarator :: CType -> CInitDeclarator -> Either TypeError (Bool, String, CType)
readInitDeclarator t (CInitDeclarator decl initOpt) = do
  (identifier, t') <- readDeclarator t $ parseItem decl
  return ( parseItem initOpt == CAssignInitializerOptionalEmpty
         , identifier
         , t'
         )

readInitDeclaratorList' :: CType -> CInitDeclaratorList' -> Either TypeError [(Bool, String, CType)]
readInitDeclaratorList' t CInitDeclaratorList'Empty = Right []
readInitDeclaratorList' t (CInitDeclaratorList' decl list) =
  d <- readInitDeclarator t $ parseItem decl
  listTail <- readInitDeclaratorList' t $ parseItem list
  return d : listTail

readInitDeclaratorList :: CType -> CInitDeclaratorList -> Either TypeError [(Bool, String, CType)]
readInitDeclaratorList t (CInitDeclaratorList decl list) =
  d <- readInitDeclarator t $ parseItem decl
  listTail <- readInitDeclaratorList' t $ parseItem list
  return d : listTail

readDeclaration :: CDeclaration -> Either TypeError [(Bool, String, CType)]
readDeclaration (CDeclaration spec initDeclarators) =
  case parseItem initDeclarations of
    CInitDeclaratorListOptionalEmpty ->
      Left $ TypeError (parseLoc initDeclarations) "No identifier specified"
    CInitDeclaratorListOptional list -> do
      t <- readDeclarationSpecifiers $ parseItem spec
      readInitDeclaratorList t $ parseItem list

readDeclarationList :: CDeclarationList -> Either TypeError [(Bool, String, CType)]
readDeclarationList (CDeclarationList decl listOpt) =
  case parseItem listOpt of
    CDeclarationListOptionalEmpty -> readDeclaration $ parseItem decl
    CDeclarationListOptional list -> do
      listHead <- readDeclaration $ parseItem decl
      listTail <- readDeclarationList list
      return $ listHead ++ listTail

-----------------
-- DECLARATORS --
-----------------

readDirectDeclarator' :: CType -> CDirectDeclarator' -> Either TypeError CType
readDirectDeclarator' t CDirectDeclarator'Empty = Right t
readDirectDeclarator' t CDirectDeclarator'ConstExpr _ directDecl' =
  readDirectDeclarator' t' directDecl'
  where t' = TArray t Nothing
readDirectDeclarator' t CDirectDeclarator'ParamTypeList typeList directDecl' = do
  (varArgs, types) <- readParamTypeList $ parseItem typeList
  let t' = if varArgs
    then TVarArgFunction t types
    else TFunction t types
  readDirectDeclarator' t' directDecl'
readDirectDeclarator' t CDirectDeclarator'IdList idList directDecl' =
  case parseItem idList of
    CIdentifierListOptionalEmpty ->
      readDirectDeclarator' t' directDecl'
      where t' = TVarArgFunction t []
    _ -> Left $ TypeError "No parameter types given in function declaration"

readDirectDeclarator :: CType -> CDirectDeclarator -> Either TypeError (String, CType)
readDirectDeclarator t directDecl =
  case parseItem directDecl of
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier identifier) _) directDecl' -> do
      t' <- readDirectDeclarator' t directDecl'
      return (identifier, t')
    CDirectDeclaratorParen decl directDecl' ->  do
      (identifier, t') <- readDeclarator t decl
      t'' <- readDirectDeclarator' t' directDecl'
      return (identifier, t'')

readDeclarator :: CType -> CDeclarator -> Either TypeError (String, CType)
readDeclarator t (CDeclarator pointer directDecl) =
  readDirectDeclarator t' $ parseItem directDecl
  where t' = readPointerOptional t pointer

readDirectAbstractDeclarator' :: CType -> CDirectAbstractDeclarator' -> Either TypeError CType
readDirectAbstractDeclarator' t CDirectAbstractDeclarator'Empty = Right t
readDirectAbstractDeclarator' t (CDirectAbstractDeclarator'Const _ decl) =
  readDirectAbstractDeclarator' (TArray t Nothing) $ parseItem decl
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator' t (CDirectAbstractDeclarator'Params item _) =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readDirectAbstractDeclarator :: CType -> CDirectAbstractDeclarator -> Either TypeError CType
readDirectAbstractDeclarator t (CDirectAbstractDeclaratorIndexed _ decl) =
  readDirectAbstractDeclarator' (TArray t Nothing) $ parseItem decl
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator _ (CDirectAbstractDeclaratorParen item _) =
  Left $ TypeError (parseLoc item) "Parenthesized type"
-- Same here
readDirectAbstractDeclarator _ (CDirectAbstractDeclaratorParams item _) =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"
readDirectAbstractDeclarator t (CDirectAbstractDeclaratorP _ decl) =

readAbstractDeclarator :: CType -> CAbstractDeclarator -> Either TypeError CType
readAbstractDeclarator t (CAbstractDeclaratorPointer pointer) =
  readPointer t pointer
readAbstractDeclarator t (CAbstractDeclaratorDirect pointerOpt directDecl) =
  readDirectAbstractDeclarator t' $ parseItem directDecl
  where t' = readPointerOptional t pointerOpt

-------------
-- HELPERS --
-------------

readPointer :: CType -> CPointer -> CType
readPointer t (CPointer qualifierList pointerOpt)
  readPointerOptional t' $ parseItem pointerOpt
  where
    qualifiers = readTypeQualifiers $ parseItem qualifierList
    t' = CType
      { storageClass = []
      , typeQualifier = qualifiers
      , dataType = TPointer t
      }

readPointerOptional :: CType -> CPointerOptional -> CType
readPointerOptional t CPointerOptionalEmpty = t
readPointerOptional t (CPointerOptional pointer) = readPointer t pointer

readIdentifierList' :: CIdentifierList' -> [String]
readIdentifierList' CIdentifierList'Empty -> []
readIdentifierList' CIdentifierList' (ParseItem _ _ (CIdentifier identifier) _) -> []
  identifier : readIdentifierList' $ parseItem idList'

readIdentifierList :: CIdentifierList -> [String]
readIdentifierList CIdentifierList (ParseItem _ _ (CIdentifier identifier) _) idList' =
  identifier : readIdentifierList' $ parseItem idList'

