module Symbols where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

import ParseItem
import Scanner (Coordinates)

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
    Just sym' -> v <|> lookupSymbols s sym'
    Nothing -> v
  where
    v = snd <$> (M.lookup s (symbols sym) <|> M.lookup s (typedef sym))

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
  -> SymbolTable
  -> [CTypeSpecifier]
  -> Either TypeError CDataType
validateTypeSpecifiers c sym spec
  | length spec == 0 = Left $ TypeError c "Type unspecified"
  | length lengthSpec > 2 = Left $ TypeError c "Too many type size specifiers"
  | length dataTypes > 1 = Left $ TypeError c "Too many datatype specifiers"
  | length signedSpec > 1 =
    Left $ TypeError c "Too many datatype sign specifiers"
  -- long long int
  | lengthSpec == [CTypeSpecifierLong, CTypeSpecifierLong] =
    if dataTypes == [] || dataTypes == [CTypeSpecifierInt]
      then if signedSpec == [CTypeSpecifierUnsigned]
             then Right TULongLong
             else Right TLongLong
      else Left $ TypeError c "Long long can only be used with integers"
  -- short int
  | lengthSpec == [CTypeSpecifierShort] =
    if dataTypes == [] || head dataTypes == CTypeSpecifierInt
      then if signedSpec == [CTypeSpecifierUnsigned]
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
    case head signedSpec of
      CTypeSpecifierSigned -> Right TShort
      _ -> Right TUShort
  -- bad combination of (un)signed & type
  | length signedSpec > 0 &&
      (head dataTypes) `notElem` [CTypeSpecifierChar, CTypeSpecifierInt] =
    Left $ TypeError c "Bad combination of datatype sign specifier & type"
  -- unsigned type
  | signedSpec == [CTypeSpecifierUnsigned] =
    case head dataTypes of
      CTypeSpecifierChar -> Right TUChar
      _ -> Right TUShort
  | otherwise =
    case head dataTypes of
      CTypeSpecifierChar -> Right TChar
      CTypeSpecifierInt -> Right TShort
      CTypeSpecifierFloat -> Right TFloat
      CTypeSpecifierDouble -> Right TDouble
      CTypeSpecifierEnum spec -> readEnumSpecifier c sym $ parseItem spec
      CTypeSpecifierStructOrUnion spec -> readStructOrUnionSpecifier c sym $ parseItem spec
      CTypeSpecifierTypedef t -> readTypedefName c sym $ parseItem t
      t -> Left $ TypeError c "Internal error reading type"  -- this shoudn't happen
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

validateTypeQualifiers ::
     Coordinates -> [CTypeQualifier] -> Either TypeError [CTypeQualifier]
validateTypeQualifiers c spec
  | length spec > 2 = Left $ TypeError c "Repeated type qualifiers"
  | spec == [CTypeQualifierConst, CTypeQualifierConst] =
    Left $ TypeError c "Repeated type qualifiers"
  | spec == [CTypeQualifierVolatile, CTypeQualifierVolatile] =
    Left $ TypeError c "Repeated type qualifiers"
  | otherwise = Right spec

validateFunctionStorageClasses ::
     Coordinates
  -> [CStorageClassSpecifier]
  -> Either TypeError [CStorageClassSpecifier]
validateFunctionStorageClasses c spec =
  if spec `elem`
     [[], [CStorageClassSpecifierExtern], [CStorageClassSpecifierStatic]]
    then Right spec
    else Left $
         TypeError c "Illegal storage class specifier in function definition"

readTypeQualifiers :: CTypeQualifierListOptional -> [CTypeQualifier]
readTypeQualifiers CTypeQualifierListOptionalEmpty = []
readTypeQualifiers (CTypeQualifierListOptional (ParseItem _ _ (CTypeQualifierList qualifier list) _)) =
  (parseItem qualifier) : (readTypeQualifiers $ parseItem list)

readSpecifierQualifierList ::
     CSpecifierQualifierList -> ([CTypeSpecifier], [CTypeQualifier])
readSpecifierQualifierList (CSpecifierQualifierListSpecifier spec list) =
  case parseItem list of
    CSpecifierQualifierListOptional list' ->
      let (specifiers, qualifiers) = readSpecifierQualifierList $ parseItem list'
       in (parseItem spec : specifiers, qualifiers)
    CSpecifierQualifierListOptionalEmpty -> ([parseItem spec], [])
readSpecifierQualifierList (CSpecifierQualifierListQualifier qualifier list) =
  case parseItem list of
    CSpecifierQualifierListOptional list' ->
      let (specifiers, qualifiers) = readSpecifierQualifierList $ parseItem list'
       in (specifiers, parseItem qualifier : qualifiers)
    CSpecifierQualifierListOptionalEmpty -> ([], [parseItem qualifier])

readDeclarationSpecifiers ::
     Coordinates
  -> SymbolTable
  -> CDeclarationSpecifiers
  -> Either TypeError CType
readDeclarationSpecifiers c sym spec = do
  let (storageClasses, typeQualifiers, typeSpecifiers) =
        readDeclarationSpecifierList spec
  storageClasses' <- validateStorageClasses c storageClasses
  typeQualifiers' <- validateTypeQualifiers c typeQualifiers
  typeSpecifier' <- validateTypeSpecifiers c sym typeSpecifiers
  return
    CType
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
    (CDeclarationSpecifiersOptional spec') ->
      (parseItem spec : storageCls, qualifiers, specifiers)
      where (storageCls, qualifiers, specifiers) =
              readDeclarationSpecifierList $ parseItem spec'
readDeclarationSpecifierList (CDeclarationSpecifiersTypeQualifier spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([], [parseItem spec], [])
    (CDeclarationSpecifiersOptional spec') ->
      (storageCls, parseItem spec : qualifiers, specifiers)
      where (storageCls, qualifiers, specifiers) =
              readDeclarationSpecifierList $ parseItem spec'
readDeclarationSpecifierList (CDeclarationSpecifiersTypeSpecifier spec item) =
  case parseItem item of
    CDeclarationSpecifiersOptionalEmpty -> ([], [], [parseItem spec])
    (CDeclarationSpecifiersOptional spec') ->
      (storageCls, qualifiers, parseItem spec : specifiers)
      where (storageCls, qualifiers, specifiers) =
              readDeclarationSpecifierList $ parseItem spec'

----------------------
-- STRUCTURED TYPES --
----------------------

readEnumSpecifier ::
     Coordinates -> SymbolTable -> CEnumSpecifier -> Either TypeError CDataType
readEnumSpecifier c sym (CEnumSpecifier (ParseItem _ _ (CIdentifier identifier) _)) =
  case lookupSymbols identifier sym of
    Just t -> Right $ dataType t
    Nothing -> Left $ TypeError c $ "Undefined enum '" ++ identifier ++ "'."

readStructOrUnionSpecifier ::
     Coordinates
  -> SymbolTable
  -> CStructOrUnionSpecifier
  -> Either TypeError (Maybe String, CDataType)
readStructOrUnionSpecifier c sym (CStructOrUnionSpecifier structOrUnion (ParseItem _ _ (CIdentifier identifier) _)) =
  case lookupSymbols identifier sym of
    Nothing ->
      Left $ TypeError c $ "Undefined struct or union '" ++ identifier ++ "'."
    Just t ->
      case dataType t of
        TUnion _ ->
          if parseItem structOrUnion == CStructOrUnionUnion
            then Right (Just identifier, dataType t)
            else Left $
                 TypeError c $ "conflicting type name: '" ++ identifier ++ "'."
        TStruct _ ->
          if parseItem structOrUnion == CStructOrUnionStruct
            then Right (Just identifier, dataType t)
            else Left $
                 TypeError c $ "conflicting type name: '" ++ identifier ++ "'."
readStructOrUnionSpecifier c sym (CStructOrUnionSpecifierList structOrUnion identifierOpt decl) = do
  declarations <- readStructDeclarationList c sym $ parseItem decl
  let identifiers = filter (/= Nothing) $ map (\(_, i, _) -> i) declarations
  let name =
        case parseItem identifierOpt of
          CIdentifierOptionalEmpty -> Nothing
          CIdentifierOptional (ParseItem _ _ (CIdentifier i) _) -> Just i
  if (length $ S.fromList identifiers) /= length identifiers
    then Left $ TypeError c "Conflicting identifiers"
    else case parseItem structOrUnion of
           CStructOrUnionStruct -> Right (name, TStruct declarations)
           CStructOrUnionUnion ->
             case traverse (\(_, identifier, _) -> identifier) declarations of
               Nothing -> Left $ TypeError c "Union identifier not specified"
               Just identifiers ->
                 let unionTypes = M.fromList $ zip identifiers $ map (\(t, _, _) -> t) declarations
                  in Right (name, TUnion unionTypes)

readStructDeclarator ::
     Coordinates
  -> SymbolTable
  -> CType
  -> CStructDeclarator
  -> Either TypeError (CDataType, Maybe String, Maybe Int)
readStructDeclarator _ sym t (CStructDeclarator decl) = do
  (identifier, t) <- readDeclarator sym t $ parseItem decl
  return (dataType t, Just identifier, Nothing)

readStructDeclaratorList' ::
     Coordinates
  -> SymbolTable
  -> CType
  -> CStructDeclaratorList'
  -> Either TypeError [(CDataType, Maybe String, Maybe Int)]
readStructDeclaratorList' _ _ _ CStructDeclaratorList'Empty = Right []
readStructDeclaratorList' c sym t (CStructDeclaratorList' decl list) = do
  d <- readStructDeclarator c sym t $ parseItem decl
  listTail <- readStructDeclaratorList' c sym t $ parseItem list
  return $ d : listTail

readStructDeclaratorList ::
     Coordinates
  -> SymbolTable
  -> CType
  -> CStructDeclaratorList
  -> Either TypeError [(CDataType, Maybe String, Maybe Int)]
readStructDeclaratorList c sym t (CStructDeclaratorList decl list) = do
  d <- readStructDeclarator c sym t $ parseItem decl
  listTail <- readStructDeclaratorList' c sym t $ parseItem list
  return $ d : listTail

readStructDeclaration ::
     Coordinates
  -> SymbolTable
  -> CStructDeclaration
  -> Either TypeError [(CDataType, Maybe String, Maybe Int)]
readStructDeclaration c sym (CStructDeclaration specList declList) = do
  let (typeSpecifiers, typeQualifiers) =
        readSpecifierQualifierList $ parseItem specList
  typeQualifiers' <- validateTypeQualifiers c typeQualifiers
  typeSpecifier' <- validateTypeSpecifiers c sym typeSpecifiers
  let t =
        CType
          { storageClass = []
          , typeQualifier = typeQualifiers'
          , dataType = typeSpecifier'
          }
  readStructDeclaratorList c sym t $ parseItem declList

readStructDeclarationList ::
     Coordinates
  -> SymbolTable
  -> CStructDeclarationList
  -> Either TypeError [(CDataType, Maybe String, Maybe Int)]
readStructDeclarationList c sym (CStructDeclarationList decl list) =
  case parseItem list of
    CStructDeclarationListOptional list' -> do
      decl' <- readStructDeclaration c sym $ parseItem decl
      declarations <- readStructDeclarationList c sym $ parseItem list'
      return $ decl' ++ declarations
    CStructDeclarationListOptionalEmpty ->
      readStructDeclaration c sym $ parseItem decl

readTypedefName ::
     Coordinates -> SymbolTable -> CTypedefName -> Either TypeError CType
readTypedefName c sym (CTypedefName (ParseItem _ _ (CIdentifier identifier) _)) =
  case M.lookup identifier $ typedef sym of
    Just (_, t) -> Right t
    Nothing -> Left $ TypeError c $ "Unknown typedef name '" ++ identifier ++ "'."

---------------
-- FUNCTIONS --
---------------

functionReturnType ::
     SymbolTable -> PI CDeclarationSpecifiersOptional -> Either TypeError CType
functionReturnType _ (ParseItem _ _ CDeclarationSpecifiersOptionalEmpty _) =
  return CType {storageClass = [], typeQualifier = [], dataType = TShort}
functionReturnType sym (ParseItem c _ (CDeclarationSpecifiersOptional spec) _) = do
  t <- readDeclarationSpecifiers c sym $ parseItem spec
  validateFunctionStorageClasses c $ storageClass t
  return t

-- (varargs? [argument types])
readParamTypeList ::
     SymbolTable -> CParameterTypeList -> Either TypeError (Bool, [CType])
readParamTypeList sym (CParameterTypeList paramList varargs) = do
  params <- readParamList sym $ parseItem paramList
  let varargs' = parseItem varargs == CVarArgsOptionalEmpty
  return (varargs', map snd params)

readParamDeclaration ::
     SymbolTable
  -> CParameterDeclaration
  -> Either TypeError ((Maybe String), CType)
readParamDeclaration sym (CParameterDeclaration specifiers paramDecl) =
  case parseItem paramDecl of
    CParameterDeclaration' decl -> do
      t <- readDeclarationSpecifiers
             (parseLoc specifiers)
             sym
             (parseItem specifiers)
      (identifier, t') <- readDeclarator sym t $ parseItem decl
      return (Just identifier, t')
    CParameterDeclaration'Abstract abstractDeclOpt ->
      case parseItem abstractDeclOpt of
        CAbstractDeclaratorOptionalEmpty -> do
          t <- readDeclarationSpecifiers
                 (parseLoc specifiers)
                 sym
                 (parseItem specifiers)
          return (Nothing, t)
        CAbstractDeclaratorOptional abstractDecl -> do
          t <- readDeclarationSpecifiers
                 (parseLoc specifiers)
                 sym
                 (parseItem specifiers)
          t' <- readAbstractDeclarator t $ parseItem abstractDecl
          return (Nothing, t')

readParamList' ::
     SymbolTable
  -> CParameterList'
  -> Either TypeError [((Maybe String), CType)]
readParamList' _ CParameterList'Empty = Right []
readParamList' sym (CParameterList' decl list) = do
  t <- readParamDeclaration sym $ parseItem decl
  listTail <- readParamList' sym $ parseItem list
  return $ t : listTail

readParamList ::
     SymbolTable
  -> CParameterList
  -> Either TypeError [((Maybe String), CType)]
readParamList sym (CParameterList decl list) = do
  t <- readParamDeclaration sym $ parseItem decl
  listTail <- readParamList' sym $ parseItem list
  return $ t : listTail

-- (varargs?, [(param name, param type)])
readParamTypes ::
     SymbolTable
  -> CParameterTypeList
  -> Either TypeError (Bool, [((Maybe String), CType)])
readParamTypes sym (CParameterTypeList list varargs) = do
  paramList <- readParamList sym $ parseItem list
  return (parseItem varargs == CVarArgsOptionalEmpty, paramList)

-- returns (function name, function type, param names)
readFunctionDeclaration ::
     Coordinates
  -> SymbolTable
  -> CDeclarator
  -> CDeclarationListOptional
  -> CType
  -> Either TypeError (String, CType, [String])
-- new style
readFunctionDeclaration c sym (CDeclarator pointer directDeclarator) CDeclarationListOptionalEmpty t =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'ParamTypeList typeList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              (varargs, params) <- readParamTypes sym $ parseItem typeList
              let functionType =
                    if varargs
                      then CType
                             { storageClass = []
                             , typeQualifier = []
                             , dataType =
                                 TVarArgFunction returnType $ map snd params
                             }
                      else CType
                             { storageClass = []
                             , typeQualifier = []
                             , dataType = TFunction returnType $ map snd params
                             }
              case traverse fst params of
                Nothing -> Left $ TypeError c "Unnamed function parameter(s)"
                Just paramNames ->
                  if (length $ S.fromList paramNames) == length paramNames
                    then return (fName, functionType, paramNames)
                    else Left $ TypeError c "Conflicting parameter names"
            _ -> Left $ TypeError c "Invalid function declaration"
        _ -> Left $ TypeError c "Invalid function declaration"
  where
    returnType = readPointerOptional t $ parseItem pointer
-- old style
readFunctionDeclaration c sym (CDeclarator pointer directDeclarator) (CDeclarationListOptional declList) t =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'IdList idList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              params <- readDeclarationList sym $ parseItem declList
              let paramNames =
                    case parseItem idList of
                      CIdentifierListOptionalEmpty -> []
                      CIdentifierListOptional idList' ->
                        readIdentifierList $ parseItem idList'
              let paramTypeLookup =
                    M.fromList $ map (\(_, k, v) -> (k, v)) params
              let paramTypes = map (\k -> M.lookup k paramTypeLookup) paramNames
              case sequenceA paramTypes of
                Nothing ->
                  Left $ TypeError c "Internal error parsing function parameter types" -- this shouldn't happen
                Just paramTypes' ->
                  if (S.fromList paramNames == (S.fromList $ map (\(_, k, _) -> k) params)) &&
                     ((length $ S.fromList paramNames) == length paramNames) &&
                     all (\(initialized, _, _) -> not initialized) params
                    then return
                           ( fName
                           , CType
                               { storageClass = []
                               , typeQualifier = []
                               , dataType = TFunction returnType paramTypes'
                               }
                           , paramNames)
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
      Left $ TypeError (parseLoc specifiersOpt) "Function return type not specified"
    CDeclarationSpecifiersOptional specifiers -> do
      returnType <-
        readDeclarationSpecifiers (parseLoc specifiers) sym (parseItem specifiers)
      readFunctionDeclaration
        (parseLoc declarator)
        sym
        (parseItem declarator)
        (parseItem declarationList)
        returnType

------------------
-- DECLARATIONS --
------------------

-- (assigned?, identifier, type)
readInitDeclarator ::
     SymbolTable
  -> CType
  -> CInitDeclarator
  -> Either TypeError (Bool, String, CType)
readInitDeclarator sym t (CInitDeclarator decl initOpt) = do
  (identifier, t') <- readDeclarator sym t $ parseItem decl
  return (parseItem initOpt == CAssignInitializerOptionalEmpty, identifier, t')

readInitDeclaratorList' ::
     SymbolTable
  -> CType
  -> CInitDeclaratorList'
  -> Either TypeError [(Bool, String, CType)]
readInitDeclaratorList' _ t CInitDeclaratorList'Empty = Right []
readInitDeclaratorList' sym t (CInitDeclaratorList' decl list) = do
  d <- readInitDeclarator sym t $ parseItem decl
  listTail <- readInitDeclaratorList' sym t $ parseItem list
  return $ d : listTail

readInitDeclaratorList ::
     SymbolTable
  -> CType
  -> CInitDeclaratorList
  -> Either TypeError [(Bool, String, CType)]
readInitDeclaratorList sym t (CInitDeclaratorList decl list) = do
  d <- readInitDeclarator sym t $ parseItem decl
  listTail <- readInitDeclaratorList' sym t $ parseItem list
  return $ d : listTail

readDeclaration ::
     SymbolTable -> CDeclaration -> Either TypeError [(Bool, String, CType)]
readDeclaration sym (CDeclaration spec initDeclarators) =
  case parseItem initDeclarators of
    CInitDeclaratorListOptionalEmpty ->
      Left $ TypeError (parseLoc initDeclarators) "No identifier specified"
    CInitDeclaratorListOptional list -> do
      t <- readDeclarationSpecifiers (parseLoc initDeclarators) sym $ parseItem spec
      readInitDeclaratorList sym t $ parseItem list

readDeclarationList ::
     SymbolTable
  -> CDeclarationList
  -> Either TypeError [(Bool, String, CType)]
readDeclarationList sym (CDeclarationList decl listOpt) =
  case parseItem listOpt of
    CDeclarationListOptionalEmpty -> readDeclaration sym $ parseItem decl
    CDeclarationListOptional list -> do
      listHead <- readDeclaration sym $ parseItem decl
      listTail <- readDeclarationList sym $ parseItem list
      return $ listHead ++ listTail

-----------------
-- DECLARATORS --
-----------------

readDirectDeclarator' ::
     SymbolTable -> CType -> CDirectDeclarator' -> Either TypeError CType
readDirectDeclarator' _ t CDirectDeclarator'Empty = Right t
readDirectDeclarator' sym t (CDirectDeclarator'ConstExpr _ directDecl') =
  readDirectDeclarator' sym t' $ parseItem directDecl'
  where
    t' = CType { storageClass = []
               , typeQualifier = []
               , dataType = TArray t Nothing
               }
readDirectDeclarator' sym t (CDirectDeclarator'ParamTypeList typeList directDecl') = do
  (varArgs, types) <- readParamTypeList sym $ parseItem typeList
  let t' = CType { storageClass = []
                 , typeQualifier = []
                 , dataType = if varArgs
                                then TVarArgFunction t types
                                else TFunction t types
                 }
  readDirectDeclarator' sym t' $ parseItem directDecl'
readDirectDeclarator' sym t (CDirectDeclarator'IdList idList directDecl') =
  case parseItem idList of
    CIdentifierListOptionalEmpty -> readDirectDeclarator' sym t' $ parseItem directDecl'
      where t' = CType { storageClass = []
                       , typeQualifier = []
                       , dataType = TVarArgFunction t []
                       }
    _ -> Left $ TypeError (parseLoc idList) "No parameter types given in function declaration"

readDirectDeclarator ::
     SymbolTable
  -> CType
  -> CDirectDeclarator
  -> Either TypeError (String, CType)
readDirectDeclarator sym t (CDirectDeclaratorId (ParseItem _ _ (CIdentifier identifier) _) directDecl') = do
  t' <- readDirectDeclarator' sym t $ parseItem directDecl'
  return (identifier, t')
readDirectDeclarator sym t (CDirectDeclaratorParen decl directDecl') = do
  (identifier, t') <- readDeclarator sym t $ parseItem decl
  t'' <- readDirectDeclarator' sym t' $ parseItem directDecl'
  return (identifier, t'')

readDeclarator ::
     SymbolTable
  -> CType
  -> CDeclarator
  -> Either TypeError (String, CType)
readDeclarator sym t (CDeclarator pointer directDecl) =
  readDirectDeclarator sym t' $ parseItem directDecl
  where
    t' = readPointerOptional t $ parseItem pointer

readDirectAbstractDeclarator' ::
     CType -> CDirectAbstractDeclarator' -> Either TypeError CType
readDirectAbstractDeclarator' t CDirectAbstractDeclarator'Empty = Right t
readDirectAbstractDeclarator' t (CDirectAbstractDeclarator'Const _ decl) =
  readDirectAbstractDeclarator' t' $ parseItem decl
  where t' = CType { storageClass = []
                   , typeQualifier = []
                   , dataType = TArray t Nothing
                   }
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator' t (CDirectAbstractDeclarator'Params item _) =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readDirectAbstractDeclarator ::
     CType -> CDirectAbstractDeclarator -> Either TypeError CType
readDirectAbstractDeclarator t (CDirectAbstractDeclaratorIndexed _ decl) =
  readDirectAbstractDeclarator' t' $ parseItem decl
  where t' = CType { storageClass = []
                   , typeQualifier = []
                   , dataType = TArray t Nothing
                   }
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator _ (CDirectAbstractDeclaratorParen item _) =
  Left $ TypeError (parseLoc item) "Parenthesized type"
-- Same here
readDirectAbstractDeclarator _ (CDirectAbstractDeclaratorParams item _) =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readAbstractDeclarator :: CType -> CAbstractDeclarator -> Either TypeError CType
readAbstractDeclarator t (CAbstractDeclaratorPointer pointer) =
  Right $ readPointer t $ parseItem pointer
readAbstractDeclarator t (CAbstractDeclaratorDirect pointerOpt directDecl) =
  readDirectAbstractDeclarator t' $ parseItem directDecl
  where
    t' = readPointerOptional t $ parseItem pointerOpt

-------------
-- HELPERS --
-------------

readPointer :: CType -> CPointer -> CType
readPointer t (CPointer qualifierList pointerOpt) =
  readPointerOptional t' $ parseItem pointerOpt
  where
    qualifiers = readTypeQualifiers $ parseItem qualifierList
    t' =
      CType
        {storageClass = [], typeQualifier = qualifiers, dataType = TPointer t}

readPointerOptional :: CType -> CPointerOptional -> CType
readPointerOptional t CPointerOptionalEmpty = t
readPointerOptional t (CPointerOptional pointer) =
  readPointer t $ parseItem pointer

readIdentifierList' :: CIdentifierList' -> [String]
readIdentifierList' CIdentifierList'Empty = []
readIdentifierList' (CIdentifierList' (ParseItem _ _ (CIdentifier identifier) _) idList') =
  identifier : (readIdentifierList' $ parseItem idList')

readIdentifierList :: CIdentifierList -> [String]
readIdentifierList (CIdentifierList (ParseItem _ _ (CIdentifier identifier) _) idList') =
  identifier : (readIdentifierList' $ parseItem idList')

