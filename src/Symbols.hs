module Symbols where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

import ParseItem
import Scanner (Coordinates)

addSymbols :: ParseItem a -> SymbolTable -> ParseItem a
addSymbols (ParseItem l s i _) = ParseItem l s i

data TypeError =
  TypeError
    { errorLoc :: Coordinates
    , errorMsg :: String
    }

data TypeCheckItem a =
  TypeCheckItem
    { typeCheckSymbols   :: SymbolTable  -- for checking for existence of typedef / union / struct
    , previousType  :: CType        -- for passing partially constructed type
    , typeCheckItem :: a            -- an item in the parse tree
    , typeCheckLoc  :: Coordinates  -- coordinates for possible error message
    }

type TypeReader a b = TypeCheckItem a -> Either TypeError b

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
     TypeReader [CStorageClassSpecifier] [CStorageClassSpecifier]
validateStorageClasses item
  | length (typeCheckItem item) > 1 =
      Left $ TypeError (typeCheckLoc item) "Multiple storage classes"
  | otherwise = Right $ typeCheckItem item

validateTypeSpecifiers :: TypeReader [CTypeSpecifier] CDataType
validateTypeSpecifiers item@(TypeCheckItem { typeCheckLoc = c, typeCheckItem = spec })
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
      CTypeSpecifierEnum enumSpec ->
        readEnumSpecifier $ item { typeCheckItem = parseItem $ enumSpec }
      CTypeSpecifierStructOrUnion structOrUnionSpec ->
        readStructOrUnionSpecifier $ item { typeCheckItem = parseItem structOrUnionSpec }
      CTypeSpecifierTypedef t ->
        readTypedefName $ item { typeCheckItem = parseItem t }
      _ -> Left $ TypeError c "Internal error reading type"  -- this shoudn't happen
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

validateTypeQualifiers :: TypeReader [CTypeQualifier] [CTypeQualifier]
validateTypeQualifiers (TypeCheckItem { typeCheckLoc = c, typeCheckItem = spec })
  | length spec > 2 = Left $ TypeError c "Repeated type qualifiers"
  | spec == [CTypeQualifierConst, CTypeQualifierConst] =
    Left $ TypeError c "Repeated type qualifiers"
  | spec == [CTypeQualifierVolatile, CTypeQualifierVolatile] =
    Left $ TypeError c "Repeated type qualifiers"
  | otherwise = Right spec

validateFunctionStorageClasses ::
     TypeReader [CStorageClassSpecifier] [CStorageClassSpecifier]
validateFunctionStorageClasses (TypeCheckItem { typeCheckLoc = c, typeCheckItem = spec }) =
  if spec `elem`
     [[], [CStorageClassSpecifierExtern], [CStorageClassSpecifierStatic]]
    then Right spec
    else Left $
         TypeError c "Illegal storage class specifier in function definition"

readTypeQualifiers :: TypeReader CTypeQualifierListOptional [CTypeQualifier]
readTypeQualifiers item =
  case typeCheckItem item of
    CTypeQualifierListOptionalEmpty -> Right []
    CTypeQualifierListOptional (ParseItem _ _ (CTypeQualifierList qualifier list) _) -> do
      listTail <- readTypeQualifiers $ item { typeCheckItem = parseItem list }
      return $ (parseItem qualifier) : listTail

readSpecifierQualifierList ::
     TypeReader CSpecifierQualifierList ([CTypeSpecifier], [CTypeQualifier])
readSpecifierQualifierList item =
  case typeCheckItem item of
    (CSpecifierQualifierListSpecifier spec list) ->
      case parseItem list of
        CSpecifierQualifierListOptional list' -> do
          (specifiers, qualifiers) <- readSpecifierQualifierList $ item { typeCheckItem = parseItem list' }
          return (parseItem spec : specifiers, qualifiers)
        CSpecifierQualifierListOptionalEmpty -> return ([parseItem spec], [])
    (CSpecifierQualifierListQualifier qualifier list) ->
      case parseItem list of
        CSpecifierQualifierListOptional list' -> do
          (specifiers, qualifiers) <- readSpecifierQualifierList $ item { typeCheckItem = parseItem list' }
          return (specifiers, parseItem qualifier : qualifiers)
        CSpecifierQualifierListOptionalEmpty -> return ([], [parseItem qualifier])

readDeclarationSpecifiers :: TypeReader CDeclarationSpecifiers CType
readDeclarationSpecifiers item = do
  (storageClasses, typeQualifiers, typeSpecifiers) <- readDeclarationSpecifierList item
  storageClasses' <- validateStorageClasses $ item { typeCheckItem = storageClasses }
  typeQualifiers' <- validateTypeQualifiers $ item { typeCheckItem = typeQualifiers }
  typeSpecifier' <- validateTypeSpecifiers $ item { typeCheckItem = typeSpecifiers }
  return
    CType
      { storageClass = storageClasses'
      , typeQualifier = typeQualifiers'
      , dataType = typeSpecifier'
      }

readDeclarationSpecifierList ::
  TypeReader
    CDeclarationSpecifiers
    ([CStorageClassSpecifier], [CTypeQualifier], [CTypeSpecifier])
readDeclarationSpecifierList
     item@(TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersStorageClass spec list }) =
  case parseItem list of
    CDeclarationSpecifiersOptionalEmpty ->
      return ([parseItem spec], [], [])
    (CDeclarationSpecifiersOptional spec') -> do
      (storageCls, qualifiers, specifiers) <- readDeclarationSpecifierList $ item { typeCheckItem =  parseItem spec' }
      return (parseItem spec : storageCls, qualifiers, specifiers)
readDeclarationSpecifierList
     item@(TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersTypeQualifier spec list }) =
  case parseItem list of
    CDeclarationSpecifiersOptionalEmpty ->
      return ([], [parseItem spec], [])
    (CDeclarationSpecifiersOptional spec') -> do
      (storageCls, qualifiers, specifiers) <- readDeclarationSpecifierList $ item { typeCheckItem = parseItem spec' }
      return (storageCls, parseItem spec : qualifiers, specifiers)
readDeclarationSpecifierList
     item@(TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersTypeSpecifier spec list }) =
  case parseItem list of
    CDeclarationSpecifiersOptionalEmpty ->
      return ([], [], [parseItem spec])
    (CDeclarationSpecifiersOptional spec') -> do
      (storageCls, qualifiers, specifiers) <- readDeclarationSpecifierList $ item { typeCheckItem = parseItem spec' }
      return (storageCls, qualifiers, parseItem spec : specifiers)

----------------------
-- STRUCTURED TYPES --
----------------------

readEnumerator :: TypeReader CEnumerator (String, Int)
readEnumerator (TypeCheckItem { typeCheckItem = CEnumerator identifier }) =
  -- TODO: handle integer value properly
  return (i, 1)
  where (CIdentifier i) = parseItem identifier
readEnumerator (TypeCheckItem { typeCheckItem = CEnumeratorAssign identifier _ }) =
  -- TODO: handle integer value properly
  return (i, 1)
  where (CIdentifier i) = parseItem identifier


readEnumeratorList' :: TypeReader CEnumeratorList' [(String, Int)]
readEnumeratorList' (TypeCheckItem { typeCheckItem = CEnumeratorList'Empty }) =
  Right []
readEnumeratorList'
     item@(TypeCheckItem { typeCheckItem = CEnumeratorList' enum list}) = do
  listHead <- readEnumerator $ item { typeCheckItem = parseItem enum }
  listTail <- readEnumeratorList' $ item { typeCheckItem = parseItem list }
  return $ listHead : listTail

readEnumeratorList :: TypeReader CEnumeratorList (M.Map String Int)
readEnumeratorList
     item@(TypeCheckItem { typeCheckItem = CEnumeratorList enum list }) = do
  listHead <- readEnumerator $ item { typeCheckItem = parseItem enum }
  listTail <- readEnumeratorList' $ item { typeCheckItem = parseItem list }
  return $ M.fromList $ listHead : listTail

readEnumSpecifier :: TypeReader CEnumSpecifier CDataType
readEnumSpecifier
     (TypeCheckItem
       { typeCheckLoc = c
       , typeCheckSymbols = sym
       , typeCheckItem =
          CEnumSpecifier (ParseItem {parseItem = CIdentifier identifier })
       }) =
  case lookupSymbols identifier sym of
    Just t -> Right $ dataType t
    Nothing -> Left $ TypeError c $ "Undefined enum '" ++ identifier ++ "'."
readEnumSpecifier
     item@(TypeCheckItem
            { typeCheckItem =
               CEnumSpecifierList idOpt enumList
            }) = do
  let identifier = case parseItem idOpt of
                     CIdentifierOptionalEmpty -> Nothing
                     CIdentifierOptional identifier' ->
                       Just i
                       where CIdentifier i = parseItem identifier'
  enumList' <- readEnumeratorList $ item { typeCheckItem = parseItem enumList }
  return $ TEnum identifier enumList'

readStructOrUnionSpecifier ::
     TypeReader CStructOrUnionSpecifier CDataType
readStructOrUnionSpecifier
     (TypeCheckItem
       { typeCheckLoc = c
       , typeCheckSymbols = sym
       , typeCheckItem =
           CStructOrUnionSpecifier
             structOrUnion
             (ParseItem { parseItem = CIdentifier identifier })
       }) =
  case lookupSymbols identifier sym of
    Nothing ->
      Left $ TypeError c $ "Undefined struct or union '" ++ identifier ++ "'."
    Just t ->
      case dataType t of
        TUnion _ _ ->
          if parseItem structOrUnion == CStructOrUnionUnion
            then Right $ dataType t
            else Left $
                 TypeError c $ "conflicting type name: '" ++ identifier ++ "'."
        TStruct _ _ ->
          if parseItem structOrUnion == CStructOrUnionStruct
            then Right $ dataType t
            else Left $
                 TypeError c $ "conflicting type name: '" ++ identifier ++ "'."
        _ ->
          Left $ TypeError c "Internal error: expected struct or union"
readStructOrUnionSpecifier
     item@(TypeCheckItem
            { typeCheckLoc = c
            , typeCheckItem =
                CStructOrUnionSpecifierList structOrUnion identifierOpt decl
            }) = do
  declarations <- readStructDeclarationList $ item { typeCheckItem = parseItem decl }
  let identifiers = filter (/= Nothing) $ map (\(_, i, _) -> i) declarations
  let name =
        case parseItem identifierOpt of
          CIdentifierOptionalEmpty -> Nothing
          CIdentifierOptional (ParseItem _ _ (CIdentifier i) _) -> Just i
  if (length $ S.fromList identifiers) /= length identifiers
    then Left $ TypeError c "Conflicting identifiers"
    else case parseItem structOrUnion of
           CStructOrUnionStruct -> Right $ TStruct name declarations
           CStructOrUnionUnion ->
             case traverse (\(_, identifier, _) -> identifier) declarations of
               Nothing -> Left $ TypeError c "Union identifier not specified"
               Just identifiers' ->
                 let unionTypes = M.fromList $ zip identifiers' $ map (\(t, _, _) -> t) declarations
                  in Right $ TUnion name unionTypes

readStructDeclarator ::
     TypeReader CStructDeclarator (CType, Maybe String, Maybe Int)
readStructDeclarator
     item@(TypeCheckItem { typeCheckItem = (CStructDeclarator decl) }) = do
  (identifier, t) <- readDeclarator $ item { typeCheckItem = parseItem decl }
  return (t, Just identifier, Nothing)
readStructDeclarator
     item@(TypeCheckItem { typeCheckItem = (CStructDeclaratorField declOpt _) }) =
  case parseItem declOpt of
    CDeclaratorOptionalEmpty ->
      -- TODO: handle bit fields properly
      Right (previousType item, Nothing, Nothing)
    CDeclaratorOptional decl -> do
      (identifier, t) <- readDeclarator $ item { typeCheckItem = parseItem decl }
      return (t, Just identifier, Nothing)

readStructDeclaratorList' ::
     TypeReader CStructDeclaratorList' [(CType, Maybe String, Maybe Int)]
readStructDeclaratorList' (TypeCheckItem { typeCheckItem = CStructDeclaratorList'Empty }) =
  Right []
readStructDeclaratorList'
     item@(TypeCheckItem
            { typeCheckItem = (CStructDeclaratorList' decl list) }) = do
  d <- readStructDeclarator $ item { typeCheckItem = parseItem decl }
  listTail <- readStructDeclaratorList' $ item { typeCheckItem = parseItem list }
  return $ d : listTail

readStructDeclaratorList ::
     TypeReader CStructDeclaratorList [(CType, Maybe String, Maybe Int)]
readStructDeclaratorList
     item@(TypeCheckItem
            { typeCheckItem = (CStructDeclaratorList decl list) }) = do
  d <- readStructDeclarator $ item { typeCheckItem = parseItem decl }
  listTail <- readStructDeclaratorList' $ item { typeCheckItem = parseItem list }
  return $ d : listTail

readStructDeclaration ::
     TypeReader CStructDeclaration [(CType, Maybe String, Maybe Int)]
readStructDeclaration
     item@(TypeCheckItem
            { typeCheckItem = CStructDeclaration specList declList }) = do
  (typeSpecifiers, typeQualifiers) <-
    readSpecifierQualifierList $ item { typeCheckItem = parseItem specList }
  typeQualifiers' <-
    validateTypeQualifiers $ item { typeCheckItem = typeQualifiers }
  typeSpecifier' <-
    validateTypeSpecifiers $ item { typeCheckItem = typeSpecifiers }
  let t =
        CType
          { storageClass = []
          , typeQualifier = typeQualifiers'
          , dataType = typeSpecifier'
          }
  readStructDeclaratorList $ item { previousType = t
                                  , typeCheckItem = parseItem declList }

readStructDeclarationList ::
     TypeReader CStructDeclarationList [(CType, Maybe String, Maybe Int)]
readStructDeclarationList
     item@(TypeCheckItem
            { typeCheckItem = CStructDeclarationList decl list }) =
  case parseItem list of
    CStructDeclarationListOptional list' -> do
      decl' <- readStructDeclaration $ item { typeCheckItem = parseItem decl }
      declarations <- readStructDeclarationList $ item { typeCheckItem = parseItem list' }
      return $ decl' ++ declarations
    CStructDeclarationListOptionalEmpty ->
      readStructDeclaration $ item { typeCheckItem = parseItem decl }

readTypedefName :: TypeReader CTypedefName CDataType
readTypedefName
     (TypeCheckItem
       { typeCheckLoc = c
       , typeCheckSymbols = sym
       , typeCheckItem =
           CTypedefName (ParseItem { parseItem = CIdentifier identifier } )
       }) =
  case M.lookup identifier $ typedef sym of
    Just (_, t) -> Right $ dataType t
    Nothing -> Left $ TypeError c $ "Unknown typedef name '" ++ identifier ++ "'."

---------------
-- FUNCTIONS --
---------------

functionReturnType :: TypeReader CDeclarationSpecifiersOptional CType
functionReturnType (TypeCheckItem { typeCheckItem = CDeclarationSpecifiersOptionalEmpty }) =
  return CType {storageClass = [], typeQualifier = [], dataType = TShort}
functionReturnType
     item@(TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersOptional spec }) = do
  t <- readDeclarationSpecifiers $ item { typeCheckItem = parseItem spec }
  _ <- validateFunctionStorageClasses $ item { typeCheckItem = storageClass t }
  return t

-- (varargs? [argument types])
readParamTypeList :: TypeReader CParameterTypeList (Bool, [CType])
readParamTypeList
     item@(TypeCheckItem
            { typeCheckItem = CParameterTypeList paramList varargs }) = do
  params <- readParamList $ item { typeCheckItem = parseItem paramList }
  let varargs' = parseItem varargs == CVarArgsOptionalEmpty
  return (varargs', map snd params)

readParamDeclaration :: TypeReader CParameterDeclaration ((Maybe String), CType)
readParamDeclaration
     item@(TypeCheckItem
            { typeCheckItem = CParameterDeclaration specifiers paramDecl
            }) =
  case parseItem paramDecl of
    CParameterDeclaration' decl -> do
      t <- readDeclarationSpecifiers $ item { typeCheckLoc = parseLoc specifiers
                                            , typeCheckItem = parseItem specifiers
                                            }
      (identifier, t') <- readDeclarator $ item { previousType = t
                                                , typeCheckItem = parseItem decl
                                                }
      return (Just identifier, t')
    CParameterDeclaration'Abstract abstractDeclOpt ->
      case parseItem abstractDeclOpt of
        CAbstractDeclaratorOptionalEmpty -> do
          t <- readDeclarationSpecifiers $ item { typeCheckLoc = parseLoc specifiers
                                                , typeCheckItem = parseItem specifiers
                                                }
          return (Nothing, t)
        CAbstractDeclaratorOptional abstractDecl -> do
          t <- readDeclarationSpecifiers $ item { typeCheckLoc = parseLoc specifiers
                                                , typeCheckItem = parseItem specifiers
                                                }
          t' <- readAbstractDeclarator $ item { previousType = t
                                              , typeCheckItem = parseItem abstractDecl
                                              }
          return (Nothing, t')

readParamList' :: TypeReader CParameterList' [((Maybe String), CType)]
readParamList' (TypeCheckItem { typeCheckItem = CParameterList'Empty }) =
  Right []
readParamList'
     item@(TypeCheckItem { typeCheckItem = CParameterList' decl list }) = do
  t <- readParamDeclaration $ item { typeCheckItem = parseItem decl }
  listTail <- readParamList' $ item { typeCheckItem = parseItem list }
  return $ t : listTail

readParamList :: TypeReader CParameterList [((Maybe String), CType)]
readParamList
     item@(TypeCheckItem { typeCheckItem = CParameterList decl list }) = do
  t <- readParamDeclaration $ item { typeCheckItem = parseItem decl }
  listTail <- readParamList' $ item { typeCheckItem = parseItem list }
  return $ t : listTail

-- (varargs?, [(param name, param type)])
readParamTypes :: TypeReader CParameterTypeList (Bool, [((Maybe String), CType)])
readParamTypes
     item@(TypeCheckItem { typeCheckItem = CParameterTypeList list varargs }) = do
  paramList <- readParamList $ item { typeCheckItem = parseItem list }
  return (parseItem varargs == CVarArgsOptionalEmpty, paramList)

-- returns (function name, function type, param names)
readFunctionDeclaration ::
     TypeReader
       CFunctionDefinition
       (String, CType, [String])
-- new style
readFunctionDeclaration
     item@(TypeCheckItem
            { typeCheckLoc = c
            , typeCheckItem =
                CFunctionDefinition
                  _
                  (ParseItem { parseItem = CDeclarator pointer directDeclarator })
                  (ParseItem { parseItem = CDeclarationListOptionalEmpty })
                  _
            }) =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'ParamTypeList typeList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              returnType <- readPointerOptional $ item { typeCheckItem = parseItem pointer }
              (varargs, params) <- readParamTypes $ item { typeCheckItem = parseItem typeList }
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
-- old style
readFunctionDeclaration
     item@(TypeCheckItem
            { typeCheckLoc = c
            , typeCheckItem =
                CFunctionDefinition
                  _
                  (ParseItem { parseItem = (CDeclarator pointer directDeclarator) })
                  (ParseItem { parseItem = (CDeclarationListOptional declList) })
                  _
            }) =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId (ParseItem _ _ (CIdentifier fName) _) decl ->
      case parseItem decl of
        (CDirectDeclarator'IdList idList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              params <- readDeclarationList $ item { typeCheckItem = parseItem declList }
              returnType <- readPointerOptional $ item { typeCheckItem = parseItem pointer }
              paramNames <- case parseItem idList of
                              CIdentifierListOptionalEmpty -> return []
                              CIdentifierListOptional idList' ->
                                readIdentifierList $ item { typeCheckItem = parseItem idList' }
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

readFunctionDefinition :: TypeReader CFunctionDefinition (String, CType, [String])
readFunctionDefinition
     item@(TypeCheckItem
            { typeCheckItem =
                CFunctionDefinition specifiersOpt _ _ _
            }) =
  case parseItem specifiersOpt of
    CDeclarationSpecifiersOptionalEmpty ->
      Left $ TypeError (parseLoc specifiersOpt) "Function return type not specified"
    CDeclarationSpecifiersOptional specifiers -> do
      returnType <-
        readDeclarationSpecifiers $ item { typeCheckLoc = parseLoc specifiers
                                         , typeCheckItem = parseItem specifiers
                                         }
      readFunctionDeclaration $ item { previousType = returnType }

------------------
-- DECLARATIONS --
------------------

-- (assigned?, identifier, type)
readInitDeclarator :: TypeReader CInitDeclarator (Bool, String, CType)
readInitDeclarator
     item@(TypeCheckItem { typeCheckItem = CInitDeclarator decl initOpt }) = do
  (identifier, t') <- readDeclarator $ item { typeCheckItem =  parseItem decl }
  return (parseItem initOpt == CAssignInitializerOptionalEmpty, identifier, t')

readInitDeclaratorList' :: TypeReader CInitDeclaratorList' [(Bool, String, CType)]
readInitDeclaratorList' (TypeCheckItem { typeCheckItem = CInitDeclaratorList'Empty }) =
  Right []
readInitDeclaratorList'
     item@(TypeCheckItem { typeCheckItem = CInitDeclaratorList' decl list }) = do
  d <- readInitDeclarator $ item { typeCheckItem = parseItem decl }
  listTail <- readInitDeclaratorList' $ item { typeCheckItem = parseItem list }
  return $ d : listTail

readInitDeclaratorList :: TypeReader CInitDeclaratorList [(Bool, String, CType)]
readInitDeclaratorList
     item@(TypeCheckItem { typeCheckItem = (CInitDeclaratorList decl list) }) = do
  d <- readInitDeclarator $ item { typeCheckItem = parseItem decl }
  listTail <- readInitDeclaratorList' $ item { typeCheckItem = parseItem list }
  return $ d : listTail

readDeclaration :: TypeReader CDeclaration [(Bool, String, CType)]
readDeclaration
     item@(TypeCheckItem { typeCheckItem = CDeclaration spec initDeclarators }) =
  case parseItem initDeclarators of
    CInitDeclaratorListOptionalEmpty ->
      Left $ TypeError (parseLoc initDeclarators) "No identifier specified"
    CInitDeclaratorListOptional list -> do
      t <- readDeclarationSpecifiers $ item { typeCheckLoc = parseLoc initDeclarators
                                            , typeCheckItem = parseItem spec
                                            }
      readInitDeclaratorList $ item { previousType = t
                                    , typeCheckItem =  parseItem list
                                    }

readDeclarationList :: TypeReader CDeclarationList [(Bool, String, CType)]
readDeclarationList
     item@(TypeCheckItem { typeCheckItem = CDeclarationList decl listOpt }) =
  case parseItem listOpt of
    CDeclarationListOptionalEmpty ->
      readDeclaration $ item { typeCheckItem = parseItem decl }
    CDeclarationListOptional list -> do
      listHead <- readDeclaration $ item { typeCheckItem = parseItem decl }
      listTail <- readDeclarationList $ item { typeCheckItem = parseItem list }
      return $ listHead ++ listTail

-----------------
-- DECLARATORS --
-----------------

readDirectDeclarator' :: TypeReader CDirectDeclarator' CType
readDirectDeclarator' item@(TypeCheckItem { typeCheckItem = CDirectDeclarator'Empty }) =
  Right $ previousType item
readDirectDeclarator'
     item@(TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectDeclarator'ConstExpr _ directDecl'
            }) =
  readDirectDeclarator' $ item { previousType = t'
                               , typeCheckItem =  parseItem directDecl'
                               }
  where
    t' = CType { storageClass = []
               , typeQualifier = []
               , dataType = TArray t Nothing
               }
readDirectDeclarator'
     item@(TypeCheckItem
            { previousType = t
            , typeCheckItem =
                CDirectDeclarator'ParamTypeList typeList directDecl'
            }) = do
  (varArgs, types) <- readParamTypeList $ item { typeCheckItem = parseItem typeList }
  let t' = CType { storageClass = []
                 , typeQualifier = []
                 , dataType = if varArgs
                                then TVarArgFunction t types
                                else TFunction t types
                 }
  readDirectDeclarator' $ item { previousType = t'
                               , typeCheckItem = parseItem directDecl'
                               }
readDirectDeclarator'
     item@(TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectDeclarator'IdList idList directDecl'
            }) =
  case parseItem idList of
    CIdentifierListOptionalEmpty ->
      readDirectDeclarator' $ item { previousType = t'
                                   , typeCheckItem = parseItem directDecl'
                                   }
      where t' = CType { storageClass = []
                       , typeQualifier = []
                       , dataType = TVarArgFunction t []
                       }
    _ -> Left $ TypeError (parseLoc idList) "No parameter types given in function declaration"

readDirectDeclarator :: TypeReader CDirectDeclarator (String, CType)
readDirectDeclarator
     item@(TypeCheckItem
            { typeCheckItem =
               (CDirectDeclaratorId
                 (ParseItem _ _ (CIdentifier identifier) _)
                 directDecl')
            }) = do
  t' <- readDirectDeclarator' $ item { typeCheckItem = parseItem directDecl' }
  return (identifier, t')
readDirectDeclarator
     item@(TypeCheckItem
            { typeCheckItem = CDirectDeclaratorParen decl directDecl'
            }) = do
  (identifier, t') <- readDeclarator $ item { typeCheckItem = parseItem decl }
  t'' <- readDirectDeclarator' $ item { previousType = t'
                                      , typeCheckItem = parseItem directDecl'
                                      }
  return (identifier, t'')

readDeclarator :: TypeReader CDeclarator (String, CType)
readDeclarator
     item@(TypeCheckItem
            { typeCheckItem = CDeclarator pointer directDecl
            }) = do
  t' <- readPointerOptional $ item { typeCheckItem = parseItem pointer }
  readDirectDeclarator $ item { previousType = t', typeCheckItem = parseItem directDecl }

readDirectAbstractDeclarator' :: TypeReader CDirectAbstractDeclarator' CType
readDirectAbstractDeclarator'
     item@(TypeCheckItem
            { typeCheckItem = CDirectAbstractDeclarator'Empty
            }) =
  Right $ previousType item
readDirectAbstractDeclarator'
     item@(TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectAbstractDeclarator'Const _ decl
            }) =
  readDirectAbstractDeclarator' $ item { previousType = t'
                                       , typeCheckItem = parseItem decl
                                       }
  where t' = CType { storageClass = []
                   , typeQualifier = []
                   , dataType = TArray t Nothing
                   }
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator'
     (TypeCheckItem { typeCheckItem = (CDirectAbstractDeclarator'Params item _) }) =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readDirectAbstractDeclarator :: TypeReader CDirectAbstractDeclarator CType
readDirectAbstractDeclarator
     item@(TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectAbstractDeclaratorIndexed _ decl
            }) =
  readDirectAbstractDeclarator' $ item { previousType = t', typeCheckItem = parseItem decl }
  where t' = CType { storageClass = []
                   , typeQualifier = []
                   , dataType = TArray t Nothing
                   }
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator
     (TypeCheckItem { typeCheckItem = (CDirectAbstractDeclaratorParen item _) }) =
  Left $ TypeError (parseLoc item) "Parenthesized type"
-- Same here
readDirectAbstractDeclarator
     (TypeCheckItem { typeCheckItem = (CDirectAbstractDeclaratorParams item _) }) =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readAbstractDeclarator :: TypeReader CAbstractDeclarator CType
readAbstractDeclarator
     item@(TypeCheckItem
            { typeCheckItem = CAbstractDeclaratorPointer pointer
            }) =
  readPointer $ item { typeCheckItem = parseItem pointer }
readAbstractDeclarator
    item@(TypeCheckItem
            { typeCheckItem = CAbstractDeclaratorDirect pointerOpt directDecl
            }) = do
  t' <- readPointerOptional $ item { typeCheckItem = parseItem pointerOpt }
  readDirectAbstractDeclarator $ item { previousType = t', typeCheckItem = parseItem directDecl }

-------------
-- HELPERS --
-------------

readPointer :: TypeReader CPointer CType
readPointer
     item@(TypeCheckItem
            { previousType = t
            , typeCheckItem = CPointer qualifierList pointerOpt
            }) = do
  qualifiers <- readTypeQualifiers $ item { typeCheckItem = parseItem qualifierList }
  let t' = CType {storageClass = [], typeQualifier = qualifiers, dataType = TPointer t}
  readPointerOptional $ item { previousType = t', typeCheckItem = parseItem pointerOpt }

readPointerOptional :: TypeReader CPointerOptional CType
readPointerOptional (TypeCheckItem { previousType = t, typeCheckItem = CPointerOptionalEmpty }) =
  Right t
readPointerOptional item@(TypeCheckItem { typeCheckItem = (CPointerOptional pointer) }) =
  readPointer $ item { typeCheckItem = parseItem pointer }

readIdentifierList' :: TypeReader CIdentifierList' [String]
readIdentifierList' (TypeCheckItem { typeCheckItem = CIdentifierList'Empty }) =
  return []
readIdentifierList'
     item@(TypeCheckItem
            { typeCheckItem =
                (CIdentifierList'
                  (ParseItem { parseItem = CIdentifier identifier})
                  idList')
            }) = do
  listTail <- readIdentifierList' $ item { typeCheckItem = parseItem idList' }
  return $ identifier : listTail

readIdentifierList :: TypeReader CIdentifierList [String]
readIdentifierList
     item@(TypeCheckItem
            { typeCheckItem =
                (CIdentifierList
                  (ParseItem { parseItem = (CIdentifier identifier) })
                  idList')
            }) = do
  listTail <- readIdentifierList' $ item { typeCheckItem = parseItem idList' }
  return $ identifier : listTail

