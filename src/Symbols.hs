{-# LANGUAGE TupleSections #-}

module Symbols where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

import ParseItem
import Utils ( Coordinates
             , Error(..)
             , DataType(..)
             , StorageClass(..)
             , TypeQualifier(..)
             , CType(..)
             , SymbolTable(..)
             )

data TypeCheckItem a =
  TypeCheckItem
    { typeCheckSymbols   :: SymbolTable  -- for checking for existence of typedef / union / struct
    , previousType  :: CType        -- for passing partially constructed type
    , typeCheckItem :: a            -- an item in the parse tree
    , typeCheckLoc  :: Coordinates  -- coordinates for possible error message
    }

type TypeReader a b = TypeCheckItem a -> Either Error b

lookupSymbols :: String -> SymbolTable -> Maybe CType
lookupSymbols s sym =
  case parent sym of
    Just sym' -> v <|> lookupSymbols s sym'
    Nothing -> v
  where
    v = M.lookup s (symbols sym) <|> M.lookup s (typedef sym)

updateTCItem :: TypeCheckItem a -> ParseItem b -> TypeCheckItem b
updateTCItem old item = old { typeCheckItem = parseItem item
                            , typeCheckLoc = parseLoc item
                            }

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

validateStorageClasses ::
     TypeReader [StorageClass] [StorageClass]
validateStorageClasses item
  | length (typeCheckItem item) > 1 =
      Left $ TypeError (typeCheckLoc item) "Multiple storage classes"
  | otherwise = Right $ typeCheckItem item

validateTypeSpecifiers :: TypeReader [CTypeSpecifier] DataType
validateTypeSpecifiers item@TypeCheckItem { typeCheckLoc = c, typeCheckItem = spec }
  | null spec = Left $ TypeError c "Type unspecified"
  | length lengthSpec > 2 = Left $ TypeError c "Too many type size specifiers"
  | length dataTypes > 1 = Left $ TypeError c $ "Too many datatype specifiers:" ++ show dataTypes
  | length signedSpec > 1 =
    Left $ TypeError c "Too many datatype sign specifiers"
  -- long long int
  | lengthSpec == [CTypeSpecifierLong, CTypeSpecifierLong] =
    if null dataTypes || dataTypes == [CTypeSpecifierInt]
      then if signedSpec == [CTypeSpecifierUnsigned]
             then Right TULongLong
             else Right TLongLong
      else Left $ TypeError c "Long long can only be used with integers"
  -- short int
  | lengthSpec == [CTypeSpecifierShort] =
    if null dataTypes || head dataTypes == CTypeSpecifierInt
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
        if null signedSpec
          then Right TLongDouble
          else Left $ TypeError c "Can't specify sign of double"
      _ -> Left $ TypeError c "Long can only be used with integers and doubles"
  -- (un)signed int
  | not (null signedSpec) && null dataTypes =
    case head signedSpec of
      CTypeSpecifierSigned -> Right TShort
      _ -> Right TUShort
  -- bad combination of (un)signed & type
  | not (null signedSpec) &&
      head dataTypes `notElem` [CTypeSpecifierChar, CTypeSpecifierInt] =
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
        readEnumSpecifier $ updateTCItem item enumSpec
      CTypeSpecifierStructOrUnion structOrUnionSpec ->
        readStructOrUnionSpecifier $ updateTCItem item structOrUnionSpec
      CTypeSpecifierTypedef t ->
        readTypedefName $ updateTCItem item t
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

validateTypeQualifiers :: TypeReader [TypeQualifier] [TypeQualifier]
validateTypeQualifiers TypeCheckItem { typeCheckLoc = c, typeCheckItem = spec }
  | length spec > 2 = Left $ TypeError c "Repeated type qualifiers"
  | spec == [TQConst, TQConst] =
    Left $ TypeError c "Repeated type qualifiers"
  | spec == [TQVolatile, TQVolatile] =
    Left $ TypeError c "Repeated type qualifiers"
  | otherwise = Right spec

validateFunctionStorageClasses ::
     TypeReader [StorageClass] [StorageClass]
validateFunctionStorageClasses TypeCheckItem { typeCheckLoc = c, typeCheckItem = spec } =
  if spec `elem` [[], [SCExtern], [SCStatic]]
    then Right spec
    else Left $
         TypeError c "Illegal storage class specifier in function definition"

readTypeQualifiers :: TypeReader CTypeQualifierListOptional [TypeQualifier]
readTypeQualifiers item =
  case typeCheckItem item of
    CTypeQualifierListOptionalEmpty -> Right []
    CTypeQualifierListOptional ParseItem { parseItem = CTypeQualifierList qualifier list } -> do
      listTail <- readTypeQualifiers $ updateTCItem item list
      return $ toQualifier (parseItem qualifier) : listTail

readSpecifierQualifierList ::
     TypeReader CSpecifierQualifierList ([CTypeSpecifier], [TypeQualifier])
readSpecifierQualifierList item =
  case typeCheckItem item of
    (CSpecifierQualifierListSpecifier spec list) ->
      case parseItem list of
        CSpecifierQualifierListOptional list' -> do
          (specifiers, qualifiers) <-
            readSpecifierQualifierList $ updateTCItem item list'
          return (parseItem spec : specifiers, qualifiers)
        CSpecifierQualifierListOptionalEmpty -> return ([parseItem spec], [])
    (CSpecifierQualifierListQualifier qualifier list) ->
      case parseItem list of
        CSpecifierQualifierListOptional list' -> do
          (specifiers, qualifiers) <-
            readSpecifierQualifierList $ updateTCItem item list'
          return (specifiers, toQualifier (parseItem qualifier) : qualifiers)
        CSpecifierQualifierListOptionalEmpty ->
          return ([], [toQualifier $ parseItem qualifier])

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

readDeclarationSpecifiersTypedef :: TypeReader CDeclarationSpecifiers ([String], CType)
readDeclarationSpecifiersTypedef item = do
  (storageClasses, typeQualifiers, typeSpecifiers) <- readDeclarationSpecifierList item
  storageClasses' <-
    validateStorageClasses $
      item { typeCheckItem =
               filter (/= SCTypedef) storageClasses
           }
  typeQualifiers' <- validateTypeQualifiers $ item { typeCheckItem = typeQualifiers }
  typeSpecifier' <-
    validateTypeSpecifiers $
      item { typeCheckItem = filter (not . isTypedefName) typeSpecifiers }
  let typedefLabels = map getTypedefName $ filter isTypedefName typeSpecifiers
  return
    ( typedefLabels
    , CType
        { storageClass = storageClasses'
        , typeQualifier = typeQualifiers'
        , dataType = typeSpecifier'
        }
    )
  where
    isTypedefName x =
      case x of
        CTypeSpecifierTypedef _ -> True
        _ -> False
    getTypedefName (CTypeSpecifierTypedef typedefName) =
      i
      where
        (CTypedefName identifier) = parseItem typedefName
        (CIdentifier i) = parseItem identifier
    getTypedefName _ = error "Internal error"

readDeclarationSpecifierList ::
  TypeReader
    CDeclarationSpecifiers
    ([StorageClass], [TypeQualifier], [CTypeSpecifier])
readDeclarationSpecifierList
     item@TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersStorageClass spec list } =
  case parseItem list of
    CDeclarationSpecifiersOptionalEmpty ->
      return ([toStorageClass $ parseItem spec], [], [])
    (CDeclarationSpecifiersOptional spec') -> do
      (storageCls, qualifiers, specifiers) <-
        readDeclarationSpecifierList $ updateTCItem item spec'
      return (toStorageClass (parseItem spec) : storageCls, qualifiers, specifiers)
readDeclarationSpecifierList
     item@TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersTypeQualifier spec list } =
  case parseItem list of
    CDeclarationSpecifiersOptionalEmpty ->
      return ([], [toQualifier $ parseItem spec], [])
    (CDeclarationSpecifiersOptional spec') -> do
      (storageCls, qualifiers, specifiers) <-
        readDeclarationSpecifierList $ updateTCItem item spec'
      return (storageCls, toQualifier (parseItem spec) : qualifiers, specifiers)
readDeclarationSpecifierList
     item@TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersTypeSpecifier spec list } =
  case parseItem list of
    CDeclarationSpecifiersOptionalEmpty ->
      return ([], [], [parseItem spec])
    (CDeclarationSpecifiersOptional spec') -> do
      (storageCls, qualifiers, specifiers) <-
        readDeclarationSpecifierList $ updateTCItem item spec'
      return (storageCls, qualifiers, parseItem spec : specifiers)

----------------------
-- STRUCTURED TYPES --
----------------------

readEnumerator :: TypeReader CEnumerator (String, Int)
readEnumerator TypeCheckItem { typeCheckItem = CEnumerator identifier } =
  -- TODO: handle integer value properly
  return (i, 1)
  where (CIdentifier i) = parseItem identifier
readEnumerator TypeCheckItem { typeCheckItem = CEnumeratorAssign identifier _ } =
  -- TODO: handle integer value properly
  return (i, 1)
  where (CIdentifier i) = parseItem identifier


readEnumeratorList' :: TypeReader CEnumeratorList' [(String, Int)]
readEnumeratorList' TypeCheckItem { typeCheckItem = CEnumeratorList'Empty } =
  Right []
readEnumeratorList'
     item@TypeCheckItem { typeCheckItem = CEnumeratorList' enum list} = do
  listHead <- readEnumerator $ updateTCItem item enum
  listTail <- readEnumeratorList' $ updateTCItem item list
  return $ listHead : listTail

readEnumeratorList :: TypeReader CEnumeratorList (M.Map String Int)
readEnumeratorList
     item@TypeCheckItem { typeCheckItem = CEnumeratorList enum list } = do
  listHead <- readEnumerator $ updateTCItem item enum
  listTail <- readEnumeratorList' $ updateTCItem item list
  return $ M.fromList $ listHead : listTail

readEnumSpecifier :: TypeReader CEnumSpecifier DataType
readEnumSpecifier
     TypeCheckItem
       { typeCheckLoc = c
       , typeCheckSymbols = sym
       , typeCheckItem =
          CEnumSpecifier ParseItem {parseItem = CIdentifier identifier }
       } =
  case lookupSymbols identifier sym of
    Just t -> Right $ dataType t
    Nothing -> Left $ TypeError c $ "Undefined enum '" ++ identifier ++ "'."
readEnumSpecifier
     item@TypeCheckItem
            { typeCheckItem =
               CEnumSpecifierList idOpt enumList
            } = do
  let identifier = case parseItem idOpt of
                     CIdentifierOptionalEmpty -> Nothing
                     CIdentifierOptional identifier' ->
                       Just i
                       where CIdentifier i = parseItem identifier'
  enumList' <-
    readEnumeratorList $ updateTCItem item enumList
  return $ TEnum identifier enumList'

readStructOrUnionSpecifier ::
     TypeReader CStructOrUnionSpecifier DataType
readStructOrUnionSpecifier
     TypeCheckItem
       { typeCheckLoc = c
       , typeCheckSymbols = sym
       , typeCheckItem =
           CStructOrUnionSpecifier
             structOrUnion
             ParseItem { parseItem = CIdentifier identifier }
       } =
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
     item@TypeCheckItem
            { typeCheckLoc = c
            , typeCheckItem =
                CStructOrUnionSpecifierList structOrUnion identifierOpt decl
            } = do
  declarations <-
    readStructDeclarationList $ updateTCItem item decl
  let identifiers = filter (/= Nothing) $ map (\(_, i, _) -> i) declarations
  let name =
        case parseItem identifierOpt of
          CIdentifierOptionalEmpty -> Nothing
          CIdentifierOptional ParseItem { parseItem = CIdentifier i } -> Just i
  if length (S.fromList identifiers) /= length identifiers
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
     item@TypeCheckItem { typeCheckItem = (CStructDeclarator decl) } = do
  (identifier, t) <- readDeclarator $ updateTCItem item decl
  return (t, Just identifier, Nothing)
readStructDeclarator
     item@TypeCheckItem { typeCheckItem = (CStructDeclaratorField declOpt _) } =
  case parseItem declOpt of
    CDeclaratorOptionalEmpty ->
      -- TODO: handle bit fields properly
      Right (previousType item, Nothing, Nothing)
    CDeclaratorOptional decl -> do
      (identifier, t) <-
        readDeclarator $ updateTCItem item decl
      return (t, Just identifier, Nothing)

readStructDeclaratorList' ::
     TypeReader CStructDeclaratorList' [(CType, Maybe String, Maybe Int)]
readStructDeclaratorList' TypeCheckItem { typeCheckItem = CStructDeclaratorList'Empty } =
  Right []
readStructDeclaratorList'
     item@TypeCheckItem
            { typeCheckItem = (CStructDeclaratorList' decl list) } = do
  d <- readStructDeclarator $ updateTCItem item decl
  listTail <- readStructDeclaratorList' $ updateTCItem item list
  return $ d : listTail

readStructDeclaratorList ::
     TypeReader CStructDeclaratorList [(CType, Maybe String, Maybe Int)]
readStructDeclaratorList
     item@TypeCheckItem
            { typeCheckItem = (CStructDeclaratorList decl list) } = do
  d <- readStructDeclarator $ updateTCItem item decl
  listTail <- readStructDeclaratorList' $ updateTCItem item list
  return $ d : listTail

readStructDeclaration ::
     TypeReader CStructDeclaration [(CType, Maybe String, Maybe Int)]
readStructDeclaration
     item@TypeCheckItem
            { typeCheckItem = CStructDeclaration specList declList } = do
  (typeSpecifiers, typeQualifiers) <-
    readSpecifierQualifierList $ updateTCItem item specList
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
  readStructDeclaratorList $ updateTCItem (item { previousType = t }) declList

readStructDeclarationList ::
     TypeReader CStructDeclarationList [(CType, Maybe String, Maybe Int)]
readStructDeclarationList
     item@TypeCheckItem
            { typeCheckItem = CStructDeclarationList decl list } =
  case parseItem list of
    CStructDeclarationListOptional list' -> do
      decl' <- readStructDeclaration $ updateTCItem item decl
      declarations <- readStructDeclarationList $ updateTCItem item list'
      return $ decl' ++ declarations
    CStructDeclarationListOptionalEmpty ->
      readStructDeclaration $ updateTCItem item decl

readTypedefName :: TypeReader CTypedefName DataType
readTypedefName
     TypeCheckItem
       { typeCheckLoc = c
       , typeCheckSymbols = sym
       , typeCheckItem =
           CTypedefName ParseItem { parseItem = CIdentifier identifier }
       } =
  case M.lookup identifier $ typedef sym of
    Just t -> Right $ dataType t
    Nothing -> Left $ TypeError c $ "Unknown typedef name '" ++ identifier ++ "'."

---------------
-- FUNCTIONS --
---------------

functionReturnType :: TypeReader CDeclarationSpecifiersOptional CType
functionReturnType TypeCheckItem { typeCheckItem = CDeclarationSpecifiersOptionalEmpty } =
  return CType {storageClass = [], typeQualifier = [], dataType = TShort}
functionReturnType
     item@TypeCheckItem
            { typeCheckItem = CDeclarationSpecifiersOptional spec } = do
  t <- readDeclarationSpecifiers $ updateTCItem item spec
  _ <- validateFunctionStorageClasses $ item { typeCheckItem = storageClass t }
  return t

-- (varargs? [argument types])
readParamTypeList :: TypeReader CParameterTypeList (Bool, [CType])
readParamTypeList
     item@TypeCheckItem
            { typeCheckItem = CParameterTypeList paramList varargs } = do
  params <- readParamList $ updateTCItem item paramList
  let varargs' = parseItem varargs /= CVarArgsOptionalEmpty
  return (varargs', map snd params)

readParamDeclaration :: TypeReader CParameterDeclaration (Maybe String, CType)
readParamDeclaration
     item@TypeCheckItem
            { typeCheckItem = CParameterDeclaration specifiers paramDecl
            } =
  case parseItem paramDecl of
    CParameterDeclaration' decl -> do
      t <- readDeclarationSpecifiers $ updateTCItem item specifiers
      (identifier, t') <- readDeclarator $
        updateTCItem (item { previousType = t }) decl
      return (Just identifier, t')
    CParameterDeclaration'Abstract abstractDeclOpt ->
      case parseItem abstractDeclOpt of
        CAbstractDeclaratorOptionalEmpty -> do
          t <- readDeclarationSpecifiers $ updateTCItem item specifiers
          return (Nothing, t)
        CAbstractDeclaratorOptional abstractDecl -> do
          t <- readDeclarationSpecifiers $ updateTCItem item specifiers
          t' <-
            readAbstractDeclarator $
              updateTCItem (item { previousType = t }) abstractDecl
          return (Nothing, t')

readParamList' :: TypeReader CParameterList' [(Maybe String, CType)]
readParamList' TypeCheckItem { typeCheckItem = CParameterList'Empty } =
  Right []
readParamList'
     item@TypeCheckItem { typeCheckItem = CParameterList' decl list } = do
  t <- readParamDeclaration $ updateTCItem item decl
  listTail <- readParamList' $ updateTCItem item list
  return $ t : listTail

readParamList :: TypeReader CParameterList [(Maybe String, CType)]
readParamList
     item@TypeCheckItem { typeCheckItem = CParameterList decl list } = do
  t <- readParamDeclaration $ updateTCItem item decl
  listTail <- readParamList' $ updateTCItem item list
  return $ t : listTail

-- (varargs?, [(param name, param type)])
readParamTypes :: TypeReader CParameterTypeList (Bool, [(Maybe String, CType)])
readParamTypes
     item@TypeCheckItem { typeCheckItem = CParameterTypeList list varargs } = do
  paramList <- readParamList $ updateTCItem item list
  return (parseItem varargs /= CVarArgsOptionalEmpty, paramList)

validateFunctionReturnType :: Coordinates -> CType -> Either Error ()
validateFunctionReturnType c t =
  case dataType t of
    TFunction {} -> Left $ TypeError c "invalid function return type"
    TStruct _ _ -> Left $ TypeError c "invalid function return type"
    _ -> return ()

-- returns (function name, function type, param names)
readFunctionDeclaration ::
     TypeReader
       CFunctionDefinition
       (String, CType, [String])
-- new style
readFunctionDeclaration
     item@TypeCheckItem
            { typeCheckLoc = c
            , typeCheckItem =
                CFunctionDefinition
                  _
                  ParseItem { parseItem = CDeclarator pointer directDeclarator }
                  ParseItem { parseItem = CDeclarationListOptionalEmpty }
                  _
            } =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left $ TypeError c "Invalid function declaration"
    CDirectDeclaratorId ParseItem { parseItem = CIdentifier fName } decl ->
      case parseItem decl of
        (CDirectDeclarator'ParamTypeList typeList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              returnType <- readPointerOptional $ updateTCItem item pointer
              validateFunctionReturnType c returnType
              (varargs, params) <- readParamTypes $ updateTCItem item typeList
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
                Nothing -> Left $ TypeError c "Unnamed function parameter(s)"
                Just paramNames ->
                  if length (S.fromList paramNames) == length paramNames
                    then return (fName, functionType, paramNames)
                    else Left $ TypeError c "Conflicting parameter names"
            _ -> Left $ TypeError c "Invalid function declaration"
        (CDirectDeclarator'IdList
            ParseItem { parseItem = CIdentifierListOptionalEmpty }
            ParseItem { parseItem = CDirectDeclarator'Empty }) -> do
          returnType <- readPointerOptional $ updateTCItem item pointer
          validateFunctionReturnType c returnType
          let functionType =
                CType
                  { storageClass = []
                  , typeQualifier = []
                  , dataType = TFunction fName returnType [] False
                  }
          return (fName, functionType, [])
        _ -> Left $ TypeError c "Invalid function declaration"
-- old style
readFunctionDeclaration
     item@TypeCheckItem
            { typeCheckLoc = c
            , typeCheckItem =
                CFunctionDefinition
                  _
                  ParseItem { parseItem = (CDeclarator pointer directDeclarator) }
                  ParseItem { parseItem = (CDeclarationListOptional declList) }
                  _
            } =
  case parseItem directDeclarator of
    CDirectDeclaratorParen _ _ ->
      Left $ TypeError c "3Invalid function declaration"
    CDirectDeclaratorId ParseItem { parseItem = CIdentifier fName } decl ->
      case parseItem decl of
        (CDirectDeclarator'IdList idList decl') ->
          case parseItem decl' of
            CDirectDeclarator'Empty -> do
              params <- readDeclarationList $ updateTCItem item declList
              returnType <- readPointerOptional $ updateTCItem item pointer
              validateFunctionReturnType c returnType
              paramNames <- case parseItem idList of
                              CIdentifierListOptionalEmpty -> return []
                              CIdentifierListOptional idList' ->
                                readIdentifierList $ updateTCItem item idList'
              let paramTypeLookup =
                    M.fromList $ map (\(_, k, v) -> (k, v)) params
              let paramTypes = map (`M.lookup` paramTypeLookup) paramNames
              case sequenceA paramTypes of
                Nothing ->
                  Left $ TypeError c "Internal error parsing function parameter types" -- this shouldn't happen
                Just paramTypes' ->
                  if (S.fromList paramNames == S.fromList (map (\(_, k, _) -> k) params)) &&
                     (length (S.fromList paramNames) == length paramNames) &&
                     all (\(initialized, _, _) -> not initialized) params
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
                           , paramNames)
                    else Left $ TypeError c "Invalid parameter declaration"
            _ -> Left $ TypeError c "Invalid function declaration"
        _ -> Left $ TypeError c "Invalid function declaration"

readFunctionDefinition :: TypeReader CFunctionDefinition (String, CType, [String])
readFunctionDefinition
     item@TypeCheckItem
            { typeCheckItem =
                CFunctionDefinition specifiersOpt _ _ _
            } =
  case parseItem specifiersOpt of
    CDeclarationSpecifiersOptionalEmpty ->
      Left $ TypeError (parseLoc specifiersOpt) "Function return type not specified"
    CDeclarationSpecifiersOptional specifiers -> do
      returnType <-
        readDeclarationSpecifiers $ updateTCItem item specifiers
      readFunctionDeclaration $ item { previousType = returnType }

------------------
-- DECLARATIONS --
------------------

-- (assigned?, identifier, type)
readInitDeclarator :: TypeReader CInitDeclarator (Bool, String, CType)
readInitDeclarator
     item@TypeCheckItem { typeCheckItem = CInitDeclarator decl initOpt } = do
  (identifier, t') <- readDeclarator $ updateTCItem item decl
  return (parseItem initOpt /= CAssignInitializerOptionalEmpty, identifier, t')

readInitDeclaratorList' :: TypeReader CInitDeclaratorList' [(Bool, String, CType)]
readInitDeclaratorList' TypeCheckItem { typeCheckItem = CInitDeclaratorList'Empty } =
  Right []
readInitDeclaratorList'
     item@TypeCheckItem { typeCheckItem = CInitDeclaratorList' decl list } = do
  d <- readInitDeclarator $ updateTCItem item decl
  listTail <- readInitDeclaratorList' $ updateTCItem item list
  return $ d : listTail

readInitDeclaratorList :: TypeReader CInitDeclaratorList [(Bool, String, CType)]
readInitDeclaratorList
     item@TypeCheckItem { typeCheckItem = (CInitDeclaratorList decl list) } = do
  d <- readInitDeclarator $ updateTCItem item decl
  listTail <- readInitDeclaratorList' $ updateTCItem item list
  return $ d : listTail

isTypeDef :: TypeReader CDeclarationSpecifiers Bool
isTypeDef item = do
  (storageClasses, _, _) <- readDeclarationSpecifierList item
  return $ SCTypedef `elem` storageClasses

-- (typedef?, [(assigned?, label, type)]
readDeclaration :: TypeReader CDeclaration (Bool, [(Bool, String, CType)])
readDeclaration
     item@TypeCheckItem { typeCheckItem = CDeclaration spec initDeclarators
                         , typeCheckLoc = l
                         } = do
  td <- isTypeDef $ updateTCItem item spec
  if td
    then
      case parseItem initDeclarators of
        CInitDeclaratorListOptionalEmpty -> do
          (typedefLabels, t) <-
            readDeclarationSpecifiersTypedef $ updateTCItem item spec
          let variables = map (False,, t) typedefLabels
          return (True, variables)
        (CInitDeclaratorListOptional list) -> do
          (typedefLabels, t) <-
            readDeclarationSpecifiersTypedef $ updateTCItem item spec
          d <-
            readInitDeclaratorList $
              updateTCItem (item { previousType = t }) list
          if all (\(a, _, _) -> not a) d
            then
              return
                ( True
                , map (False,, t) typedefLabels ++ d
                )
            else
              Left $ TypeError l "Value assigned to typedef declaration"
    else
      case parseItem initDeclarators of
        CInitDeclaratorListOptionalEmpty -> do
          t <- readDeclarationSpecifiers $ updateTCItem item spec
          case dataType t of
            TUnion (Just label) _ -> return (False, [(False, label, t)])
            TStruct (Just label) _ -> return (False, [(False, label, t)])
            TEnum (Just label) _ -> return (False, [(False, label, t)])
            TFunction label _ _ _ -> return (False, [(False, label, t)])
            _ -> Left $ TypeError (parseLoc initDeclarators) "No identifier specified"
        CInitDeclaratorListOptional list -> do
          t <- readDeclarationSpecifiers $ updateTCItem item spec
          d <- readInitDeclaratorList $ updateTCItem (item { previousType = t }) list
          return (False, d)

readDeclarationList :: TypeReader CDeclarationList [(Bool, String, CType)]
readDeclarationList
     item@TypeCheckItem { typeCheckItem = CDeclarationList decl listOpt } =
  case parseItem listOpt of
    CDeclarationListOptionalEmpty -> do
      d <- readDeclaration $ updateTCItem item decl
      if fst d
        then Left $ TypeError (typeCheckLoc item) "Typedef declaration inside a declaration list"
        else return $ snd d
    CDeclarationListOptional list -> do
      listHead <- readDeclaration $ updateTCItem item decl
      listTail <- readDeclarationList $ updateTCItem item list
      if fst listHead
        then Left $ TypeError (typeCheckLoc item) "Typedef declaration inside a declaration list"
        else return $ snd listHead ++ listTail

-----------------
-- DECLARATORS --
-----------------

readDirectDeclarator' :: String -> TypeReader CDirectDeclarator' CType
readDirectDeclarator' _ item@TypeCheckItem { typeCheckItem = CDirectDeclarator'Empty } =
  Right $ previousType item
readDirectDeclarator'
     label
     item@TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectDeclarator'ConstExpr _ directDecl'
            } =
  readDirectDeclarator' label $ updateTCItem (item { previousType = t' }) directDecl'
  where
    t' = CType { storageClass = []
               , typeQualifier = []
               , dataType = TArray t Nothing
               }
readDirectDeclarator'
     fName
     item@TypeCheckItem
            { previousType = t
            , typeCheckLoc = c
            , typeCheckItem =
                CDirectDeclarator'ParamTypeList typeList directDecl'
            } = do
  (varArgs, types) <-
    readParamTypeList $ updateTCItem item typeList
  validateFunctionReturnType c t
  let t' = CType { storageClass = []
                 , typeQualifier = []
                 , dataType = TFunction fName t types varArgs
                 }
  readDirectDeclarator' fName $ updateTCItem (item { previousType = t' }) directDecl'
readDirectDeclarator'
     fName
     item@TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectDeclarator'IdList idList directDecl'
            } =
  case parseItem idList of
    CIdentifierListOptionalEmpty ->
      readDirectDeclarator' fName $
        updateTCItem (item { previousType = t' }) directDecl'
      where t' = CType { storageClass = []
                       , typeQualifier = []
                       , dataType = TFunction fName t [] True
                       }
    _ -> Left $ TypeError (parseLoc idList) "No parameter types given in function declaration"

readDirectDeclarator :: TypeReader CDirectDeclarator (String, CType)
readDirectDeclarator
     item@TypeCheckItem
            { typeCheckItem =
               (CDirectDeclaratorId
                 ParseItem { parseItem = CIdentifier identifier }
                 directDecl')
            } = do
  t' <- readDirectDeclarator' identifier $ updateTCItem item directDecl'
  return (identifier, t')
readDirectDeclarator
     item@TypeCheckItem
            { typeCheckItem = CDirectDeclaratorParen decl directDecl'
            } = do
  (identifier, t') <- readDeclarator $ updateTCItem item decl
  t'' <-
    readDirectDeclarator' identifier $
      updateTCItem (item { previousType = t' }) directDecl'
  return (identifier, t'')

readDeclarator :: TypeReader CDeclarator (String, CType)
readDeclarator
     item@TypeCheckItem
            { typeCheckItem = CDeclarator pointer directDecl
            } = do
  t' <- readPointerOptional $ updateTCItem item pointer
  readDirectDeclarator $ updateTCItem (item { previousType = t' }) directDecl

readDirectAbstractDeclarator' :: TypeReader CDirectAbstractDeclarator' CType
readDirectAbstractDeclarator'
     item@TypeCheckItem
            { typeCheckItem = CDirectAbstractDeclarator'Empty
            } =
  Right $ previousType item
readDirectAbstractDeclarator'
     item@TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectAbstractDeclarator'Const _ decl
            } =
  readDirectAbstractDeclarator' $
    updateTCItem (item { previousType = t' }) decl
  where t' = CType { storageClass = []
                   , typeQualifier = []
                   , dataType = TArray t Nothing
                   }
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator'
     TypeCheckItem { typeCheckItem = (CDirectAbstractDeclarator'Params item _) } =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readDirectAbstractDeclarator :: TypeReader CDirectAbstractDeclarator CType
readDirectAbstractDeclarator
     item@TypeCheckItem
            { previousType = t
            , typeCheckItem = CDirectAbstractDeclaratorIndexed _ decl
            } =
  readDirectAbstractDeclarator' $
    updateTCItem (item { previousType = t' }) decl
  where t' = CType { storageClass = []
                   , typeQualifier = []
                   , dataType = TArray t Nothing
                   }
-- Not sure what this is supposed to do so throw an error.
readDirectAbstractDeclarator
     TypeCheckItem { typeCheckItem = (CDirectAbstractDeclaratorParen item _) } =
  Left $ TypeError (parseLoc item) "Parenthesized type"
-- Same here
readDirectAbstractDeclarator
     TypeCheckItem { typeCheckItem = (CDirectAbstractDeclaratorParams item _) } =
  Left $ TypeError (parseLoc item) "Unexpected parameter list"

readAbstractDeclarator :: TypeReader CAbstractDeclarator CType
readAbstractDeclarator
     item@TypeCheckItem
            { typeCheckItem = CAbstractDeclaratorPointer pointer
            } =
  readPointer $ updateTCItem item pointer
readAbstractDeclarator
    item@TypeCheckItem
           { typeCheckItem = CAbstractDeclaratorDirect pointerOpt directDecl
           } = do
  t' <- readPointerOptional $ updateTCItem item pointerOpt
  readDirectAbstractDeclarator $
    updateTCItem (item { previousType = t' }) directDecl

-------------
-- HELPERS --
-------------

readPointer :: TypeReader CPointer CType
readPointer
     item@TypeCheckItem
            { previousType = t
            , typeCheckItem = CPointer qualifierList pointerOpt
            } = do
  qualifiers <-
    readTypeQualifiers $ updateTCItem item qualifierList
  let t' = CType { storageClass = []
                 , typeQualifier = qualifiers
                 , dataType = TPointer t
                 }
  readPointerOptional $ item { previousType = t'
                             , typeCheckItem = parseItem pointerOpt
                             }

readPointerOptional :: TypeReader CPointerOptional CType
readPointerOptional
     TypeCheckItem { previousType = t
                   , typeCheckItem = CPointerOptionalEmpty } =
  Right t
readPointerOptional item@TypeCheckItem { typeCheckItem = (CPointerOptional pointer) } =
  readPointer $ updateTCItem item pointer

readIdentifierList' :: TypeReader CIdentifierList' [String]
readIdentifierList' TypeCheckItem { typeCheckItem = CIdentifierList'Empty } =
  return []
readIdentifierList'
     item@TypeCheckItem
            { typeCheckItem =
                (CIdentifierList'
                  ParseItem { parseItem = CIdentifier identifier}
                  idList')
            } = do
  listTail <- readIdentifierList' $ updateTCItem item idList'
  return $ identifier : listTail

readIdentifierList :: TypeReader CIdentifierList [String]
readIdentifierList
     item@TypeCheckItem
            { typeCheckItem =
                (CIdentifierList
                  ParseItem { parseItem = (CIdentifier identifier) }
                  idList')
            } = do
  listTail <- readIdentifierList' $ updateTCItem item idList'
  return $ identifier : listTail

