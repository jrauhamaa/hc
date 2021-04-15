module IR where

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

tOptional ::
     Eq a
  => a
  -> (ParseItem b -> a)
  -> (a -> ParseItem b)
  -> TypeAnnotate b
  -> TypeAnnotate a
tOptional empty nonempty extr tSub item@(ParseItem l s i _) sym
  | i == empty = Right $ addSymbols item sym
  | otherwise  =
      do
        subItem <- tSub (extr i) sym
        return $ ParseItem
          { parseLoc = l
          , scanItems = s
          , parseItem = nonempty subItem
          , symbolTable = sym
          }

tIdentifier :: TypeAnnotate CIdentifier
tIdentifier i = Right . addSymbols i

tIdentifierOptional :: TypeAnnotate CIdentifierOptional
tIdentifierOptional =
  tOptional
    CIdentifierOptionalEmpty
    CIdentifierOptional
    (\(CIdentifierOptional x) -> x)
    tIdentifier

tTranslationUnit :: TypeAnnotate CTranslationUnit
tTranslationUnit (ParseItem l s (CTranslationUnit ext opt) _) sym = do
  ext' <- tExternalDeclaration ext sym
  opt' <- tTranslationUnitOptional opt (symbolTable ext')
  let sym' = symbolTable opt'
  return $ ParseItem l s (CTranslationUnit ext' opt') sym'

tTranslationUnitOptional :: TypeAnnotate CTranslationUnitOptional
tTranslationUnitOptional =
  tOptional
    CTranslationUnitOptionalEmpty
    CTranslationUnitOptional
    (\(CTranslationUnitOptional x) -> x)
    tTranslationUnit

tExternalDeclaration :: TypeAnnotate CExternalDeclaration
tExternalDeclaration (ParseItem l s (CExternalDeclarationFunction fd) _) sym = do
  fd' <- tFunctionDefinition fd sym
  let sym' = symbolTable fd'
  return $ ParseItem l s (CExternalDeclarationFunction fd') sym'
tExternalDeclaration (ParseItem l s (CExternalDeclaration decl) _) sym = do
  decl' <- tDeclaration decl sym
  let sym' = symbolTable decl'
  return $ ParseItem l s (CExternalDeclaration decl') sym'

functionType ::
     PI CDeclarationSpecifiersOptional
  -> PI CDeclarator
  -> PI CDeclarationListOptional
  -> SymbolTable
  -> (String, CType)
functionType = undefined

addSymbol :: SymbolTable -> String -> CType -> Either TypeError SymbolTable
addSymbol = undefined

tFunctionDefinition :: TypeAnnotate CFunctionDefinition
tFunctionDefinition (ParseItem l s (CFunctionDefinition spec decl declList compStatement) _) sym = do
  let (fName, fType) = functionType spec decl declList sym
  spec'          <- tDeclarationSpecifiersOptional spec sym
  decl'          <- tDeclarator decl $ symbolTable spec'
  declSym        <- addSymbol (symbolTable decl') fName fType
  declList'      <- tDeclarationListOptional declList declSym
  compStatement' <- tCompoundStatement compStatement $ symbolTable declList'
  sym'           <- addSymbol sym fName fType
  return $
    ParseItem
      l
      s
      (CFunctionDefinition spec' decl' declList' compStatement')
      sym'

varType :: PI CDeclarationSpecifiers -> CType
varType = undefined

varNames :: PI CInitDeclaratorListOptional -> [String]
varNames = undefined

tDeclaration :: TypeAnnotate CDeclaration
tDeclaration (ParseItem l s (CDeclaration spec initList) _) sym = do
  let vType  = varType spec
      vNames = varNames initList
  spec' <- tDeclarationSpecifiers spec sym
  initList' <- tInitDeclaratorListOptional initList $ symbolTable spec'
  sym' <- foldl (updateSym vType) (Right $ symbolTable initList') vNames
  return $ ParseItem l s (CDeclaration spec' initList') sym'
  where
    updateSym t prev name = prev >>= (\p -> addSymbol p name t)

tDeclarationList :: TypeAnnotate CDeclarationList
tDeclarationList (ParseItem l s (CDeclarationList decl opt) _) sym = do
  decl' <- tDeclaration decl sym
  opt' <- tDeclarationListOptional opt (symbolTable decl')
  let sym' = symbolTable opt'
  return $ ParseItem l s (CDeclarationList decl' opt') sym'

tDeclarationListOptional :: TypeAnnotate CDeclarationListOptional
tDeclarationListOptional =
  tOptional
    CDeclarationListOptionalEmpty
    CDeclarationListOptional
    (\(CDeclarationListOptional x) -> x)
    tDeclarationList

tDeclarationSpecifiers :: TypeAnnotate CDeclarationSpecifiers
tDeclarationSpecifiers (ParseItem l s (CDeclarationSpecifiersStorageClass spec opt) _) sym = do
  spec' <- tStorageClassSpecifier spec sym
  opt' <- tDeclarationSpecifiersOptional opt (symbolTable spec')
  let sym' = symbolTable opt'
  return $ ParseItem l s (CDeclarationSpecifiersStorageClass spec' opt') sym'
tDeclarationSpecifiers (ParseItem l s (CDeclarationSpecifiersTypeSpecifier spec opt) _) sym = do
  spec' <- tTypeSpecifier spec sym
  opt' <- tDeclarationSpecifiersOptional opt (symbolTable spec')
  let sym' = symbolTable opt'
  return $ ParseItem l s (CDeclarationSpecifiersTypeSpecifier spec' opt') sym'
tDeclarationSpecifiers (ParseItem l s (CDeclarationSpecifiersTypeQualifier spec opt) _) sym = do
  spec' <- tTypeQualifier spec sym
  opt' <- tDeclarationSpecifiersOptional opt (symbolTable spec')
  let sym' = symbolTable opt'
  return $ ParseItem l s (CDeclarationSpecifiersTypeQualifier spec' opt') sym'


tDeclarationSpecifiersOptional :: TypeAnnotate CDeclarationSpecifiersOptional
tDeclarationSpecifiersOptional =
  tOptional
    CDeclarationSpecifiersOptionalEmpty
    CDeclarationSpecifiersOptional
    (\(CDeclarationSpecifiersOptional x) -> x)
    tDeclarationSpecifiers

tStorageClassSpecifier :: TypeAnnotate CStorageClassSpecifier
tStorageClassSpecifier i = Right . addSymbols i

tTypeSpecifier :: TypeAnnotate CTypeSpecifier
tTypeSpecifier (ParseItem l s (CTypeSpecifierStructOrUnion spec) _) sym = do
  spec' <- tStructOrUnionSpecifier spec sym
  return $ ParseItem l s (CTypeSpecifierStructOrUnion spec') (symbolTable spec')
tTypeSpecifier (ParseItem l s (CTypeSpecifierEnum spec) _) sym = do
  spec' <- tEnumSpecifier spec sym
  return $ ParseItem l s (CTypeSpecifierEnum spec') (symbolTable spec')
tTypeSpecifier (ParseItem l s (CTypeSpecifierTypedef spec) _) sym = do
  spec' <- tTypedefName spec sym
  return $ ParseItem l s (CTypeSpecifierTypedef spec') (symbolTable spec')
tTypeSpecifier i sym =
  Right $ addSymbols i sym

tTypeQualifier :: TypeAnnotate CTypeQualifier
tTypeQualifier i = Right . addSymbols i

tStructOrUnionSpecifier :: TypeAnnotate CStructOrUnionSpecifier
tStructOrUnionSpecifier (ParseItem l s (CStructOrUnionSpecifierList su i decl) _) sym = do
  su' <- tStructOrUnion su sym
  i' <- tIdentifierOptional i $ symbolTable su'
  decl' <- tStructDeclarationList decl $ symbolTable i'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CStructOrUnionSpecifierList su' i' decl') sym'
tStructOrUnionSpecifier (ParseItem l s (CStructOrUnionSpecifier su i) _) sym = do
  su' <- tStructOrUnion su sym
  i' <- tIdentifier i $ symbolTable su'
  let sym' = symbolTable i'
  return $ ParseItem l s (CStructOrUnionSpecifier su' i') sym'

tStructOrUnion :: TypeAnnotate CStructOrUnion
tStructOrUnion i = Right . addSymbols i

tStructDeclarationList :: TypeAnnotate CStructDeclarationList
tStructDeclarationList (ParseItem l s (CStructDeclarationList decl lst) _) sym = do
  decl' <- tStructDeclaration decl sym
  lst' <- tStructDeclarationListOptional lst $ symbolTable decl'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CStructDeclarationList decl' lst') sym'

tStructDeclarationListOptional :: TypeAnnotate CStructDeclarationListOptional
tStructDeclarationListOptional =
  tOptional
    CStructDeclarationListOptionalEmpty
    CStructDeclarationListOptional
    (\(CStructDeclarationListOptional x) -> x)
    tStructDeclarationList

tInitDeclaratorList :: TypeAnnotate CInitDeclaratorList
tInitDeclaratorList (ParseItem l s (CInitDeclaratorList decl lst) _) sym = do
  decl' <- tInitDeclarator decl sym
  lst' <- tInitDeclaratorList' lst $ symbolTable decl'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CInitDeclaratorList decl' lst') sym'

tInitDeclaratorListOptional :: TypeAnnotate CInitDeclaratorListOptional
tInitDeclaratorListOptional =
  tOptional
    CInitDeclaratorListOptionalEmpty
    CInitDeclaratorListOptional
    (\(CInitDeclaratorListOptional x) -> x)
    tInitDeclaratorList

tInitDeclaratorList' :: TypeAnnotate CInitDeclaratorList'
tInitDeclaratorList' (ParseItem l s (CInitDeclaratorList' decl lst) _) sym = do
  decl' <- tInitDeclarator decl sym
  lst' <- tInitDeclaratorList' lst $ symbolTable decl'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CInitDeclaratorList' decl' lst') sym'
tInitDeclaratorList' i sym =
  Right $ addSymbols i sym

tInitDeclarator :: TypeAnnotate CInitDeclarator
tInitDeclarator (ParseItem l s (CInitDeclarator decl initializer) _) sym = do
  decl' <- tDeclarator decl sym
  initializer' <- tAssignInitializerOptional initializer $ symbolTable decl'
  let sym' = symbolTable initializer'
  return $ ParseItem l s (CInitDeclarator decl' initializer') sym'

tAssignInitializerOptional :: TypeAnnotate CAssignInitializerOptional
tAssignInitializerOptional =
  tOptional
    CAssignInitializerOptionalEmpty
    CAssignInitializerOptional
    (\(CAssignInitializerOptional x) -> x)
    tInitializer

tStructDeclaration :: TypeAnnotate CStructDeclaration
tStructDeclaration (ParseItem l s (CStructDeclaration spec decl) _) sym = do
  spec' <- tSpecifierQualifierList spec sym
  decl' <- tStructDeclaratorList decl $ symbolTable spec'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CStructDeclaration spec' decl') sym'

tSpecifierQualifierList :: TypeAnnotate CSpecifierQualifierList
tSpecifierQualifierList (ParseItem l s (CSpecifierQualifierListSpecifier spec lst) _) sym = do
  spec' <- tTypeSpecifier spec sym
  lst' <- tSpecifierQualifierListOptional lst $ symbolTable spec'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CSpecifierQualifierListSpecifier spec' lst') sym'
tSpecifierQualifierList (ParseItem l s (CSpecifierQualifierListQualifier qualifier lst) _) sym = do
  qualifier' <- tTypeQualifier qualifier sym
  lst' <- tSpecifierQualifierListOptional lst $ symbolTable qualifier'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CSpecifierQualifierListQualifier qualifier' lst') sym'

tSpecifierQualifierListOptional :: TypeAnnotate CSpecifierQualifierListOptional
tSpecifierQualifierListOptional =
  tOptional
    CSpecifierQualifierListOptionalEmpty
    CSpecifierQualifierListOptional
    (\(CSpecifierQualifierListOptional x) -> x)
    tSpecifierQualifierList

tStructDeclaratorList :: TypeAnnotate CStructDeclaratorList
tStructDeclaratorList (ParseItem l s (CStructDeclaratorList decl lst) _) sym = do
  decl' <- tStructDeclarator decl sym
  lst' <- tStructDeclaratorList' lst $ symbolTable decl'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CStructDeclaratorList decl' lst') sym'

tStructDeclaratorList' :: TypeAnnotate CStructDeclaratorList'
tStructDeclaratorList' (ParseItem l s (CStructDeclaratorList' decl lst) _) sym = do
  decl' <- tStructDeclarator decl sym
  lst' <- tStructDeclaratorList' lst $ symbolTable decl'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CStructDeclaratorList' decl' lst') sym'
tStructDeclaratorList' i sym =
  Right $ addSymbols i sym

tStructDeclarator :: TypeAnnotate CStructDeclarator
tStructDeclarator (ParseItem l s (CStructDeclarator decl) _) sym = do
  decl' <- tDeclarator decl sym
  let sym' = symbolTable decl'
  return $ ParseItem l s (CStructDeclarator decl') sym'
tStructDeclarator (ParseItem l s (CStructDeclaratorField decl expr) _) sym = do
  decl' <- tDeclaratorOptional decl sym
  expr' <- tConstantExpression expr $ symbolTable decl'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CStructDeclaratorField decl' expr') sym'

tEnumSpecifier :: TypeAnnotate CEnumSpecifier
tEnumSpecifier (ParseItem l s (CEnumSpecifierList identifier lst) _) sym = do
  identifier' <- tIdentifierOptional identifier sym
  lst' <- tEnumeratorList lst $ symbolTable identifier'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CEnumSpecifierList identifier' lst') sym'
tEnumSpecifier (ParseItem l s (CEnumSpecifier identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  let sym' = symbolTable identifier'
  return $ ParseItem l s (CEnumSpecifier identifier') sym'

tEnumeratorList :: TypeAnnotate CEnumeratorList
tEnumeratorList (ParseItem l s (CEnumeratorList enum lst) _) sym = do
  enum' <- tEnumerator enum sym
  lst' <- tEnumeratorList' lst $ symbolTable enum'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CEnumeratorList enum' lst') sym'

tEnumeratorList' :: TypeAnnotate CEnumeratorList'
tEnumeratorList' (ParseItem l s (CEnumeratorList' enum lst) _) sym = do
  enum' <- tEnumerator enum sym
  lst' <- tEnumeratorList' lst $ symbolTable enum'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CEnumeratorList' enum' lst') sym'
tEnumeratorList' i sym =
  Right $ addSymbols i sym

tEnumerator :: TypeAnnotate CEnumerator
tEnumerator (ParseItem l s (CEnumerator identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  let sym' = symbolTable identifier'
  return $ ParseItem l s (CEnumerator identifier') sym'
tEnumerator (ParseItem l s (CEnumeratorAssign identifier expr) _) sym = do
  identifier' <- tIdentifier identifier sym
  expr' <- tConstantExpression expr $ symbolTable identifier'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CEnumeratorAssign identifier' expr') sym'

tDeclarator :: TypeAnnotate CDeclarator
tDeclarator (ParseItem l s (CDeclarator pointer decl) _) sym = do
  pointer' <- tPointerOptional pointer sym
  decl' <- tDirectDeclarator decl $ symbolTable pointer'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDeclarator pointer' decl') sym'

tDeclaratorOptional :: TypeAnnotate CDeclaratorOptional
tDeclaratorOptional =
  tOptional
    CDeclaratorOptionalEmpty
    CDeclaratorOptional
    (\(CDeclaratorOptional x) -> x)
    tDeclarator

tDirectDeclarator :: TypeAnnotate CDirectDeclarator
tDirectDeclarator (ParseItem l s (CDirectDeclaratorId identifier decl) _) sym = do
  identifier' <- tIdentifier identifier sym
  decl' <- tDirectDeclarator' decl $ symbolTable identifier'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDirectDeclaratorId identifier' decl') sym'
tDirectDeclarator (ParseItem l s (CDirectDeclaratorParen decl directDecl) _) sym = do
  decl' <- tDeclarator decl sym
  directDecl' <- tDirectDeclarator' directDecl $ symbolTable decl'
  let sym' = symbolTable directDecl'
  return $ ParseItem l s (CDirectDeclaratorParen decl' directDecl') sym'

tDirectDeclarator' :: TypeAnnotate CDirectDeclarator'
tDirectDeclarator' (ParseItem l s (CDirectDeclarator'ConstExpr expr decl) _) sym = do
  expr' <- tConstantExpressionOptional expr sym
  decl' <- tDirectDeclarator' decl $ symbolTable expr
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDirectDeclarator'ConstExpr expr' decl') sym'
tDirectDeclarator' (ParseItem l s (CDirectDeclarator'ParamTypeList lst decl) _) sym = do
  lst' <- tParameterTypeList lst sym
  decl' <- tDirectDeclarator' decl $ symbolTable lst
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDirectDeclarator'ParamTypeList lst' decl') sym'
tDirectDeclarator' (ParseItem l s (CDirectDeclarator'IdList lst decl) _) sym = do
  lst' <- tIdentifierListOptional lst sym
  decl' <- tDirectDeclarator' decl $ symbolTable lst
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDirectDeclarator'IdList lst' decl') sym'
tDirectDeclarator' i sym =
  Right $ addSymbols i sym

tPointer :: TypeAnnotate CPointer
tPointer (ParseItem l s (CPointer lst pointer) _) sym = do
  lst' <- tTypeQualifierListOptional lst sym
  pointer' <- tPointerOptional pointer $ symbolTable lst'
  let sym' = symbolTable pointer'
  return $ ParseItem l s (CPointer lst' pointer') sym'

tPointerOptional :: TypeAnnotate CPointerOptional
tPointerOptional =
  tOptional
    CPointerOptionalEmpty
    CPointerOptional
    (\(CPointerOptional x) -> x)
    tPointer

tTypeQualifierList :: TypeAnnotate CTypeQualifierList
tTypeQualifierList (ParseItem l s (CTypeQualifierList qualifier lst) _) sym = do
  qualifier' <- tTypeQualifier qualifier sym
  lst' <- tTypeQualifierListOptional lst $ symbolTable qualifier'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CTypeQualifierList qualifier' lst') sym'

tTypeQualifierListOptional :: TypeAnnotate CTypeQualifierListOptional
tTypeQualifierListOptional =
  tOptional
    CTypeQualifierListOptionalEmpty
    CTypeQualifierListOptional
    (\(CTypeQualifierListOptional x) -> x)
    tTypeQualifierList

tParameterTypeList :: TypeAnnotate CParameterTypeList
tParameterTypeList (ParseItem l s (CParameterTypeList lst varargs) _) sym = do
  lst' <- tParameterList lst sym
  varargs' <- tVarArgsOptional varargs $ symbolTable lst'
  let sym' = symbolTable varargs'
  return $ ParseItem l s (CParameterTypeList lst' varargs') sym'

tParameterTypeListOptional :: TypeAnnotate CParameterTypeListOptional
tParameterTypeListOptional =
  tOptional
    CParameterTypeListOptionalEmpty
    CParameterTypeListOptional
    (\(CParameterTypeListOptional x) -> x)
    tParameterTypeList

tVarArgsOptional :: TypeAnnotate CVarArgsOptional
tVarArgsOptional i = Right . addSymbols i

tParameterList :: TypeAnnotate CParameterList
tParameterList (ParseItem l s (CParameterList decl lst) _) sym = do
  decl' <- tParameterDeclaration decl sym
  lst' <- tParameterList' lst $ symbolTable lst
  let sym' = symbolTable lst'
  return $ ParseItem l s (CParameterList decl' lst') sym'

tParameterList' :: TypeAnnotate CParameterList'
tParameterList' (ParseItem l s (CParameterList' decl lst) _) sym = do
  decl' <- tParameterDeclaration decl sym
  lst' <- tParameterList' lst $ symbolTable decl'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CParameterList' decl' lst') sym'
tParameterList' i sym = Right $ addSymbols i sym

tParameterDeclaration :: TypeAnnotate CParameterDeclaration
tParameterDeclaration (ParseItem l s (CParameterDeclaration spec decl) _) sym = do
  spec' <- tDeclarationSpecifiers spec sym
  decl' <- tParameterDeclaration' decl $ symbolTable spec'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CParameterDeclaration spec' decl') sym'

tParameterDeclaration' :: TypeAnnotate CParameterDeclaration'
tParameterDeclaration' (ParseItem l s (CParameterDeclaration' decl) _) sym = do
  decl' <- tDeclarator decl sym
  let sym' = symbolTable decl'
  return $ ParseItem l s (CParameterDeclaration' decl') sym'
tParameterDeclaration' (ParseItem l s (CParameterDeclaration'Abstract decl) _) sym = do
  decl' <- tAbstractDeclaratorOptional decl sym
  let sym' = symbolTable decl'
  return $ ParseItem l s (CParameterDeclaration'Abstract decl') sym'

tIdentifierList :: TypeAnnotate CIdentifierList
tIdentifierList (ParseItem l s (CIdentifierList identifier lst) _) sym = do
  identifier' <- tIdentifier identifier sym
  lst' <- tIdentifierList' lst $ symbolTable identifier'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CIdentifierList identifier' lst') sym'

tIdentifierListOptional :: TypeAnnotate CIdentifierListOptional
tIdentifierListOptional =
  tOptional
    CIdentifierListOptionalEmpty
    CIdentifierListOptional
    (\(CIdentifierListOptional x) -> x)
    tIdentifierList

tIdentifierList' :: TypeAnnotate CIdentifierList'
tIdentifierList' (ParseItem l s (CIdentifierList' identifier lst) _) sym = do
  identifier' <- tIdentifier identifier sym
  lst' <- tIdentifierList' lst $ symbolTable identifier'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CIdentifierList' identifier' lst') sym'
tIdentifierList' i sym =
  Right $ addSymbols i sym

tInitializer :: TypeAnnotate CInitializer
tInitializer (ParseItem l s (CInitializerAssignment expr) _) sym = do
  expr' <- tAssignmentExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CInitializerAssignment expr') sym'
tInitializer (ParseItem l s (CInitializerInitList lst) _) sym = do
  lst' <- tInitializerList lst sym
  let sym' = symbolTable lst'
  return $ ParseItem l s (CInitializerInitList lst') sym'

tInitializerList :: TypeAnnotate CInitializerList
tInitializerList (ParseItem l s (CInitializerList initializer lst) _) sym = do
  initializer' <- tInitializer initializer sym
  lst' <- tInitializerList' lst $ symbolTable initializer'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CInitializerList initializer' lst') sym'

tInitializerList' :: TypeAnnotate CInitializerList'
tInitializerList' (ParseItem l s (CInitializerList' initializer lst) _) sym = do
  initializer' <- tInitializer initializer sym
  lst' <- tInitializerList' lst $ symbolTable initializer'
  let sym' = symbolTable lst'
  return $ ParseItem l s (CInitializerList' initializer' lst') sym'
tInitializerList' i sym =
  Right $ addSymbols i sym

tTypeName :: TypeAnnotate CTypeName
tTypeName (ParseItem l s (CTypeName lst decl) _) sym = do
  lst' <- tSpecifierQualifierList lst sym
  decl' <- tAbstractDeclaratorOptional decl $ symbolTable lst'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CTypeName lst' decl') sym'

tAbstractDeclarator :: TypeAnnotate CAbstractDeclarator
tAbstractDeclarator (ParseItem l s (CAbstractDeclaratorPointer pointer) _) sym = do
  pointer' <- tPointer pointer sym
  let sym' = symbolTable pointer'
  return $ ParseItem l s (CAbstractDeclaratorPointer pointer') sym'
tAbstractDeclarator (ParseItem l s (CAbstractDeclaratorDirect pointer decl) _) sym = do
  pointer' <- tPointerOptional pointer sym
  decl' <- tDirectAbstractDeclarator decl $ symbolTable pointer'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CAbstractDeclaratorDirect pointer' decl') sym'

tAbstractDeclaratorOptional :: TypeAnnotate CAbstractDeclaratorOptional
tAbstractDeclaratorOptional =
  tOptional
    CAbstractDeclaratorOptionalEmpty
    CAbstractDeclaratorOptional
    (\(CAbstractDeclaratorOptional x) -> x)
    tAbstractDeclarator

tDirectAbstractDeclarator :: TypeAnnotate CDirectAbstractDeclarator
tDirectAbstractDeclarator (ParseItem l s (CDirectAbstractDeclarator decl rest) _) sym = do
  decl' <- tAbstractDeclarator decl sym
  rest' <- tDirectAbstractDeclarator' rest $ symbolTable decl'
  let sym' = symbolTable rest'
  return $ ParseItem l s (CDirectAbstractDeclarator decl' rest') sym'

tDirectAbstractDeclarator' :: TypeAnnotate CDirectAbstractDeclarator'
tDirectAbstractDeclarator' (ParseItem l s (CDirectAbstractDeclarator'Const expr decl) _) sym = do
  expr' <- tConstantExpressionOptional expr sym
  decl' <- tDirectAbstractDeclarator' decl $ symbolTable expr'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDirectAbstractDeclarator'Const expr' decl') sym'
tDirectAbstractDeclarator' (ParseItem l s (CDirectAbstractDeclarator'Params lst decl) _) sym = do
  lst' <- tParameterTypeListOptional lst sym
  decl' <- tDirectAbstractDeclarator' decl $ symbolTable lst'
  let sym' = symbolTable decl'
  return $ ParseItem l s (CDirectAbstractDeclarator'Params lst' decl') sym'
tDirectAbstractDeclarator' i sym =
  Right $ addSymbols i sym

tTypedefName :: TypeAnnotate CTypedefName
tTypedefName (ParseItem l s (CTypedefName identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  let sym' = symbolTable identifier'
  return $ ParseItem l s (CTypedefName identifier') sym'

tStatement :: TypeAnnotate CStatement
tStatement (ParseItem l s (CStatementLabeled statement) _) sym = do
  statement' <- tLabeledStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CStatementLabeled statement') sym'
tStatement (ParseItem l s (CStatementExpression statement) _) sym = do
  statement' <- tExpressionStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CStatementExpression statement') sym'
tStatement (ParseItem l s (CStatementCompound statement) _) sym = do
  statement' <- tCompoundStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CStatementCompound statement') sym'
tStatement (ParseItem l s (CStatementSelection statement) _) sym = do
  statement' <- tSelectionStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CStatementSelection statement') sym'
tStatement (ParseItem l s (CStatementIteration statement) _) sym = do
  statement' <- tIterationStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CStatementIteration statement') sym'
tStatement (ParseItem l s (CStatementJump statement) _) sym = do
  statement' <- tJumpStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CStatementJump statement') sym'

tLabeledStatement :: TypeAnnotate CLabeledStatement
tLabeledStatement (ParseItem l s (CLabeledStatementId identifier statement) _) sym = do
  identifier' <- tIdentifier identifier sym
  statement' <- tStatement statement $ symbolTable identifier'
  let sym' = symbolTable statement'
  return $ ParseItem l s (CLabeledStatementId identifier' statement') sym'
tLabeledStatement (ParseItem l s (CLabeledStatementCase expr statement) _) sym = do
  expr' <- tConstantExpression expr sym
  statement' <- tStatement statement $ symbolTable expr'
  let sym' = symbolTable statement'
  return $ ParseItem l s (CLabeledStatementCase expr' statement') sym'
tLabeledStatement (ParseItem l s (CLabeledStatementDefault statement) _) sym = do
  statement' <- tStatement statement sym
  let sym' = symbolTable statement'
  return $ ParseItem l s (CLabeledStatementDefault statement') sym'

tExpressionStatement :: TypeAnnotate CExpressionStatement
tExpressionStatement (ParseItem l s (CExpressionStatement expr) _) sym = do
  expr' <- tExpressionOptional expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CExpressionStatement expr') sym'

tCompoundStatement :: TypeAnnotate CCompoundStatement
tCompoundStatement (ParseItem l s (CCompoundStatement decl statement) _) sym = do
  decl' <- tDeclarationListOptional decl sym
  statement' <- tStatementListOptional statement $ symbolTable decl'
  let sym' = symbolTable statement'
  return $ ParseItem l s (CCompoundStatement decl' statement') sym'

tStatementList :: TypeAnnotate CStatementList
tStatementList (ParseItem l s (CStatementList statement list) _) sym = do
  statement' <- tStatement statement sym
  list' <- tStatementListOptional list $ symbolTable statement'
  let sym' = symbolTable list'
  return $ ParseItem l s (CStatementList statement' list') sym'

tStatementListOptional :: TypeAnnotate CStatementListOptional
tStatementListOptional =
  tOptional
    CStatementListOptionalEmpty
    CStatementListOptional
    (\(CStatementListOptional x) -> x)
    tStatementList

tSelectionStatement :: TypeAnnotate CSelectionStatement
tSelectionStatement (ParseItem l s (CSelectionStatementIf expr statement elseopt) _) sym = do
  expr' <- tExpression expr sym
  statement' <- tStatement statement $ symbolTable expr'
  elseopt' <- tElseOptional elseopt $ symbolTable statement'
  let sym' = symbolTable elseopt'
  return $ ParseItem l s (CSelectionStatementIf expr' statement' elseopt') sym'
tSelectionStatement (ParseItem l s (CSelectionStatementSwitch expr statement) _) sym = do
  expr' <- tExpression expr sym
  statement' <- tStatement statement $ symbolTable expr'
  let sym' = symbolTable statement'
  return $ ParseItem l s (CSelectionStatementSwitch expr' statement') sym'

tElseOptional :: TypeAnnotate CElseOptional
tElseOptional =
  tOptional
    CElseOptionalEmpty
    CElseOptional
    (\(CElseOptional x) -> x)
    tStatement

tIterationStatement :: TypeAnnotate CIterationStatement
tIterationStatement (ParseItem l s (CIterationStatementWhile expr statement) _) sym = do
  expr' <- tExpression expr sym
  statement' <- tStatement statement $ symbolTable expr'
  let sym' = symbolTable statement'
  return $ ParseItem l s (CIterationStatementWhile expr' statement') sym'
tIterationStatement (ParseItem l s (CIterationStatementDoWhile statement expr) _) sym = do
  statement' <- tStatement statement sym
  expr' <- tExpression expr $ symbolTable statement'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CIterationStatementDoWhile statement' expr') sym'
tIterationStatement (ParseItem l s (CIterationStatementFor decl incr cond body) _) sym = do
  decl' <- tExpressionOptional decl sym
  incr' <- tExpressionOptional incr $ symbolTable decl'
  cond' <- tExpressionOptional cond $ symbolTable incr'
  body' <- tStatement body $ symbolTable cond'
  let sym' = symbolTable body'
  return $ ParseItem l s (CIterationStatementFor decl' incr' cond' body') sym'

tJumpStatement :: TypeAnnotate CJumpStatement
tJumpStatement (ParseItem l s (CJumpStatementGoto identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  let sym' = symbolTable identifier'
  return $ ParseItem l s (CJumpStatementGoto identifier') sym'
tJumpStatement (ParseItem l s (CJumpStatementContinue) _) sym =
  return $ ParseItem l s (CJumpStatementContinue) sym
tJumpStatement (ParseItem l s (CJumpStatementBreak) _) sym =
  return $ ParseItem l s (CJumpStatementBreak) sym
tJumpStatement (ParseItem l s (CJumpStatementReturn expr) _) sym = do
  expr' <- tExpressionOptional expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CJumpStatementReturn expr') sym'

tExpression :: TypeAnnotate CExpression
tExpression (ParseItem l s (CExpression assign expr) _) sym = do
  assign' <- tAssignmentExpression assign sym
  expr' <- tExpression' expr $ symbolTable assign'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CExpression assign' expr') sym'

tExpressionOptional :: TypeAnnotate CExpressionOptional
tExpressionOptional =
  tOptional
    CExpressionOptionalEmpty
    CExpressionOptional
    (\(CExpressionOptional x) -> x)
    tExpression

tExpression' :: TypeAnnotate CExpression'
tExpression' (ParseItem l s CExpression'Empty _) sym =
  return $ ParseItem l s CExpression'Empty sym
tExpression' (ParseItem l s (CExpression' assign expr) _) sym = do
  assign' <- tAssignmentExpression assign sym
  expr' <- tExpression' expr $ symbolTable assign'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CExpression' assign' expr') sym'

tAssignmentExpression :: TypeAnnotate CAssignmentExpression
tAssignmentExpression (ParseItem l s (CAssignmentExpressionConditional expr) _) sym = do
  expr' <- tConditionalExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CAssignmentExpressionConditional expr') sym'
tAssignmentExpression (ParseItem l s (CAssignmentExpression unary operator expr) _) sym = do
  unary' <- tUnaryExpression unary sym
  operator' <- tAssignmentOperator operator $ symbolTable unary'
  expr' <- tAssignmentExpression expr $ symbolTable operator'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CAssignmentExpression unary' operator' expr') sym'

tAssignmentOperator :: TypeAnnotate CAssignmentOperator
tAssignmentOperator (ParseItem l s expr _) sym =
  return $ ParseItem l s expr sym

tConditionalExpression :: TypeAnnotate CConditionalExpression
tConditionalExpression (ParseItem l s (CConditionalExpression expr ternary) _) sym = do
  expr' <- tLogicalOrExpression expr sym
  ternary' <- tTernaryOptional ternary $ symbolTable expr'
  let sym' = symbolTable ternary'
  return $ ParseItem l s (CConditionalExpression expr' ternary') sym'

tTernaryOptional :: TypeAnnotate CTernaryOptional
tTernaryOptional (ParseItem l s CTernaryOptionalEmpty _) sym =
  return $ ParseItem l s CTernaryOptionalEmpty sym
tTernaryOptional (ParseItem l s (CTernaryOptional expr cond) _) sym = do
  expr' <- tExpression expr sym
  cond' <- tConditionalExpression cond $ symbolTable expr'
  let sym' = symbolTable cond'
  return $ ParseItem l s (CTernaryOptional expr' cond') sym'

tConstantExpression :: TypeAnnotate CConstantExpression
tConstantExpression (ParseItem l s (CConstantExpression expr) _) sym = do
  expr' <- tConditionalExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CConstantExpression expr') sym'

tConstantExpressionOptional :: TypeAnnotate CConstantExpressionOptional
tConstantExpressionOptional =
  tOptional
    CConstantExpressionOptionalEmpty
    CConstantExpressionOptional
    (\(CConstantExpressionOptional x) -> x)
    tConstantExpression

tLogicalOrExpression :: TypeAnnotate CLogicalOrExpression
tLogicalOrExpression (ParseItem l s (CLogicalOrExpression andExpr orExpr) _) sym = do
  andExpr' <- tLogicalAndExpression andExpr sym
  orExpr' <- tLogicalOrExpression' orExpr $ symbolTable andExpr'
  let sym' = symbolTable orExpr'
  return $ ParseItem l s (CLogicalOrExpression andExpr' orExpr') sym'

tLogicalOrExpression' :: TypeAnnotate CLogicalOrExpression'
tLogicalOrExpression' (ParseItem l s CLogicalOrExpression'Empty _) sym =
  return $ ParseItem l s CLogicalOrExpression'Empty sym
tLogicalOrExpression' (ParseItem l s (CLogicalOrExpression' andExpr orExpr) _) sym = do
  andExpr' <- tLogicalAndExpression andExpr sym
  orExpr' <- tLogicalOrExpression' orExpr $ symbolTable andExpr'
  let sym' = symbolTable orExpr'
  return $ ParseItem l s (CLogicalOrExpression' andExpr' orExpr') sym'

tLogicalAndExpression :: TypeAnnotate CLogicalAndExpression
tLogicalAndExpression (ParseItem l s (CLogicalAndExpression orExpr andExpr) _) sym = do
  orExpr' <- tInclusiveOrExpression orExpr sym
  andExpr' <- tLogicalAndExpression' andExpr $ symbolTable orExpr'
  let sym' = symbolTable andExpr'
  return $ ParseItem l s (CLogicalAndExpression orExpr' andExpr') sym'

tLogicalAndExpression' :: TypeAnnotate CLogicalAndExpression'
tLogicalAndExpression' (ParseItem l s CLogicalAndExpression'Empty _) sym =
  return $ ParseItem l s CLogicalAndExpression'Empty sym
tLogicalAndExpression' (ParseItem l s (CLogicalAndExpression' orExpr andExpr) _) sym = do
  orExpr' <- tInclusiveOrExpression orExpr sym
  andExpr' <- tLogicalAndExpression' andExpr $ symbolTable orExpr'
  let sym' = symbolTable andExpr'
  return $ ParseItem l s (CLogicalAndExpression' orExpr' andExpr') sym'

tInclusiveOrExpression :: TypeAnnotate CInclusiveOrExpression
tInclusiveOrExpression (ParseItem l s (CInclusiveOrExpression exclusive inclusive) _) sym = do
  exclusive' <- tExclusiveOrExpression exclusive sym
  inclusive' <- tInclusiveOrExpression' inclusive $ symbolTable exclusive'
  let sym' = symbolTable inclusive'
  return $ ParseItem l s (CInclusiveOrExpression exclusive' inclusive') sym'

tInclusiveOrExpression' :: TypeAnnotate CInclusiveOrExpression'
tInclusiveOrExpression' (ParseItem l s (CInclusiveOrExpression'Empty) _) sym =
  return $ ParseItem l s CInclusiveOrExpression'Empty sym
tInclusiveOrExpression' (ParseItem l s (CInclusiveOrExpression' exclusive inclusive) _) sym = do
  exclusive' <- tExclusiveOrExpression exclusive sym
  inclusive' <- tInclusiveOrExpression' inclusive $ symbolTable exclusive'
  let sym' = symbolTable inclusive'
  return $ ParseItem l s (CInclusiveOrExpression' exclusive' inclusive') sym'

tExclusiveOrExpression :: TypeAnnotate CExclusiveOrExpression
tExclusiveOrExpression (ParseItem l s (CExclusiveOrExpression andExpr orExpr) _) sym = do
  andExpr' <- tAndExpression andExpr sym
  orExpr' <- tExclusiveOrExpression' orExpr $ symbolTable andExpr'
  let sym' = symbolTable orExpr'
  return $ ParseItem l s (CExclusiveOrExpression andExpr' orExpr') sym'

tExclusiveOrExpression' :: TypeAnnotate CExclusiveOrExpression'
tExclusiveOrExpression' (ParseItem l s CExclusiveOrExpression'Empty _) sym = do
  return $ ParseItem l s CExclusiveOrExpression'Empty sym
tExclusiveOrExpression' (ParseItem l s (CExclusiveOrExpression' andExpr orExpr) _) sym = do
  andExpr' <- tAndExpression andExpr sym
  orExpr' <- tExclusiveOrExpression' orExpr $ symbolTable andExpr'
  let sym' = symbolTable orExpr'
  return $ ParseItem l s (CExclusiveOrExpression' andExpr' orExpr') sym'

tAndExpression :: TypeAnnotate CAndExpression
tAndExpression (ParseItem l s (CAndExpression eqExpr andExpr) _) sym = do
  eqExpr' <- tEqualityExpression eqExpr sym
  andExpr' <- tAndExpression' andExpr $ symbolTable eqExpr'
  let sym' = symbolTable andExpr'
  return $ ParseItem l s (CAndExpression eqExpr' andExpr') sym'

tAndExpression' :: TypeAnnotate CAndExpression'
tAndExpression' (ParseItem l s CAndExpression'Empty _) sym =
  return $ ParseItem l s CAndExpression'Empty sym
tAndExpression' (ParseItem l s (CAndExpression' eqExpr andExpr) _) sym = do
  eqExpr' <- tEqualityExpression eqExpr sym
  andExpr' <- tAndExpression' andExpr $ symbolTable eqExpr'
  let sym' = symbolTable andExpr'
  return $ ParseItem l s (CAndExpression' eqExpr' andExpr') sym'

tEqualityExpression :: TypeAnnotate CEqualityExpression
tEqualityExpression (ParseItem l s (CEqualityExpression relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp $ symbolTable relExp'
  let sym' = symbolTable eqExp'
  return $ ParseItem l s (CEqualityExpression relExp' eqExp') sym'

tEqualityExpression' :: TypeAnnotate CEqualityExpression'
tEqualityExpression' (ParseItem l s CEqualityExpression'Empty _) sym =
  return $ ParseItem l s CEqualityExpression'Empty sym
tEqualityExpression' (ParseItem l s (CEqualityExpression'EQ relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp $ symbolTable relExp'
  let sym' = symbolTable eqExp'
  return $ ParseItem l s (CEqualityExpression'EQ relExp' eqExp') sym'
tEqualityExpression' (ParseItem l s (CEqualityExpression'NEQ relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp $ symbolTable relExp'
  let sym' = symbolTable eqExp'
  return $ ParseItem l s (CEqualityExpression'NEQ relExp' eqExp') sym'

tRelationalExpression :: TypeAnnotate CRelationalExpression
tRelationalExpression (ParseItem l s (CRelationalExpression shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  let sym' = symbolTable relExp'
  return $ ParseItem l s (CRelationalExpression shiftExp' relExp') sym'

tRelationalExpression' :: TypeAnnotate CRelationalExpression'
tRelationalExpression' (ParseItem l s CRelationalExpression'Empty _) sym =
  return $ ParseItem l s CRelationalExpression'Empty sym
tRelationalExpression' (ParseItem l s (CRelationalExpression'LT shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  let sym' = symbolTable relExp'
  return $ ParseItem l s (CRelationalExpression'LT shiftExp' relExp') sym'
tRelationalExpression' (ParseItem l s (CRelationalExpression'LTE shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  let sym' = symbolTable relExp'
  return $ ParseItem l s (CRelationalExpression'LTE shiftExp' relExp') sym'
tRelationalExpression' (ParseItem l s (CRelationalExpression'GT shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  let sym' = symbolTable relExp'
  return $ ParseItem l s (CRelationalExpression'GT shiftExp' relExp') sym'
tRelationalExpression' (ParseItem l s (CRelationalExpression'GTE shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  let sym' = symbolTable relExp'
  return $ ParseItem l s (CRelationalExpression'GTE shiftExp' relExp') sym'

tShiftExpression :: TypeAnnotate CShiftExpression
tShiftExpression (ParseItem l s (CShiftExpression addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp $ symbolTable addExp'
  let sym' = symbolTable shiftExp'
  return $ ParseItem l s (CShiftExpression addExp' shiftExp') sym'

tShiftExpression' :: TypeAnnotate CShiftExpression'
tShiftExpression' (ParseItem l s CShiftExpression'Empty _) sym =
  return $ ParseItem l s CShiftExpression'Empty sym
tShiftExpression' (ParseItem l s (CShiftExpression'Right addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp $ symbolTable addExp'
  let sym' = symbolTable shiftExp'
  return $ ParseItem l s (CShiftExpression'Right addExp' shiftExp') sym'
tShiftExpression' (ParseItem l s (CShiftExpression'Left addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp $ symbolTable addExp'
  let sym' = symbolTable shiftExp'
  return $ ParseItem l s (CShiftExpression'Left addExp' shiftExp') sym'

tAdditiveExpression :: TypeAnnotate CAdditiveExpression
tAdditiveExpression (ParseItem l s (CAdditiveExpression multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp $ symbolTable multExp'
  let sym' = symbolTable addExp'
  return $ ParseItem l s (CAdditiveExpression multExp' addExp') sym'

tAdditiveExpression' :: TypeAnnotate CAdditiveExpression'
tAdditiveExpression' (ParseItem l s CAdditiveExpression'Empty _) sym =
  return $ ParseItem l s CAdditiveExpression'Empty sym
tAdditiveExpression' (ParseItem l s (CAdditiveExpression'Sub multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp $ symbolTable multExp'
  let sym' = symbolTable addExp'
  return $ ParseItem l s (CAdditiveExpression'Sub multExp' addExp') sym'
tAdditiveExpression' (ParseItem l s (CAdditiveExpression'Add multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp $ symbolTable multExp'
  let sym' = symbolTable addExp'
  return $ ParseItem l s (CAdditiveExpression'Add multExp' addExp') sym'

tMultiplicativeExpression :: TypeAnnotate CMultiplicativeExpression
tMultiplicativeExpression (ParseItem l s (CMultiplicativeExpression castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  let sym' = symbolTable multExp'
  return $ ParseItem l s (CMultiplicativeExpression castExp' multExp') sym'

tMultiplicativeExpression' :: TypeAnnotate CMultiplicativeExpression'
tMultiplicativeExpression' (ParseItem l s CMultiplicativeExpression'Empty _) sym =
  return $ ParseItem l s CMultiplicativeExpression'Empty sym
tMultiplicativeExpression' (ParseItem l s (CMultiplicativeExpression'Mul castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  let sym' = symbolTable multExp'
  return $ ParseItem l s (CMultiplicativeExpression'Mul castExp' multExp') sym'
tMultiplicativeExpression' (ParseItem l s (CMultiplicativeExpression'Div castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  let sym' = symbolTable multExp'
  return $ ParseItem l s (CMultiplicativeExpression'Div castExp' multExp') sym'
tMultiplicativeExpression' (ParseItem l s (CMultiplicativeExpression'Mod castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  let sym' = symbolTable multExp'
  return $ ParseItem l s (CMultiplicativeExpression'Mod castExp' multExp') sym'

tCastExpression :: TypeAnnotate CCastExpression
tCastExpression (ParseItem l s (CCastExpressionUnary expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CCastExpressionUnary expr') sym'
tCastExpression (ParseItem l s (CCastExpression typeName expr) _) sym = do
  typeName' <- tTypeName typeName sym
  expr' <- tCastExpression expr $ symbolTable typeName'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CCastExpression typeName' expr') sym'

tUnaryExpression :: TypeAnnotate CUnaryExpression
tUnaryExpression (ParseItem l s (CUnaryExpressionPostfix expr) _) sym = do
  expr' <- tPostfixExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CUnaryExpressionPostfix expr') sym'
tUnaryExpression (ParseItem l s (CUnaryExpressionInc expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CUnaryExpressionInc expr') sym'
tUnaryExpression (ParseItem l s (CUnaryExpressionDec expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CUnaryExpressionDec expr') sym'
tUnaryExpression (ParseItem l s (CUnaryExpressionUnaryOp op expr) _) sym = do
  op' <- tUnaryOperator op sym
  expr' <- tCastExpression expr $ symbolTable op'
  let sym' = symbolTable expr'
  return $ ParseItem l s (CUnaryExpressionUnaryOp op' expr') sym'
tUnaryExpression (ParseItem l s (CUnaryExpressionSizeof expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CUnaryExpressionSizeof expr') sym'
tUnaryExpression (ParseItem l s (CUnaryExpressionSizeofType t) _) sym = do
  t' <- tTypeName t sym
  let sym' = symbolTable t'
  return $ ParseItem l s (CUnaryExpressionSizeofType t') sym'

tUnaryOperator :: TypeAnnotate CUnaryOperator
tUnaryOperator (ParseItem l s op _) sym =
  return $ ParseItem l s op sym

tPostfixExpression :: TypeAnnotate CPostfixExpression
tPostfixExpression (ParseItem l s (CPostfixExpression primaryExp postfixExp) _) sym = do
  primaryExp' <- tPrimaryExpression primaryExp sym
  postfixExp' <- tPostfixExpression' postfixExp $ symbolTable primaryExp'
  let sym' = symbolTable postfixExp'
  return $ ParseItem l s (CPostfixExpression primaryExp' postfixExp') sym'

tPostfixExpression' :: TypeAnnotate CPostfixExpression'
tPostfixExpression' (ParseItem l s CPostfixExpression'Empty _) sym =
  return $ ParseItem l s CPostfixExpression'Empty sym
tPostfixExpression' (ParseItem l s (CPostfixExpression'Bracket expr postfix) _) sym = do
  expr' <- tExpression expr sym
  postfix' <- tPostfixExpression' postfix $ symbolTable expr'
  let sym' = symbolTable postfix'
  return $ ParseItem l s (CPostfixExpression'Bracket expr' postfix') sym'
tPostfixExpression' (ParseItem l s (CPostfixExpression'Paren exprList postfix) _) sym = do
  exprList' <- tArgumentExpressionListOptional exprList sym
  postfix' <- tPostfixExpression' postfix $ symbolTable exprList'
  let sym' = symbolTable postfix'
  return $ ParseItem l s (CPostfixExpression'Paren exprList' postfix') sym'
tPostfixExpression' (ParseItem l s (CPostfixExpression'Dot identifier postfix) _) sym = do
  identifier' <- tIdentifier identifier sym
  postfix' <- tPostfixExpression' postfix $ symbolTable identifier'
  let sym' = symbolTable postfix'
  return $ ParseItem l s (CPostfixExpression'Dot identifier' postfix') sym'
tPostfixExpression' (ParseItem l s (CPostfixExpression'Arrow identifier postfix) _) sym = do
  identifier' <- tIdentifier identifier sym
  postfix' <- tPostfixExpression' postfix $ symbolTable identifier'
  let sym' = symbolTable postfix'
  return $ ParseItem l s (CPostfixExpression'Arrow identifier' postfix') sym'
tPostfixExpression' (ParseItem l s (CPostfixExpression'Inc postfix) _) sym = do
  postfix' <- tPostfixExpression' postfix sym
  let sym' = symbolTable postfix'
  return $ ParseItem l s (CPostfixExpression'Inc postfix') sym'
tPostfixExpression' (ParseItem l s (CPostfixExpression'Dec postfix) _) sym = do
  postfix' <- tPostfixExpression' postfix sym
  let sym' = symbolTable postfix'
  return $ ParseItem l s (CPostfixExpression'Dec postfix') sym'

tPrimaryExpression :: TypeAnnotate CPrimaryExpression
tPrimaryExpression (ParseItem l s (CPrimaryExpressionParen expr) _) sym = do
  expr' <- tExpression expr sym
  let sym' = symbolTable expr'
  return $ ParseItem l s (CPrimaryExpressionParen expr') sym'
tPrimaryExpression (ParseItem l s item _) sym =
  return $ ParseItem l s item sym

tArgumentExpressionList :: TypeAnnotate CArgumentExpressionList
tArgumentExpressionList (ParseItem l s (CArgumentExpressionList expr exprList) _) sym = do
  expr' <- tAssignmentExpression expr sym
  exprList' <- tArgumentExpressionList' exprList $ symbolTable expr'
  let sym' = symbolTable exprList'
  return $ ParseItem l s (CArgumentExpressionList expr' exprList') sym'

tArgumentExpressionListOptional :: TypeAnnotate CArgumentExpressionListOptional
tArgumentExpressionListOptional =
  tOptional
    CArgumentExpressionListOptionalEmpty
    CArgumentExpressionListOptional
    (\(CArgumentExpressionListOptional x) -> x)
    tArgumentExpressionList

tArgumentExpressionList' :: TypeAnnotate CArgumentExpressionList'
tArgumentExpressionList' (ParseItem l s CArgumentExpressionList'Empty _) sym =
  return $ ParseItem l s CArgumentExpressionList'Empty sym
tArgumentExpressionList' (ParseItem l s (CArgumentExpressionList' expr exprList) _) sym = do
  expr' <- tAssignmentExpression expr sym
  exprList' <- tArgumentExpressionList' exprList $ symbolTable expr'
  let sym' = symbolTable exprList'
  return $ ParseItem l s (CArgumentExpressionList' expr' exprList') sym'

tConstant :: TypeAnnotate CConstant
tConstant (ParseItem l s c _) sym =
  return $ ParseItem l s c sym

