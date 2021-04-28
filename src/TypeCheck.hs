module TypeCheck where

import Control.Monad
import qualified Data.Map as M

import ParseItem
import Utils ( Error(..)
             , Coordinates
             , DataType(..)
             , SymbolTable(..)
             , CType(..)
             )
import Symbols ( TypeCheckItem(..)
               , readFunctionDefinition
               , readDeclaration
               )

type TypeAnnotate a
  = ParseItem a -> SymbolTable -> Either Error (ParseItem a)

typeCheck ::
     ParseItem CTranslationUnit
  -> Either Error (ParseItem CTranslationUnit)
typeCheck item = tTranslationUnit item initialSymbols

emptyType :: CType
emptyType =
  CType
    { storageClass = []
    , typeQualifier = []
    , dataType = TVoid
    }

childSymbols :: SymbolTable -> SymbolTable
childSymbols sym =
  SymbolTable
    { typedef = M.empty
    , labels = M.empty
    , symbols = M.empty
    , structured = M.empty
    , parent = Just sym
    }

addSymbol ::
     String
  -> CType
  -> SymbolTable
  -> Coordinates
  -> Either Error SymbolTable
addSymbol label t sym@(SymbolTable { symbols = symTable }) c =
  case M.lookup label symTable of
    Nothing -> return $ sym { symbols = M.insert label t symTable }
    Just _ -> Left $ TypeError c $ "Name clash: " ++ label

addLabel ::
     String
  -> Coordinates
  -> SymbolTable
  -> Either Error SymbolTable
addLabel label c sym@(SymbolTable { labels = labelTable }) =
  case M.lookup label labelTable of
    Nothing -> return $ sym { labels = M.insert label c labelTable }
    Just _ -> Left $ TypeError c $ "Name clash: " ++ label

tOptional ::
     Eq a => a
  -> (ParseItem b -> a)
  -> (a -> ParseItem b)
  -> TypeAnnotate b
  -> TypeAnnotate a
tOptional empty nonempty extr tSub item@(ParseItem l i _) sym
  | i == empty = Right $ item { symbolTable = sym }
  | otherwise  =
      do
        subItem <- tSub (extr i) sym
        return $ ParseItem
          { parseLoc = l
          , parseItem = nonempty subItem
          , symbolTable = sym
          }

tIdentifier :: TypeAnnotate CIdentifier
tIdentifier item sym = Right $ item { symbolTable = sym }

tIdentifierOptional :: TypeAnnotate CIdentifierOptional
tIdentifierOptional =
  tOptional
    CIdentifierOptionalEmpty
    CIdentifierOptional
    (\(CIdentifierOptional x) -> x)
    tIdentifier

tTranslationUnit :: TypeAnnotate CTranslationUnit
tTranslationUnit (ParseItem l (CTranslationUnit ext opt) _) sym = do
  ext' <- tExternalDeclaration ext sym
  opt' <- tTranslationUnitOptional opt (symbolTable ext')
  return $ ParseItem l (CTranslationUnit ext' opt') $ symbolTable opt'

tTranslationUnitOptional :: TypeAnnotate CTranslationUnitOptional
tTranslationUnitOptional =
  tOptional
    CTranslationUnitOptionalEmpty
    CTranslationUnitOptional
    (\(CTranslationUnitOptional x) -> x)
    tTranslationUnit

tExternalDeclaration :: TypeAnnotate CExternalDeclaration
tExternalDeclaration (ParseItem l (CExternalDeclarationFunction fd) _) sym = do
  fd' <- tFunctionDefinition fd sym
  return $ ParseItem l (CExternalDeclarationFunction fd') $ symbolTable fd'
tExternalDeclaration (ParseItem l (CExternalDeclaration decl) _) sym = do
  decl' <- tDeclaration decl sym
  return $ ParseItem l (CExternalDeclaration decl') $ symbolTable decl'

tFunctionDefinition :: TypeAnnotate CFunctionDefinition
tFunctionDefinition
     (ParseItem { parseLoc = l
                , parseItem = fd@(CFunctionDefinition spec decl declList compStatement)
                })
     sym = do
  (fName, fType, argNames) <-
    readFunctionDefinition $ TypeCheckItem { typeCheckLoc = l
                                           , typeCheckSymbols = sym
                                           , typeCheckItem = fd
                                           , previousType = emptyType
                                           }

  sym'           <- addSymbol fName fType sym l
  spec'          <- tDeclarationSpecifiersOptional spec sym'
  decl'          <- tDeclarator decl sym'
  declList'      <- tDeclarationListOptional declList sym'
  let sym'' = SymbolTable
                { typedef = M.empty
                , labels  = M.empty
                , symbols = M.fromList $ zip argNames argTypes
                , structured = M.empty
                , parent  = Just sym'
                }
              where (TFunction _ _ argTypes _) = dataType fType
  compStatement' <- tCompoundStatement compStatement sym''
  return $
    ParseItem
      l
      (CFunctionDefinition spec' decl' declList' compStatement')
      sym'

tDeclaration :: TypeAnnotate CDeclaration
tDeclaration (ParseItem l item@(CDeclaration spec initList) _) sym = do
  declarations <-
    readDeclaration $ TypeCheckItem { typeCheckLoc = l
                                    , typeCheckSymbols = sym
                                    , typeCheckItem = item
                                    , previousType = emptyType
                                    }
  sym' <- foldM
            (\s (_, label, t) -> addSymbol label t s l)
            sym
            (snd declarations)
  spec' <- tDeclarationSpecifiers spec sym'
  initList' <- tInitDeclaratorListOptional initList sym'
  return $ ParseItem l (CDeclaration spec' initList') sym'

tDeclarationList :: TypeAnnotate CDeclarationList
tDeclarationList (ParseItem l (CDeclarationList decl opt) _) sym = do
  decl' <- tDeclaration decl sym
  opt' <- tDeclarationListOptional opt $ symbolTable decl'
  return $ ParseItem l (CDeclarationList decl' opt') $ symbolTable opt'

tDeclarationListOptional :: TypeAnnotate CDeclarationListOptional
tDeclarationListOptional =
  tOptional
    CDeclarationListOptionalEmpty
    CDeclarationListOptional
    (\(CDeclarationListOptional x) -> x)
    tDeclarationList

tDeclarationSpecifiers :: TypeAnnotate CDeclarationSpecifiers
tDeclarationSpecifiers (ParseItem l (CDeclarationSpecifiersStorageClass spec opt) _) sym = do
  spec' <- tStorageClassSpecifier spec sym
  opt' <- tDeclarationSpecifiersOptional opt sym
  return $ ParseItem l (CDeclarationSpecifiersStorageClass spec' opt') sym
tDeclarationSpecifiers (ParseItem l (CDeclarationSpecifiersTypeSpecifier spec opt) _) sym = do
  spec' <- tTypeSpecifier spec sym
  opt' <- tDeclarationSpecifiersOptional opt sym
  return $ ParseItem l (CDeclarationSpecifiersTypeSpecifier spec' opt') sym
tDeclarationSpecifiers (ParseItem l (CDeclarationSpecifiersTypeQualifier spec opt) _) sym = do
  spec' <- tTypeQualifier spec sym
  opt' <- tDeclarationSpecifiersOptional opt sym
  return $ ParseItem l (CDeclarationSpecifiersTypeQualifier spec' opt') sym

tDeclarationSpecifiersOptional :: TypeAnnotate CDeclarationSpecifiersOptional
tDeclarationSpecifiersOptional =
  tOptional
    CDeclarationSpecifiersOptionalEmpty
    CDeclarationSpecifiersOptional
    (\(CDeclarationSpecifiersOptional x) -> x)
    tDeclarationSpecifiers

tStorageClassSpecifier :: TypeAnnotate CStorageClassSpecifier
tStorageClassSpecifier item sym = Right $ item { symbolTable = sym }

tTypeSpecifier :: TypeAnnotate CTypeSpecifier
tTypeSpecifier (ParseItem l (CTypeSpecifierStructOrUnion spec) _) sym = do
  spec' <- tStructOrUnionSpecifier spec sym
  return $ ParseItem l (CTypeSpecifierStructOrUnion spec') sym
tTypeSpecifier (ParseItem l (CTypeSpecifierEnum spec) _) sym = do
  spec' <- tEnumSpecifier spec sym
  return $ ParseItem l (CTypeSpecifierEnum spec') sym
tTypeSpecifier (ParseItem l (CTypeSpecifierTypedef spec) _) sym = do
  spec' <- tTypedefName spec sym
  return $ ParseItem l (CTypeSpecifierTypedef spec') sym
tTypeSpecifier item sym = Right $ item { symbolTable = sym }

tTypeQualifier :: TypeAnnotate CTypeQualifier
tTypeQualifier item sym = Right $ item { symbolTable = sym }

tStructOrUnionSpecifier :: TypeAnnotate CStructOrUnionSpecifier
tStructOrUnionSpecifier (ParseItem l (CStructOrUnionSpecifierList structOrUnion idOpt declList) _) sym = do
  su' <- tStructOrUnion structOrUnion sym
  i' <- tIdentifierOptional idOpt sym
  decl' <- tStructDeclarationList declList sym
  return $ ParseItem l (CStructOrUnionSpecifierList su' i' decl') $ symbolTable decl'
tStructOrUnionSpecifier (ParseItem l (CStructOrUnionSpecifier su i) _) sym = do
  su' <- tStructOrUnion su sym
  i' <- tIdentifier i sym
  return $ ParseItem l (CStructOrUnionSpecifier su' i') sym

tStructOrUnion :: TypeAnnotate CStructOrUnion
tStructOrUnion item sym = Right $ item { symbolTable = sym }

tStructDeclarationList :: TypeAnnotate CStructDeclarationList
tStructDeclarationList (ParseItem l (CStructDeclarationList decl lst) _) sym = do
  decl' <- tStructDeclaration decl sym
  lst' <- tStructDeclarationListOptional lst sym
  return $ ParseItem l (CStructDeclarationList decl' lst') sym

tStructDeclarationListOptional :: TypeAnnotate CStructDeclarationListOptional
tStructDeclarationListOptional =
  tOptional
    CStructDeclarationListOptionalEmpty
    CStructDeclarationListOptional
    (\(CStructDeclarationListOptional x) -> x)
    tStructDeclarationList

tInitDeclaratorList :: TypeAnnotate CInitDeclaratorList
tInitDeclaratorList (ParseItem l (CInitDeclaratorList decl lst) _) sym = do
  decl' <- tInitDeclarator decl sym
  lst' <- tInitDeclaratorList' lst sym
  return $ ParseItem l (CInitDeclaratorList decl' lst') sym

tInitDeclaratorListOptional :: TypeAnnotate CInitDeclaratorListOptional
tInitDeclaratorListOptional =
  tOptional
    CInitDeclaratorListOptionalEmpty
    CInitDeclaratorListOptional
    (\(CInitDeclaratorListOptional x) -> x)
    tInitDeclaratorList

tInitDeclaratorList' :: TypeAnnotate CInitDeclaratorList'
tInitDeclaratorList' (ParseItem l (CInitDeclaratorList' decl lst) _) sym = do
  decl' <- tInitDeclarator decl sym
  lst' <- tInitDeclaratorList' lst sym
  return $ ParseItem l (CInitDeclaratorList' decl' lst') sym
tInitDeclaratorList' item sym = Right $ item { symbolTable = sym }

tInitDeclarator :: TypeAnnotate CInitDeclarator
tInitDeclarator (ParseItem l (CInitDeclarator decl initializer) _) sym = do
  decl' <- tDeclarator decl sym
  initializer' <- tAssignInitializerOptional initializer sym
  return $ ParseItem l (CInitDeclarator decl' initializer') sym

tAssignInitializerOptional :: TypeAnnotate CAssignInitializerOptional
tAssignInitializerOptional =
  tOptional
    CAssignInitializerOptionalEmpty
    CAssignInitializerOptional
    (\(CAssignInitializerOptional x) -> x)
    tInitializer

tStructDeclaration :: TypeAnnotate CStructDeclaration
tStructDeclaration (ParseItem l (CStructDeclaration spec decl) _) sym = do
  spec' <- tSpecifierQualifierList spec sym
  decl' <- tStructDeclaratorList decl sym
  return $ ParseItem l (CStructDeclaration spec' decl') sym

tSpecifierQualifierList :: TypeAnnotate CSpecifierQualifierList
tSpecifierQualifierList (ParseItem l (CSpecifierQualifierListSpecifier spec lst) _) sym = do
  spec' <- tTypeSpecifier spec sym
  lst' <- tSpecifierQualifierListOptional lst sym
  return $ ParseItem l (CSpecifierQualifierListSpecifier spec' lst') sym
tSpecifierQualifierList (ParseItem l (CSpecifierQualifierListQualifier qualifier lst) _) sym = do
  qualifier' <- tTypeQualifier qualifier sym
  lst' <- tSpecifierQualifierListOptional lst sym
  return $ ParseItem l (CSpecifierQualifierListQualifier qualifier' lst') sym

tSpecifierQualifierListOptional :: TypeAnnotate CSpecifierQualifierListOptional
tSpecifierQualifierListOptional =
  tOptional
    CSpecifierQualifierListOptionalEmpty
    CSpecifierQualifierListOptional
    (\(CSpecifierQualifierListOptional x) -> x)
    tSpecifierQualifierList

tStructDeclaratorList :: TypeAnnotate CStructDeclaratorList
tStructDeclaratorList (ParseItem l (CStructDeclaratorList decl lst) _) sym = do
  decl' <- tStructDeclarator decl sym
  lst' <- tStructDeclaratorList' lst sym
  return $ ParseItem l (CStructDeclaratorList decl' lst') sym

tStructDeclaratorList' :: TypeAnnotate CStructDeclaratorList'
tStructDeclaratorList' (ParseItem l (CStructDeclaratorList' decl lst) _) sym = do
  decl' <- tStructDeclarator decl sym
  lst' <- tStructDeclaratorList' lst sym
  return $ ParseItem l (CStructDeclaratorList' decl' lst') sym
tStructDeclaratorList' item sym = Right $ item { symbolTable = sym }

tStructDeclarator :: TypeAnnotate CStructDeclarator
tStructDeclarator (ParseItem l (CStructDeclarator decl) _) sym = do
  decl' <- tDeclarator decl sym
  return $ ParseItem l (CStructDeclarator decl') sym
tStructDeclarator (ParseItem l (CStructDeclaratorField decl expr) _) sym = do
  decl' <- tDeclaratorOptional decl sym
  expr' <- tConstantExpression expr sym
  return $ ParseItem l (CStructDeclaratorField decl' expr') sym

tEnumSpecifier :: TypeAnnotate CEnumSpecifier
tEnumSpecifier (ParseItem l (CEnumSpecifierList identifier lst) _) sym = do
  identifier' <- tIdentifierOptional identifier sym
  lst' <- tEnumeratorList lst sym
  return $ ParseItem l (CEnumSpecifierList identifier' lst') sym
tEnumSpecifier (ParseItem l (CEnumSpecifier identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return $ ParseItem l (CEnumSpecifier identifier') sym

tEnumeratorList :: TypeAnnotate CEnumeratorList
tEnumeratorList (ParseItem l (CEnumeratorList enum lst) _) sym = do
  enum' <- tEnumerator enum sym
  lst' <- tEnumeratorList' lst sym
  return $ ParseItem l (CEnumeratorList enum' lst') sym

tEnumeratorList' :: TypeAnnotate CEnumeratorList'
tEnumeratorList' (ParseItem l (CEnumeratorList' enum lst) _) sym = do
  enum' <- tEnumerator enum sym
  lst' <- tEnumeratorList' lst sym
  return $ ParseItem l (CEnumeratorList' enum' lst') sym
tEnumeratorList' item sym = Right $ item { symbolTable = sym }

tEnumerator :: TypeAnnotate CEnumerator
tEnumerator (ParseItem l (CEnumerator identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return $ ParseItem l (CEnumerator identifier') sym
tEnumerator (ParseItem l (CEnumeratorAssign identifier expr) _) sym = do
  identifier' <- tIdentifier identifier sym
  expr' <- tConstantExpression expr sym
  return $ ParseItem l (CEnumeratorAssign identifier' expr') sym

tDeclarator :: TypeAnnotate CDeclarator
tDeclarator (ParseItem l (CDeclarator pointer decl) _) sym = do
  pointer' <- tPointerOptional pointer sym
  decl' <- tDirectDeclarator decl sym
  return $ ParseItem l (CDeclarator pointer' decl') sym

tDeclaratorOptional :: TypeAnnotate CDeclaratorOptional
tDeclaratorOptional =
  tOptional
    CDeclaratorOptionalEmpty
    CDeclaratorOptional
    (\(CDeclaratorOptional x) -> x)
    tDeclarator

tDirectDeclarator :: TypeAnnotate CDirectDeclarator
tDirectDeclarator (ParseItem l (CDirectDeclaratorId identifier decl) _) sym = do
  identifier' <- tIdentifier identifier sym
  decl' <- tDirectDeclarator' decl sym
  return $ ParseItem l (CDirectDeclaratorId identifier' decl') sym
tDirectDeclarator (ParseItem l (CDirectDeclaratorParen decl directDecl) _) sym = do
  decl' <- tDeclarator decl sym
  directDecl' <- tDirectDeclarator' directDecl sym
  return $ ParseItem l (CDirectDeclaratorParen decl' directDecl') sym

tDirectDeclarator' :: TypeAnnotate CDirectDeclarator'
tDirectDeclarator' (ParseItem l (CDirectDeclarator'ConstExpr expr decl) _) sym = do
  expr' <- tConstantExpressionOptional expr sym
  decl' <- tDirectDeclarator' decl sym
  return $ ParseItem l (CDirectDeclarator'ConstExpr expr' decl') sym
tDirectDeclarator' (ParseItem l (CDirectDeclarator'ParamTypeList lst decl) _) sym = do
  lst' <- tParameterTypeList lst sym
  decl' <- tDirectDeclarator' decl sym
  return $ ParseItem l (CDirectDeclarator'ParamTypeList lst' decl') sym
tDirectDeclarator' (ParseItem l (CDirectDeclarator'IdList lst decl) _) sym = do
  lst' <- tIdentifierListOptional lst sym
  decl' <- tDirectDeclarator' decl sym
  return $ ParseItem l (CDirectDeclarator'IdList lst' decl') sym
tDirectDeclarator' item sym = Right $ item { symbolTable = sym }

tPointer :: TypeAnnotate CPointer
tPointer (ParseItem l (CPointer lst pointer) _) sym = do
  lst' <- tTypeQualifierListOptional lst sym
  pointer' <- tPointerOptional pointer sym
  return $ ParseItem l (CPointer lst' pointer') sym

tPointerOptional :: TypeAnnotate CPointerOptional
tPointerOptional =
  tOptional
    CPointerOptionalEmpty
    CPointerOptional
    (\(CPointerOptional x) -> x)
    tPointer

tTypeQualifierList :: TypeAnnotate CTypeQualifierList
tTypeQualifierList (ParseItem l (CTypeQualifierList qualifier lst) _) sym = do
  qualifier' <- tTypeQualifier qualifier sym
  lst' <- tTypeQualifierListOptional lst sym
  return $ ParseItem l (CTypeQualifierList qualifier' lst') sym

tTypeQualifierListOptional :: TypeAnnotate CTypeQualifierListOptional
tTypeQualifierListOptional =
  tOptional
    CTypeQualifierListOptionalEmpty
    CTypeQualifierListOptional
    (\(CTypeQualifierListOptional x) -> x)
    tTypeQualifierList

tParameterTypeList :: TypeAnnotate CParameterTypeList
tParameterTypeList (ParseItem l (CParameterTypeList lst varargs) _) sym = do
  lst' <- tParameterList lst sym
  varargs' <- tVarArgsOptional varargs sym
  return $ ParseItem l (CParameterTypeList lst' varargs') sym

tParameterTypeListOptional :: TypeAnnotate CParameterTypeListOptional
tParameterTypeListOptional =
  tOptional
    CParameterTypeListOptionalEmpty
    CParameterTypeListOptional
    (\(CParameterTypeListOptional x) -> x)
    tParameterTypeList

tVarArgsOptional :: TypeAnnotate CVarArgsOptional
tVarArgsOptional item sym = Right $ item { symbolTable = sym }

tParameterList :: TypeAnnotate CParameterList
tParameterList (ParseItem l (CParameterList decl lst) _) sym = do
  decl' <- tParameterDeclaration decl sym
  lst' <- tParameterList' lst sym
  return $ ParseItem l (CParameterList decl' lst') sym

tParameterList' :: TypeAnnotate CParameterList'
tParameterList' (ParseItem l (CParameterList' decl lst) _) sym = do
  decl' <- tParameterDeclaration decl sym
  lst' <- tParameterList' lst sym
  return $ ParseItem l (CParameterList' decl' lst') sym
tParameterList' item sym = Right $ item { symbolTable = sym }

tParameterDeclaration :: TypeAnnotate CParameterDeclaration
tParameterDeclaration (ParseItem l (CParameterDeclaration spec decl) _) sym = do
  spec' <- tDeclarationSpecifiers spec sym
  decl' <- tParameterDeclaration' decl sym
  return $ ParseItem l (CParameterDeclaration spec' decl') sym

tParameterDeclaration' :: TypeAnnotate CParameterDeclaration'
tParameterDeclaration' (ParseItem l (CParameterDeclaration' decl) _) sym = do
  decl' <- tDeclarator decl sym
  return $ ParseItem l (CParameterDeclaration' decl') sym
tParameterDeclaration' (ParseItem l (CParameterDeclaration'Abstract decl) _) sym = do
  decl' <- tAbstractDeclaratorOptional decl sym
  return $ ParseItem l (CParameterDeclaration'Abstract decl') sym

tIdentifierList :: TypeAnnotate CIdentifierList
tIdentifierList (ParseItem l (CIdentifierList identifier lst) _) sym = do
  identifier' <- tIdentifier identifier sym
  lst' <- tIdentifierList' lst sym
  return $ ParseItem l (CIdentifierList identifier' lst') sym

tIdentifierListOptional :: TypeAnnotate CIdentifierListOptional
tIdentifierListOptional =
  tOptional
    CIdentifierListOptionalEmpty
    CIdentifierListOptional
    (\(CIdentifierListOptional x) -> x)
    tIdentifierList

tIdentifierList' :: TypeAnnotate CIdentifierList'
tIdentifierList' (ParseItem l (CIdentifierList' identifier lst) _) sym = do
  identifier' <- tIdentifier identifier sym
  lst' <- tIdentifierList' lst sym
  return $ ParseItem l (CIdentifierList' identifier' lst') sym
tIdentifierList' item sym = Right $ item { symbolTable = sym }

tInitializer :: TypeAnnotate CInitializer
tInitializer (ParseItem l (CInitializerAssignment expr) _) sym = do
  expr' <- tAssignmentExpression expr sym
  return $ ParseItem l (CInitializerAssignment expr') sym
tInitializer (ParseItem l (CInitializerInitList lst) _) sym = do
  lst' <- tInitializerList lst sym
  return $ ParseItem l (CInitializerInitList lst') sym

tInitializerList :: TypeAnnotate CInitializerList
tInitializerList (ParseItem l (CInitializerList initializer lst) _) sym = do
  initializer' <- tInitializer initializer sym
  lst' <- tInitializerList' lst sym
  return $ ParseItem l (CInitializerList initializer' lst') sym

tInitializerList' :: TypeAnnotate CInitializerList'
tInitializerList' (ParseItem l (CInitializerList' initializer lst) _) sym = do
  initializer' <- tInitializer initializer sym
  lst' <- tInitializerList' lst sym
  return $ ParseItem l (CInitializerList' initializer' lst') sym
tInitializerList' item sym = Right $ item { symbolTable = sym }

tTypeName :: TypeAnnotate CTypeName
tTypeName (ParseItem l (CTypeName lst decl) _) sym = do
  lst' <- tSpecifierQualifierList lst sym
  decl' <- tAbstractDeclaratorOptional decl sym
  return $ ParseItem l (CTypeName lst' decl') sym

tAbstractDeclarator :: TypeAnnotate CAbstractDeclarator
tAbstractDeclarator (ParseItem l (CAbstractDeclaratorPointer pointer) _) sym = do
  pointer' <- tPointer pointer sym
  return $ ParseItem l (CAbstractDeclaratorPointer pointer') sym
tAbstractDeclarator (ParseItem l (CAbstractDeclaratorDirect pointer decl) _) sym = do
  pointer' <- tPointerOptional pointer sym
  decl' <- tDirectAbstractDeclarator decl sym
  return $ ParseItem l (CAbstractDeclaratorDirect pointer' decl') sym

tAbstractDeclaratorOptional :: TypeAnnotate CAbstractDeclaratorOptional
tAbstractDeclaratorOptional =
  tOptional
    CAbstractDeclaratorOptionalEmpty
    CAbstractDeclaratorOptional
    (\(CAbstractDeclaratorOptional x) -> x)
    tAbstractDeclarator

tDirectAbstractDeclarator :: TypeAnnotate CDirectAbstractDeclarator
tDirectAbstractDeclarator (ParseItem l (CDirectAbstractDeclaratorParen decl rest) _) sym = do
  decl' <- tAbstractDeclarator decl sym
  rest' <- tDirectAbstractDeclarator' rest sym
  return $ ParseItem l (CDirectAbstractDeclaratorParen decl' rest') sym
tDirectAbstractDeclarator (ParseItem l (CDirectAbstractDeclaratorIndexed expr rest) _) sym = do
  expr' <- tConstantExpressionOptional expr sym
  rest' <- tDirectAbstractDeclarator' rest sym
  return $ ParseItem l (CDirectAbstractDeclaratorIndexed expr' rest') sym
tDirectAbstractDeclarator (ParseItem l (CDirectAbstractDeclaratorParams typeList rest) _) sym = do
  typeList' <- tParameterTypeListOptional typeList sym
  rest' <- tDirectAbstractDeclarator' rest sym
  return $ ParseItem l (CDirectAbstractDeclaratorParams typeList' rest') sym

tDirectAbstractDeclarator' :: TypeAnnotate CDirectAbstractDeclarator'
tDirectAbstractDeclarator' (ParseItem l (CDirectAbstractDeclarator'Const expr decl) _) sym = do
  expr' <- tConstantExpressionOptional expr sym
  decl' <- tDirectAbstractDeclarator' decl sym
  return $ ParseItem l (CDirectAbstractDeclarator'Const expr' decl') sym
tDirectAbstractDeclarator' (ParseItem l (CDirectAbstractDeclarator'Params lst decl) _) sym = do
  lst' <- tParameterTypeListOptional lst sym
  decl' <- tDirectAbstractDeclarator' decl sym
  return $ ParseItem l (CDirectAbstractDeclarator'Params lst' decl') sym
tDirectAbstractDeclarator' item sym = Right $ item { symbolTable = sym }

tTypedefName :: TypeAnnotate CTypedefName
tTypedefName (ParseItem l (CTypedefName identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return $ ParseItem l (CTypedefName identifier') sym

tStatement :: TypeAnnotate CStatement
tStatement (ParseItem l (CStatementLabeled statement) _) sym = do
  statement' <- tLabeledStatement statement sym
  return $ ParseItem l (CStatementLabeled statement') $ symbolTable statement'
tStatement (ParseItem l (CStatementExpression statement) _) sym = do
  statement' <- tExpressionStatement statement sym
  return $ ParseItem l (CStatementExpression statement') sym
tStatement (ParseItem l (CStatementCompound statement) _) sym = do
  statement' <- tCompoundStatement statement sym
  return $ ParseItem l (CStatementCompound statement') sym
tStatement (ParseItem l (CStatementSelection statement) _) sym = do
  statement' <- tSelectionStatement statement sym
  return $ ParseItem l (CStatementSelection statement') sym
tStatement (ParseItem l (CStatementIteration statement) _) sym = do
  statement' <- tIterationStatement statement sym
  return $ ParseItem l (CStatementIteration statement') sym
tStatement (ParseItem l (CStatementJump statement) _) sym = do
  statement' <- tJumpStatement statement sym
  return $ ParseItem l (CStatementJump statement') sym

tLabeledStatement :: TypeAnnotate CLabeledStatement
tLabeledStatement (ParseItem l (CLabeledStatementId identifier statement) _) sym = do
  identifier' <- tIdentifier identifier sym
  let CIdentifier i = parseItem identifier
  sym' <- addLabel i l sym
  statement' <- tStatement statement sym'
  return $ ParseItem l (CLabeledStatementId identifier' statement') $ symbolTable statement'
tLabeledStatement (ParseItem l (CLabeledStatementCase expr statement) _) sym = do
  expr' <- tConstantExpression expr sym
  statement' <- tStatement statement sym
  return $ ParseItem l (CLabeledStatementCase expr' statement') $ symbolTable statement'
tLabeledStatement (ParseItem l (CLabeledStatementDefault statement) _) sym = do
  statement' <- tStatement statement sym
  return $ ParseItem l (CLabeledStatementDefault statement') $ symbolTable statement'

tExpressionStatement :: TypeAnnotate CExpressionStatement
tExpressionStatement (ParseItem l (CExpressionStatement expr) _) sym = do
  expr' <- tExpressionOptional expr sym
  return $ ParseItem l (CExpressionStatement expr') $ symbolTable expr'

tCompoundStatement :: TypeAnnotate CCompoundStatement
tCompoundStatement (ParseItem l (CCompoundStatement decl statement) _) sym = do
  let sym' = childSymbols sym
  decl' <- tDeclarationListOptional decl sym'
  statement' <- tStatementListOptional statement $ symbolTable decl'
  return $ ParseItem l (CCompoundStatement decl' statement') sym

tStatementList :: TypeAnnotate CStatementList
tStatementList (ParseItem l (CStatementList statement list) _) sym = do
  statement' <- tStatement statement sym
  list' <- tStatementListOptional list $ symbolTable statement'
  return $ ParseItem l (CStatementList statement' list') $ symbolTable list'

tStatementListOptional :: TypeAnnotate CStatementListOptional
tStatementListOptional =
  tOptional
    CStatementListOptionalEmpty
    CStatementListOptional
    (\(CStatementListOptional x) -> x)
    tStatementList

tSelectionStatement :: TypeAnnotate CSelectionStatement
tSelectionStatement (ParseItem l (CSelectionStatementIf expr statement elseopt) _) sym = do
  let sym' = childSymbols sym
  expr' <- tExpression expr sym'
  statement' <- tStatement statement $ symbolTable expr'
  elseopt' <- tElseOptional elseopt $ symbolTable expr'
  return $ ParseItem l (CSelectionStatementIf expr' statement' elseopt') sym
tSelectionStatement (ParseItem l (CSelectionStatementSwitch expr statement) _) sym = do
  expr' <- tExpression expr sym
  let sym' = childSymbols sym
  statement' <- tStatement statement sym'
  return $ ParseItem l (CSelectionStatementSwitch expr' statement') sym

tElseOptional :: TypeAnnotate CElseOptional
tElseOptional =
  tOptional
    CElseOptionalEmpty
    CElseOptional
    (\(CElseOptional x) -> x)
    tStatement

tIterationStatement :: TypeAnnotate CIterationStatement
tIterationStatement (ParseItem l (CIterationStatementWhile expr statement) _) sym = do
  expr' <- tExpression expr sym
  let sym' = childSymbols sym
  statement' <- tStatement statement sym'
  return $ ParseItem l (CIterationStatementWhile expr' statement') sym
tIterationStatement (ParseItem l (CIterationStatementDoWhile statement expr) _) sym = do
  let sym' = childSymbols sym
  statement' <- tStatement statement sym'
  expr' <- tExpression expr sym'
  return $ ParseItem l (CIterationStatementDoWhile statement' expr') sym
tIterationStatement (ParseItem l (CIterationStatementFor decl incr cond body) _) sym = do
  let sym' = childSymbols sym
  decl' <- tExpressionOptional decl sym'
  incr' <- tExpressionOptional incr $ symbolTable decl'
  cond' <- tExpressionOptional cond $ symbolTable decl'
  body' <- tStatement body $ symbolTable decl'
  return $ ParseItem l (CIterationStatementFor decl' incr' cond' body') sym

tJumpStatement :: TypeAnnotate CJumpStatement
tJumpStatement (ParseItem l (CJumpStatementGoto identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return $ ParseItem l (CJumpStatementGoto identifier') sym
tJumpStatement (ParseItem l (CJumpStatementContinue) _) sym =
  return $ ParseItem l (CJumpStatementContinue) sym
tJumpStatement (ParseItem l (CJumpStatementBreak) _) sym =
  return $ ParseItem l (CJumpStatementBreak) sym
tJumpStatement (ParseItem l (CJumpStatementReturn expr) _) sym = do
  expr' <- tExpressionOptional expr sym
  return $ ParseItem l (CJumpStatementReturn expr') sym

tExpression :: TypeAnnotate CExpression
tExpression (ParseItem l (CExpression assign expr) _) sym = do
  assign' <- tAssignmentExpression assign sym
  expr' <- tExpression' expr $ symbolTable assign'
  return $ ParseItem l (CExpression assign' expr') $ symbolTable expr'

tExpressionOptional :: TypeAnnotate CExpressionOptional
tExpressionOptional =
  tOptional
    CExpressionOptionalEmpty
    CExpressionOptional
    (\(CExpressionOptional x) -> x)
    tExpression

tExpression' :: TypeAnnotate CExpression'
tExpression' (ParseItem l CExpression'Empty _) sym =
  return $ ParseItem l CExpression'Empty sym
tExpression' (ParseItem l (CExpression' assign expr) _) sym = do
  assign' <- tAssignmentExpression assign sym
  expr' <- tExpression' expr $ symbolTable assign'
  return $ ParseItem l (CExpression' assign' expr') $ symbolTable expr'

tAssignmentExpression :: TypeAnnotate CAssignmentExpression
tAssignmentExpression (ParseItem l (CAssignmentExpressionConditional expr) _) sym = do
  expr' <- tConditionalExpression expr sym
  return $ ParseItem l (CAssignmentExpressionConditional expr') $ symbolTable expr'
tAssignmentExpression (ParseItem l (CAssignmentExpression unary operator expr) _) sym = do
  unary' <- tUnaryExpression unary sym
  operator' <- tAssignmentOperator operator $ symbolTable unary'
  expr' <- tAssignmentExpression expr $ symbolTable unary'
  return $ ParseItem l (CAssignmentExpression unary' operator' expr') $ symbolTable expr'

tAssignmentOperator :: TypeAnnotate CAssignmentOperator
tAssignmentOperator item sym = return $ item { symbolTable = sym }

tConditionalExpression :: TypeAnnotate CConditionalExpression
tConditionalExpression (ParseItem l (CConditionalExpression expr ternary) _) sym = do
  expr' <- tLogicalOrExpression expr sym
  ternary' <- tTernaryOptional ternary $ symbolTable expr'
  return $ ParseItem l (CConditionalExpression expr' ternary') $ symbolTable expr'

tTernaryOptional :: TypeAnnotate CTernaryOptional
tTernaryOptional (ParseItem l CTernaryOptionalEmpty _) sym =
  return $ ParseItem l CTernaryOptionalEmpty sym
tTernaryOptional (ParseItem l (CTernaryOptional expr cond) _) sym = do
  expr' <- tExpression expr sym
  cond' <- tConditionalExpression cond $ symbolTable expr'
  return $ ParseItem l (CTernaryOptional expr' cond') $ symbolTable expr'

tConstantExpression :: TypeAnnotate CConstantExpression
tConstantExpression (ParseItem l (CConstantExpression expr) _) sym = do
  expr' <- tConditionalExpression expr sym
  return $ ParseItem l (CConstantExpression expr') sym

tConstantExpressionOptional :: TypeAnnotate CConstantExpressionOptional
tConstantExpressionOptional =
  tOptional
    CConstantExpressionOptionalEmpty
    CConstantExpressionOptional
    (\(CConstantExpressionOptional x) -> x)
    tConstantExpression

tLogicalOrExpression :: TypeAnnotate CLogicalOrExpression
tLogicalOrExpression (ParseItem l (CLogicalOrExpression andExpr orExpr) _) sym = do
  andExpr' <- tLogicalAndExpression andExpr sym
  orExpr' <- tLogicalOrExpression' orExpr $ symbolTable andExpr'
  return $ ParseItem l (CLogicalOrExpression andExpr' orExpr') $ symbolTable andExpr'

tLogicalOrExpression' :: TypeAnnotate CLogicalOrExpression'
tLogicalOrExpression' (ParseItem l CLogicalOrExpression'Empty _) sym =
  return $ ParseItem l CLogicalOrExpression'Empty sym
tLogicalOrExpression' (ParseItem l (CLogicalOrExpression' andExpr orExpr) _) sym = do
  andExpr' <- tLogicalAndExpression andExpr sym
  orExpr' <- tLogicalOrExpression' orExpr $ symbolTable andExpr'
  return $ ParseItem l (CLogicalOrExpression' andExpr' orExpr') $ symbolTable andExpr'

tLogicalAndExpression :: TypeAnnotate CLogicalAndExpression
tLogicalAndExpression (ParseItem l (CLogicalAndExpression orExpr andExpr) _) sym = do
  orExpr' <- tInclusiveOrExpression orExpr sym
  andExpr' <- tLogicalAndExpression' andExpr $ symbolTable orExpr'
  return $ ParseItem l (CLogicalAndExpression orExpr' andExpr') $ symbolTable andExpr'

tLogicalAndExpression' :: TypeAnnotate CLogicalAndExpression'
tLogicalAndExpression' (ParseItem l CLogicalAndExpression'Empty _) sym =
  return $ ParseItem l CLogicalAndExpression'Empty sym
tLogicalAndExpression' (ParseItem l (CLogicalAndExpression' orExpr andExpr) _) sym = do
  orExpr' <- tInclusiveOrExpression orExpr sym
  andExpr' <- tLogicalAndExpression' andExpr $ symbolTable orExpr'
  return $ ParseItem l (CLogicalAndExpression' orExpr' andExpr') $ symbolTable andExpr'

tInclusiveOrExpression :: TypeAnnotate CInclusiveOrExpression
tInclusiveOrExpression (ParseItem l (CInclusiveOrExpression exclusive inclusive) _) sym = do
  exclusive' <- tExclusiveOrExpression exclusive sym
  inclusive' <- tInclusiveOrExpression' inclusive $ symbolTable exclusive'
  return $ ParseItem l (CInclusiveOrExpression exclusive' inclusive') $ symbolTable inclusive'

tInclusiveOrExpression' :: TypeAnnotate CInclusiveOrExpression'
tInclusiveOrExpression' (ParseItem l (CInclusiveOrExpression'Empty) _) sym =
  return $ ParseItem l CInclusiveOrExpression'Empty sym
tInclusiveOrExpression' (ParseItem l (CInclusiveOrExpression' exclusive inclusive) _) sym = do
  exclusive' <- tExclusiveOrExpression exclusive sym
  inclusive' <- tInclusiveOrExpression' inclusive $ symbolTable exclusive'
  return $ ParseItem l (CInclusiveOrExpression' exclusive' inclusive') $ symbolTable inclusive'

tExclusiveOrExpression :: TypeAnnotate CExclusiveOrExpression
tExclusiveOrExpression (ParseItem l (CExclusiveOrExpression andExpr orExpr) _) sym = do
  andExpr' <- tAndExpression andExpr sym
  orExpr' <- tExclusiveOrExpression' orExpr $ symbolTable andExpr'
  return $ ParseItem l (CExclusiveOrExpression andExpr' orExpr') $ symbolTable orExpr'

tExclusiveOrExpression' :: TypeAnnotate CExclusiveOrExpression'
tExclusiveOrExpression' (ParseItem l CExclusiveOrExpression'Empty _) sym = do
  return $ ParseItem l CExclusiveOrExpression'Empty sym
tExclusiveOrExpression' (ParseItem l (CExclusiveOrExpression' andExpr orExpr) _) sym = do
  andExpr' <- tAndExpression andExpr sym
  orExpr' <- tExclusiveOrExpression' orExpr $ symbolTable andExpr'
  return $ ParseItem l (CExclusiveOrExpression' andExpr' orExpr') $ symbolTable orExpr'

tAndExpression :: TypeAnnotate CAndExpression
tAndExpression (ParseItem l (CAndExpression eqExpr andExpr) _) sym = do
  eqExpr' <- tEqualityExpression eqExpr sym
  andExpr' <- tAndExpression' andExpr $ symbolTable eqExpr'
  return $ ParseItem l (CAndExpression eqExpr' andExpr') $ symbolTable andExpr'

tAndExpression' :: TypeAnnotate CAndExpression'
tAndExpression' (ParseItem l CAndExpression'Empty _) sym =
  return $ ParseItem l CAndExpression'Empty sym
tAndExpression' (ParseItem l (CAndExpression' eqExpr andExpr) _) sym = do
  eqExpr' <- tEqualityExpression eqExpr sym
  andExpr' <- tAndExpression' andExpr $ symbolTable eqExpr'
  return $ ParseItem l (CAndExpression' eqExpr' andExpr') $ symbolTable andExpr'

tEqualityExpression :: TypeAnnotate CEqualityExpression
tEqualityExpression (ParseItem l (CEqualityExpression relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp $ symbolTable relExp'
  return $ ParseItem l (CEqualityExpression relExp' eqExp') $ symbolTable eqExp'

tEqualityExpression' :: TypeAnnotate CEqualityExpression'
tEqualityExpression' (ParseItem l CEqualityExpression'Empty _) sym =
  return $ ParseItem l CEqualityExpression'Empty sym
tEqualityExpression' (ParseItem l (CEqualityExpression'EQ relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp $ symbolTable relExp'
  return $ ParseItem l (CEqualityExpression'EQ relExp' eqExp') $ symbolTable eqExp'
tEqualityExpression' (ParseItem l (CEqualityExpression'NEQ relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp $ symbolTable relExp'
  return $ ParseItem l (CEqualityExpression'NEQ relExp' eqExp') $ symbolTable eqExp'

tRelationalExpression :: TypeAnnotate CRelationalExpression
tRelationalExpression (ParseItem l (CRelationalExpression shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  return $ ParseItem l (CRelationalExpression shiftExp' relExp') $ symbolTable relExp'

tRelationalExpression' :: TypeAnnotate CRelationalExpression'
tRelationalExpression' (ParseItem l CRelationalExpression'Empty _) sym =
  return $ ParseItem l CRelationalExpression'Empty sym
tRelationalExpression' (ParseItem l (CRelationalExpression'LT shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  return $ ParseItem l (CRelationalExpression'LT shiftExp' relExp') $ symbolTable relExp'
tRelationalExpression' (ParseItem l (CRelationalExpression'LTE shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  return $ ParseItem l (CRelationalExpression'LTE shiftExp' relExp') $ symbolTable relExp'
tRelationalExpression' (ParseItem l (CRelationalExpression'GT shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  return $ ParseItem l (CRelationalExpression'GT shiftExp' relExp') $ symbolTable relExp'
tRelationalExpression' (ParseItem l (CRelationalExpression'GTE shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp $ symbolTable shiftExp'
  return $ ParseItem l (CRelationalExpression'GTE shiftExp' relExp') $ symbolTable relExp'

tShiftExpression :: TypeAnnotate CShiftExpression
tShiftExpression (ParseItem l (CShiftExpression addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp $ symbolTable addExp'
  return $ ParseItem l (CShiftExpression addExp' shiftExp') $ symbolTable shiftExp'

tShiftExpression' :: TypeAnnotate CShiftExpression'
tShiftExpression' (ParseItem l CShiftExpression'Empty _) sym =
  return $ ParseItem l CShiftExpression'Empty sym
tShiftExpression' (ParseItem l (CShiftExpression'Right addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp $ symbolTable addExp'
  return $ ParseItem l (CShiftExpression'Right addExp' shiftExp') $ symbolTable shiftExp'
tShiftExpression' (ParseItem l (CShiftExpression'Left addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp $ symbolTable addExp'
  return $ ParseItem l (CShiftExpression'Left addExp' shiftExp') $ symbolTable shiftExp'

tAdditiveExpression :: TypeAnnotate CAdditiveExpression
tAdditiveExpression (ParseItem l (CAdditiveExpression multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp $ symbolTable multExp'
  return $ ParseItem l (CAdditiveExpression multExp' addExp') $ symbolTable addExp'

tAdditiveExpression' :: TypeAnnotate CAdditiveExpression'
tAdditiveExpression' (ParseItem l CAdditiveExpression'Empty _) sym =
  return $ ParseItem l CAdditiveExpression'Empty sym
tAdditiveExpression' (ParseItem l (CAdditiveExpression'Sub multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp $ symbolTable multExp'
  return $ ParseItem l (CAdditiveExpression'Sub multExp' addExp') $ symbolTable addExp'
tAdditiveExpression' (ParseItem l (CAdditiveExpression'Add multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp $ symbolTable multExp'
  return $ ParseItem l (CAdditiveExpression'Add multExp' addExp') $ symbolTable addExp'

tMultiplicativeExpression :: TypeAnnotate CMultiplicativeExpression
tMultiplicativeExpression (ParseItem l (CMultiplicativeExpression castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  return $ ParseItem l (CMultiplicativeExpression castExp' multExp') $ symbolTable multExp'

tMultiplicativeExpression' :: TypeAnnotate CMultiplicativeExpression'
tMultiplicativeExpression' (ParseItem l CMultiplicativeExpression'Empty _) sym =
  return $ ParseItem l CMultiplicativeExpression'Empty sym
tMultiplicativeExpression' (ParseItem l (CMultiplicativeExpression'Mul castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  return $ ParseItem l (CMultiplicativeExpression'Mul castExp' multExp') $ symbolTable multExp'
tMultiplicativeExpression' (ParseItem l (CMultiplicativeExpression'Div castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  return $ ParseItem l (CMultiplicativeExpression'Div castExp' multExp') $ symbolTable multExp'
tMultiplicativeExpression' (ParseItem l (CMultiplicativeExpression'Mod castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp $ symbolTable castExp'
  return $ ParseItem l (CMultiplicativeExpression'Mod castExp' multExp') $ symbolTable multExp'

tCastExpression :: TypeAnnotate CCastExpression
tCastExpression (ParseItem l (CCastExpressionUnary expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return $ ParseItem l (CCastExpressionUnary expr') $ symbolTable expr'
tCastExpression (ParseItem l (CCastExpression typeName expr) _) sym = do
  typeName' <- tTypeName typeName sym
  expr' <- tCastExpression expr sym
  return $ ParseItem l (CCastExpression typeName' expr') $ symbolTable expr'

tUnaryExpression :: TypeAnnotate CUnaryExpression
tUnaryExpression (ParseItem l (CUnaryExpressionPostfix expr) _) sym = do
  expr' <- tPostfixExpression expr sym
  return $ ParseItem l (CUnaryExpressionPostfix expr') $ symbolTable expr'
tUnaryExpression (ParseItem l (CUnaryExpressionInc expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return $ ParseItem l (CUnaryExpressionInc expr') $ symbolTable expr'
tUnaryExpression (ParseItem l (CUnaryExpressionDec expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return $ ParseItem l (CUnaryExpressionDec expr') $ symbolTable expr'
tUnaryExpression (ParseItem l (CUnaryExpressionUnaryOp op expr) _) sym = do
  op' <- tUnaryOperator op sym
  expr' <- tCastExpression expr sym
  return $ ParseItem l (CUnaryExpressionUnaryOp op' expr') $ symbolTable expr'
tUnaryExpression (ParseItem l (CUnaryExpressionSizeof expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return $ ParseItem l (CUnaryExpressionSizeof expr') $ symbolTable expr'
tUnaryExpression (ParseItem l (CUnaryExpressionSizeofType t) _) sym = do
  t' <- tTypeName t sym
  return $ ParseItem l (CUnaryExpressionSizeofType t') sym

tUnaryOperator :: TypeAnnotate CUnaryOperator
tUnaryOperator (ParseItem l op _) sym = return $ ParseItem l op sym

tPostfixExpression :: TypeAnnotate CPostfixExpression
tPostfixExpression (ParseItem l (CPostfixExpression primaryExp postfixExp) _) sym = do
  primaryExp' <- tPrimaryExpression primaryExp sym
  postfixExp' <- tPostfixExpression' postfixExp $ symbolTable primaryExp'
  return $ ParseItem l (CPostfixExpression primaryExp' postfixExp') $ symbolTable postfixExp'

tPostfixExpression' :: TypeAnnotate CPostfixExpression'
tPostfixExpression' (ParseItem l CPostfixExpression'Empty _) sym =
  return $ ParseItem l CPostfixExpression'Empty sym
tPostfixExpression' (ParseItem l (CPostfixExpression'Bracket expr postfix) _) sym = do
  let sym' = childSymbols sym
  expr' <- tExpression expr sym'
  postfix' <- tPostfixExpression' postfix sym
  return $ ParseItem l (CPostfixExpression'Bracket expr' postfix') $ symbolTable postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Paren exprList postfix) _) sym = do
  let sym' = childSymbols sym
  exprList' <- tArgumentExpressionListOptional exprList sym'
  postfix' <- tPostfixExpression' postfix sym
  return $ ParseItem l (CPostfixExpression'Paren exprList' postfix') $ symbolTable postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Dot identifier postfix) _) sym = do
  identifier' <- tIdentifier identifier sym
  postfix' <- tPostfixExpression' postfix sym
  return $ ParseItem l (CPostfixExpression'Dot identifier' postfix') $ symbolTable postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Arrow identifier postfix) _) sym = do
  identifier' <- tIdentifier identifier sym
  postfix' <- tPostfixExpression' postfix sym
  return $ ParseItem l (CPostfixExpression'Arrow identifier' postfix') $ symbolTable postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Inc postfix) _) sym = do
  postfix' <- tPostfixExpression' postfix sym
  return $ ParseItem l (CPostfixExpression'Inc postfix') $ symbolTable postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Dec postfix) _) sym = do
  postfix' <- tPostfixExpression' postfix sym
  return $ ParseItem l (CPostfixExpression'Dec postfix') $ symbolTable postfix'

tPrimaryExpression :: TypeAnnotate CPrimaryExpression
tPrimaryExpression (ParseItem l (CPrimaryExpressionParen expr) _) sym = do
  let sym' = childSymbols sym
  expr' <- tExpression expr sym'
  return $ ParseItem l (CPrimaryExpressionParen expr') sym
tPrimaryExpression (ParseItem l (CPrimaryExpressionConst c) _) sym = do
  return $ ParseItem l (CPrimaryExpressionConst (c { symbolTable = sym })) sym
tPrimaryExpression (ParseItem l (CPrimaryExpressionString s) _) sym = do
  return $ ParseItem l (CPrimaryExpressionString (s { symbolTable = sym })) sym
tPrimaryExpression (ParseItem l (CPrimaryExpressionId i) _) sym = do
  i' <- tIdentifier i sym
  return $ ParseItem l (CPrimaryExpressionId i') sym

tArgumentExpressionList :: TypeAnnotate CArgumentExpressionList
tArgumentExpressionList (ParseItem l (CArgumentExpressionList expr exprList) _) sym = do
  expr' <- tAssignmentExpression expr sym
  exprList' <- tArgumentExpressionList' exprList sym
  return $ ParseItem l (CArgumentExpressionList expr' exprList') sym

tArgumentExpressionListOptional :: TypeAnnotate CArgumentExpressionListOptional
tArgumentExpressionListOptional =
  tOptional
    CArgumentExpressionListOptionalEmpty
    CArgumentExpressionListOptional
    (\(CArgumentExpressionListOptional x) -> x)
    tArgumentExpressionList

tArgumentExpressionList' :: TypeAnnotate CArgumentExpressionList'
tArgumentExpressionList' (ParseItem l CArgumentExpressionList'Empty _) sym =
  return $ ParseItem l CArgumentExpressionList'Empty sym
tArgumentExpressionList' (ParseItem l (CArgumentExpressionList' expr exprList) _) sym = do
  expr' <- tAssignmentExpression expr sym
  exprList' <- tArgumentExpressionList' exprList sym
  return $ ParseItem l (CArgumentExpressionList' expr' exprList') sym

tConstant :: TypeAnnotate CConstant
tConstant item sym = Right $ item { symbolTable = sym }

