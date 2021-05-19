module Symbols.TypeAnnotate where

{- This module contains functions for annotating parse tree with information
   about symbols & their associated types and that check for illegal types
   or conflicting definitions. -}

import Control.Monad
import qualified Data.Map as M
import Data.Maybe (isNothing)

import Parser.ParseItem
import Utils ( Error(..)
             , Location
             , DataType(..)
             , SymbolTable(..)
             , CType(..)
             )
import Symbols.TypeReader ( readFunctionDefinition
                          , readDeclaration
                          )

type TypeAnnotator a
  = ParseItem a -> SymbolTable -> Either Error (ParseItem a)

{- Main function. Traverse the ast & return an annontated version.
   The symbol table attached to a ParseItem contains symbols after
   the statement contained in the ParseItem.  Most of the anoontators
   simply copy symbols its parent element's symboltable to its ParseItem -}
typeAnnotate ::
     ParseItem CTranslationUnit
  -> Either Error (ParseItem CTranslationUnit)
typeAnnotate item = tTranslationUnit item initialSymbols

{- Create an empty SymbolTable that uses as its parent the SymbolTable
   given as an argument. -}
childSymbols :: SymbolTable -> SymbolTable
childSymbols sym =
  SymbolTable
    { typedef = M.empty
    , labels = M.empty
    , symbols = M.empty
    , structs = M.empty
    , unions = M.empty
    , enums = M.empty
    , parent = Just sym
    }

-- Check if given symbol is already used in a structured type
checkCollisionStructured ::
     Location -> String -> SymbolTable -> Either Error ()
checkCollisionStructured c s sym =
  if isNothing structs'
     && isNothing enums'
     && isNothing unions'
     then return ()
     else Left . TypeError c $ "name clash: " ++ s
  where
    structs' = M.lookup s (structs sym)
    enums' = M.lookup s (enums sym)
    unions' = M.lookup s (unions sym)

-- Check if given symbol clashes with another one
checkCollision :: Location -> String -> SymbolTable -> Either Error ()
checkCollision c s sym =
  if isNothing typedef'
     && isNothing labels'
     && isNothing symbols'
     then return ()
     else Left . TypeError c $ "name clash: " ++ s
  where
    typedef' = M.lookup s (typedef sym)
    labels' = M.lookup s (labels sym)
    symbols' = M.lookup s (symbols sym)

-- Add typedef to symboltable and check for collisions
addTypedef ::
     String
  -> CType
  -> SymbolTable
  -> Location
  -> Either Error SymbolTable
addTypedef label t sym@SymbolTable { typedef = symTable } c = do
  checkCollision c label sym
  return (sym { typedef = M.insert label t symTable })

-- Add symbol to symboltable and check for collisions
addSymbol ::
     String
  -> CType
  -> SymbolTable
  -> Location
  -> Either Error SymbolTable
addSymbol label t sym@SymbolTable { symbols = symTable } c = do
  checkCollision c label sym
  return (sym { symbols = M.insert label t symTable })

-- Add enum to symboltable and check for collisions
addEnum ::
     String
  -> CType
  -> SymbolTable
  -> Location
  -> Either Error SymbolTable
addEnum label t sym@SymbolTable { enums = enumTable } c = do
  checkCollisionStructured c label sym
  return (sym { enums = M.insert label t enumTable })

-- Add struct to symboltable and check for collisions
addStruct ::
     String
  -> CType
  -> SymbolTable
  -> Location
  -> Either Error SymbolTable
addStruct label t sym@SymbolTable { structs = structTable } c = do
  checkCollisionStructured c label sym
  return (sym { structs = M.insert label t structTable })

-- Add union to symboltable and check for collisions
addUnion ::
     String
  -> CType
  -> SymbolTable
  -> Location
  -> Either Error SymbolTable
addUnion label t sym@SymbolTable { unions = unionTable } c = do
  checkCollisionStructured c label sym
  return (sym { unions = M.insert label t unionTable })

-- Add label to symboltable and check for collisions
addLabel ::
     String
  -> Location
  -> SymbolTable
  -> Either Error SymbolTable
addLabel label c sym@SymbolTable { labels = labelTable } = do
  checkCollision c label sym
  return (sym { labels = M.insert label c labelTable })

-- Handle Maybe values
tOptional ::
     TypeAnnotator a
  -> Maybe (ParseItem a)
  -> SymbolTable
  -> Either Error (Maybe (ParseItem a))
tOptional ta input sym =
  case input of
    Nothing -> return Nothing
    Just x -> do
      x' <- ta x sym
      return (Just x')

--------------------
-- TYPEANNOTATORS --
--------------------

tIdentifier :: TypeAnnotator CIdentifier
tIdentifier item sym = Right (item { symbolTable = sym })

tTranslationUnit :: TypeAnnotator CTranslationUnit
tTranslationUnit (ParseItem l (CTranslationUnit ext opt) _) sym = do
  ext' <- tExternalDeclaration ext sym
  opt' <- tOptional tTranslationUnit opt (symbolTable ext')
  let sym' = case opt' of
               Just o -> symbolTable o
               Nothing -> symbolTable ext'
  return . ParseItem l (CTranslationUnit ext' opt') $ sym'

tExternalDeclaration :: TypeAnnotator CExternalDeclaration
tExternalDeclaration (ParseItem l (CExternalDeclarationFunction fd) _) sym = do
  fd' <- tFunctionDefinition fd sym
  return . ParseItem l (CExternalDeclarationFunction fd') . symbolTable $ fd'
tExternalDeclaration (ParseItem l (CExternalDeclaration decl) _) sym = do
  decl' <- tDeclaration decl sym
  return . ParseItem l (CExternalDeclaration decl') . symbolTable $ decl'

{- Add function to the symbol table. Also add functions arguments to the
   symbol table used inside the function body -}
tFunctionDefinition :: TypeAnnotator CFunctionDefinition
tFunctionDefinition
     ParseItem { parseLoc = l
               , parseItem =
                   fd@(CFunctionDefinition spec decl declList compStatement)
               }
     sym = do
  (fName, fType, argNames) <- readFunctionDefinition l sym fd
  sym'           <- addSymbol fName fType sym l
  spec'          <- tOptional tDeclarationSpecifiers spec (childSymbols sym')
  decl'          <- tDeclarator decl (childSymbols sym')
  declList'      <- tOptional tDeclarationList declList (childSymbols sym')
  {- Create new symboltable with containing function arguments. Set
     previous symbol table as its parent. -}
  let tmpSym = childSymbols sym'
      sym'' = tmpSym { symbols = M.fromList . zip argNames $ argTypes }
        where (TFunction _ _ argTypes _) = dataType fType
  compStatement' <- tCompoundStatement compStatement sym''
  return $
    ParseItem
      l
      (CFunctionDefinition spec' decl' declList' compStatement')
      sym'

-- Add declared symbol(s) to symbol table
tDeclaration :: TypeAnnotator CDeclaration
tDeclaration (ParseItem l item@(CDeclaration spec initList) _) sym = do
  (isTypedef, declarations) <- readDeclaration sym item
  sym' <- if isTypedef
            then foldM
                   (\s (_, label, t) -> addTypedef label t s l)
                   sym
                   declarations
            else return sym
  sym'' <- foldM
            (\s (_, label, t) ->
              case dataType t of
                TFunction {} -> addSymbol label t s l
                TUnion name _ ->
                  case name of
                    Nothing -> return s
                    Just name' -> addUnion name' t s l
                TStruct name _ ->
                  case name of
                    Nothing -> return s
                    Just name' -> addStruct name' t s l
                TEnum name _ ->
                  case name of
                    Nothing -> return s
                    Just name' -> addEnum name' t s l
                _ -> addSymbol label t s l)
            sym'
            declarations
  spec' <- tDeclarationSpecifiers spec sym''
  initList' <- tOptional tInitDeclaratorList initList sym''
  return (ParseItem l (CDeclaration spec' initList') sym'')

-- Add declaraed symbols to symbol table
tDeclarationList :: TypeAnnotator CDeclarationList
tDeclarationList (ParseItem l (CDeclarationList decl opt) _) sym = do
  decl' <- tDeclaration decl sym
  opt' <- tOptional tDeclarationList opt . symbolTable $ decl'
  let sym' = case opt' of
               Just o -> symbolTable o
               Nothing -> symbolTable decl'
  return . ParseItem l (CDeclarationList decl' opt') $ sym'

tDeclarationSpecifiers :: TypeAnnotator CDeclarationSpecifiers
tDeclarationSpecifiers
     (ParseItem l (CDeclarationSpecifiersStorageClass spec opt) _) sym = do
  spec' <- tStorageClassSpecifier spec sym
  opt' <- tOptional tDeclarationSpecifiers opt sym
  return (ParseItem l (CDeclarationSpecifiersStorageClass spec' opt') sym)
tDeclarationSpecifiers
     (ParseItem l (CDeclarationSpecifiersTypeSpecifier spec opt) _) sym = do
  spec' <- tTypeSpecifier spec sym
  opt' <- tOptional tDeclarationSpecifiers opt sym
  return (ParseItem l (CDeclarationSpecifiersTypeSpecifier spec' opt') sym)
tDeclarationSpecifiers
     (ParseItem l (CDeclarationSpecifiersTypeQualifier spec opt) _) sym = do
  spec' <- tTypeQualifier spec sym
  opt' <- tOptional tDeclarationSpecifiers opt sym
  return (ParseItem l (CDeclarationSpecifiersTypeQualifier spec' opt') sym)

tStorageClassSpecifier :: TypeAnnotator CStorageClassSpecifier
tStorageClassSpecifier item sym = Right (item { symbolTable = sym })

tTypeSpecifier :: TypeAnnotator CTypeSpecifier
tTypeSpecifier (ParseItem l (CTypeSpecifierStructOrUnion spec) _) sym = do
  spec' <- tStructOrUnionSpecifier spec sym
  return (ParseItem l (CTypeSpecifierStructOrUnion spec') sym)
tTypeSpecifier (ParseItem l (CTypeSpecifierEnum spec) _) sym = do
  spec' <- tEnumSpecifier spec sym
  return (ParseItem l (CTypeSpecifierEnum spec') sym)
tTypeSpecifier (ParseItem l (CTypeSpecifierTypedef spec) _) sym = do
  spec' <- tTypedefName spec sym
  return (ParseItem l (CTypeSpecifierTypedef spec') sym)
tTypeSpecifier item sym = Right (item { symbolTable = sym })

tTypeQualifier :: TypeAnnotator CTypeQualifier
tTypeQualifier item sym = Right (item { symbolTable = sym })

tStructOrUnionSpecifier :: TypeAnnotator CStructOrUnionSpecifier
tStructOrUnionSpecifier
     (ParseItem l
                (CStructOrUnionSpecifierList
                  structOrUnion
                  idOpt
                  declList)
                _)
     sym = do
  su' <- tStructOrUnion structOrUnion sym
  i <- tOptional tIdentifier idOpt sym
  decl' <- tStructDeclarationList declList sym
  return . ParseItem l (CStructOrUnionSpecifierList su' i decl')
         . symbolTable
         $ decl'
tStructOrUnionSpecifier (ParseItem l (CStructOrUnionSpecifier su i) _) sym = do
  su' <- tStructOrUnion su sym
  i' <- tIdentifier i sym
  return (ParseItem l (CStructOrUnionSpecifier su' i') sym)

tStructOrUnion :: TypeAnnotator CStructOrUnion
tStructOrUnion item sym = Right (item { symbolTable = sym })

tStructDeclarationList :: TypeAnnotator CStructDeclarationList
tStructDeclarationList
     (ParseItem l (CStructDeclarationList decl lst) _) sym = do
  decl' <- tStructDeclaration decl sym
  lst' <- tOptional tStructDeclarationList lst sym
  return (ParseItem l (CStructDeclarationList decl' lst') sym)

tInitDeclaratorList :: TypeAnnotator CInitDeclaratorList
tInitDeclaratorList (ParseItem l (CInitDeclaratorList decl lst) _) sym = do
  decl' <- tInitDeclarator decl sym
  lst' <- tInitDeclaratorList' lst sym
  return (ParseItem l (CInitDeclaratorList decl' lst') sym)

tInitDeclaratorList' :: TypeAnnotator CInitDeclaratorList'
tInitDeclaratorList' (ParseItem l (CInitDeclaratorList' decl lst) _) sym = do
  decl' <- tInitDeclarator decl sym
  lst' <- tInitDeclaratorList' lst sym
  return (ParseItem l (CInitDeclaratorList' decl' lst') sym)
tInitDeclaratorList' item sym = Right (item { symbolTable = sym })

tInitDeclarator :: TypeAnnotator CInitDeclarator
tInitDeclarator (ParseItem l (CInitDeclarator decl initializerOpt) _) sym =
  case parseItem initializerOpt of
    CAssignInitializerOptional initializer -> do
      decl' <- tDeclarator decl sym
      initializer' <- tInitializer initializer sym
      let initializerOpt' =
            initializerOpt { symbolTable = sym
                           , parseItem =
                               CAssignInitializerOptional initializer'
                           }
      return (ParseItem l (CInitDeclarator decl' initializerOpt') sym)
    CAssignInitializerOptionalEmpty -> do
      decl' <- tDeclarator decl sym
      return $ ParseItem
                 l
                 (CInitDeclarator
                   decl'
                   initializerOpt { symbolTable = sym })
                 sym

tStructDeclaration :: TypeAnnotator CStructDeclaration
tStructDeclaration (ParseItem l (CStructDeclaration spec decl) _) sym = do
  spec' <- tSpecifierQualifierList spec sym
  decl' <- tStructDeclaratorList decl sym
  return (ParseItem l (CStructDeclaration spec' decl') sym)

tSpecifierQualifierList :: TypeAnnotator CSpecifierQualifierList
tSpecifierQualifierList
     (ParseItem l (CSpecifierQualifierListSpecifier spec lst) _) sym = do
  spec' <- tTypeSpecifier spec sym
  lst' <- tOptional tSpecifierQualifierList lst sym
  return (ParseItem l (CSpecifierQualifierListSpecifier spec' lst') sym)
tSpecifierQualifierList
     (ParseItem l (CSpecifierQualifierListQualifier qualifier lst) _) sym = do
  qualifier' <- tTypeQualifier qualifier sym
  lst' <- tOptional tSpecifierQualifierList lst sym
  return (ParseItem l (CSpecifierQualifierListQualifier qualifier' lst') sym)

tStructDeclaratorList :: TypeAnnotator CStructDeclaratorList
tStructDeclaratorList (ParseItem l (CStructDeclaratorList decl lst) _) sym = do
  decl' <- tStructDeclarator decl sym
  lst' <- tStructDeclaratorList' lst sym
  return (ParseItem l (CStructDeclaratorList decl' lst') sym)

tStructDeclaratorList' :: TypeAnnotator CStructDeclaratorList'
tStructDeclaratorList'
     (ParseItem l (CStructDeclaratorList' decl lst) _) sym = do
  decl' <- tStructDeclarator decl sym
  lst' <- tStructDeclaratorList' lst sym
  return (ParseItem l (CStructDeclaratorList' decl' lst') sym)
tStructDeclaratorList' item sym = Right (item { symbolTable = sym })

tStructDeclarator :: TypeAnnotator CStructDeclarator
tStructDeclarator (ParseItem l (CStructDeclarator decl) _) sym = do
  decl' <- tDeclarator decl sym
  return (ParseItem l (CStructDeclarator decl') sym)
tStructDeclarator (ParseItem l (CStructDeclaratorField decl expr) _) sym = do
  decl' <- tOptional tDeclarator decl sym
  expr' <- tConstantExpression expr sym
  return (ParseItem l (CStructDeclaratorField decl' expr') sym)

tEnumSpecifier :: TypeAnnotator CEnumSpecifier
tEnumSpecifier (ParseItem l (CEnumSpecifierList idOpt lst) _) sym = do
  identifier' <- tOptional tIdentifier idOpt sym
  lst' <- tEnumeratorList lst sym
  return (ParseItem l (CEnumSpecifierList identifier' lst') sym)
tEnumSpecifier (ParseItem l (CEnumSpecifier identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return (ParseItem l (CEnumSpecifier identifier') sym)

tEnumeratorList :: TypeAnnotator CEnumeratorList
tEnumeratorList (ParseItem l (CEnumeratorList enum lst) _) sym = do
  enum' <- tEnumerator enum sym
  lst' <- tEnumeratorList' lst sym
  return (ParseItem l (CEnumeratorList enum' lst') sym)

tEnumeratorList' :: TypeAnnotator CEnumeratorList'
tEnumeratorList' (ParseItem l (CEnumeratorList' enum lst) _) sym = do
  enum' <- tEnumerator enum sym
  lst' <- tEnumeratorList' lst sym
  return (ParseItem l (CEnumeratorList' enum' lst') sym)
tEnumeratorList' item sym = Right (item { symbolTable = sym })

tEnumerator :: TypeAnnotator CEnumerator
tEnumerator (ParseItem l (CEnumerator identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return (ParseItem l (CEnumerator identifier') sym)
tEnumerator (ParseItem l (CEnumeratorAssign identifier expr) _) sym = do
  identifier' <- tIdentifier identifier sym
  expr' <- tConstantExpression expr sym
  return (ParseItem l (CEnumeratorAssign identifier' expr') sym)

tDeclarator :: TypeAnnotator CDeclarator
tDeclarator (ParseItem l (CDeclarator pointer decl) _) sym = do
  pointer' <- tOptional tPointer pointer sym
  decl' <- tDirectDeclarator decl sym
  return (ParseItem l (CDeclarator pointer' decl') sym)

tDirectDeclarator :: TypeAnnotator CDirectDeclarator
tDirectDeclarator
     (ParseItem l (CDirectDeclaratorId identifier decl) _) sym = do
  identifier' <- tIdentifier identifier sym
  decl' <- tDirectDeclarator' decl sym
  return (ParseItem l (CDirectDeclaratorId identifier' decl') sym)
tDirectDeclarator
     (ParseItem l (CDirectDeclaratorParen decl directDecl) _) sym = do
  decl' <- tDeclarator decl sym
  directDecl' <- tDirectDeclarator' directDecl sym
  return (ParseItem l (CDirectDeclaratorParen decl' directDecl') sym)

tDirectDeclarator' :: TypeAnnotator CDirectDeclarator'
tDirectDeclarator'
     (ParseItem l (CDirectDeclarator'ConstExpr expr decl) _) sym = do
  expr' <- tOptional tConstantExpression expr sym
  decl' <- tDirectDeclarator' decl sym
  return (ParseItem l (CDirectDeclarator'ConstExpr expr' decl') sym)
tDirectDeclarator'
     (ParseItem l (CDirectDeclarator'ParamTypeList lst decl) _) sym = do
  lst' <- tParameterTypeList lst sym
  decl' <- tDirectDeclarator' decl sym
  return (ParseItem l (CDirectDeclarator'ParamTypeList lst' decl') sym)
tDirectDeclarator' (ParseItem l (CDirectDeclarator'IdList lst decl) _) sym = do
  lst' <- tOptional tIdentifierList lst sym
  decl' <- tDirectDeclarator' decl sym
  return (ParseItem l (CDirectDeclarator'IdList lst' decl') sym)
tDirectDeclarator' item sym = Right (item { symbolTable = sym })

tPointer :: TypeAnnotator CPointer
tPointer (ParseItem l (CPointer lst pointer) _) sym = do
  lst' <- tOptional tTypeQualifierList lst sym
  pointer' <- tOptional tPointer pointer sym
  return (ParseItem l (CPointer lst' pointer') sym)

tTypeQualifierList :: TypeAnnotator CTypeQualifierList
tTypeQualifierList (ParseItem l (CTypeQualifierList qualifier lst) _) sym = do
  qualifier' <- tTypeQualifier qualifier sym
  lst' <- tOptional tTypeQualifierList lst sym
  return (ParseItem l (CTypeQualifierList qualifier' lst') sym)

tParameterTypeList :: TypeAnnotator CParameterTypeList
tParameterTypeList (ParseItem l (CParameterTypeList lst varargs) _) sym = do
  lst' <- tParameterList lst sym
  varargs' <- tVarArgsOptional varargs sym
  return (ParseItem l (CParameterTypeList lst' varargs') sym)

tVarArgsOptional :: TypeAnnotator CVarArgsOptional
tVarArgsOptional item sym = Right (item { symbolTable = sym })

tParameterList :: TypeAnnotator CParameterList
tParameterList (ParseItem l (CParameterList decl lst) _) sym = do
  decl' <- tParameterDeclaration decl sym
  lst' <- tParameterList' lst sym
  return (ParseItem l (CParameterList decl' lst') sym)

tParameterList' :: TypeAnnotator CParameterList'
tParameterList' (ParseItem l (CParameterList' decl lst) _) sym = do
  decl' <- tParameterDeclaration decl sym
  lst' <- tParameterList' lst sym
  return (ParseItem l (CParameterList' decl' lst') sym)
tParameterList' item sym = Right (item { symbolTable = sym })

tParameterDeclaration :: TypeAnnotator CParameterDeclaration
tParameterDeclaration
     (ParseItem l (CParameterDeclaration spec decl) _) sym = do
  spec' <- tDeclarationSpecifiers spec sym
  decl' <- tParameterDeclaration' decl sym
  return (ParseItem l (CParameterDeclaration spec' decl') sym)

tParameterDeclaration' :: TypeAnnotator CParameterDeclaration'
tParameterDeclaration' (ParseItem l (CParameterDeclaration' decl) _) sym = do
  decl' <- tDeclarator decl sym
  return (ParseItem l (CParameterDeclaration' decl') sym)
tParameterDeclaration'
     (ParseItem l (CParameterDeclaration'Abstract decl) _) sym = do
  decl' <- tOptional tAbstractDeclarator decl sym
  return (ParseItem l (CParameterDeclaration'Abstract decl') sym)

tIdentifierList :: TypeAnnotator CIdentifierList
tIdentifierList (ParseItem l (CIdentifierList identifier lst) _) sym = do
  identifier' <- tIdentifier identifier sym
  lst' <- tIdentifierList' lst sym
  return (ParseItem l (CIdentifierList identifier' lst') sym)

tIdentifierList' :: TypeAnnotator CIdentifierList'
tIdentifierList' (ParseItem l (CIdentifierList' identifier lst) _) sym = do
  identifier' <- tIdentifier identifier sym
  lst' <- tIdentifierList' lst sym
  return (ParseItem l (CIdentifierList' identifier' lst') sym)
tIdentifierList' item sym = Right (item { symbolTable = sym })

tInitializer :: TypeAnnotator CInitializer
tInitializer (ParseItem l (CInitializerAssignment expr) _) sym = do
  expr' <- tAssignmentExpression expr sym
  return (ParseItem l (CInitializerAssignment expr') sym)
tInitializer (ParseItem l (CInitializerInitList lst) _) sym = do
  lst' <- tInitializerList lst sym
  return (ParseItem l (CInitializerInitList lst') sym)

tInitializerList :: TypeAnnotator CInitializerList
tInitializerList (ParseItem l (CInitializerList initializer lst) _) sym = do
  initializer' <- tInitializer initializer sym
  lst' <- tInitializerList' lst sym
  return (ParseItem l (CInitializerList initializer' lst') sym)

tInitializerList' :: TypeAnnotator CInitializerList'
tInitializerList' (ParseItem l (CInitializerList' initializer lst) _) sym = do
  initializer' <- tInitializer initializer sym
  lst' <- tInitializerList' lst sym
  return (ParseItem l (CInitializerList' initializer' lst') sym)
tInitializerList' item sym = Right (item { symbolTable = sym })

tTypeName :: TypeAnnotator CTypeName
tTypeName (ParseItem l (CTypeName lst decl) _) sym = do
  lst' <- tSpecifierQualifierList lst sym
  decl' <- tOptional tAbstractDeclarator decl sym
  return (ParseItem l (CTypeName lst' decl') sym)

tAbstractDeclarator :: TypeAnnotator CAbstractDeclarator
tAbstractDeclarator
     (ParseItem l (CAbstractDeclaratorPointer pointer) _) sym = do
  pointer' <- tPointer pointer sym
  return (ParseItem l (CAbstractDeclaratorPointer pointer') sym)
tAbstractDeclarator
     (ParseItem l (CAbstractDeclaratorDirect pointer decl) _) sym = do
  pointer' <- tOptional tPointer pointer sym
  decl' <- tDirectAbstractDeclarator decl sym
  return (ParseItem l (CAbstractDeclaratorDirect pointer' decl') sym)

tDirectAbstractDeclarator :: TypeAnnotator CDirectAbstractDeclarator
tDirectAbstractDeclarator
     (ParseItem l (CDirectAbstractDeclaratorParen decl rest) _) sym = do
  decl' <- tAbstractDeclarator decl sym
  rest' <- tDirectAbstractDeclarator' rest sym
  return (ParseItem l (CDirectAbstractDeclaratorParen decl' rest') sym)
tDirectAbstractDeclarator
     (ParseItem l (CDirectAbstractDeclaratorIndexed expr rest) _) sym = do
  expr' <- tOptional tConstantExpression expr sym
  rest' <- tDirectAbstractDeclarator' rest sym
  return (ParseItem l (CDirectAbstractDeclaratorIndexed expr' rest') sym)
tDirectAbstractDeclarator
     (ParseItem l (CDirectAbstractDeclaratorParams typeList rest) _) sym = do
  typeList' <- tOptional tParameterTypeList typeList sym
  rest' <- tDirectAbstractDeclarator' rest sym
  return (ParseItem l (CDirectAbstractDeclaratorParams typeList' rest') sym)

tDirectAbstractDeclarator' :: TypeAnnotator CDirectAbstractDeclarator'
tDirectAbstractDeclarator'
     (ParseItem l (CDirectAbstractDeclarator'Const expr decl) _) sym = do
  expr' <- tOptional tConstantExpression expr sym
  decl' <- tDirectAbstractDeclarator' decl sym
  return (ParseItem l (CDirectAbstractDeclarator'Const expr' decl') sym)
tDirectAbstractDeclarator'
     (ParseItem l (CDirectAbstractDeclarator'Params lst decl) _) sym = do
  lst' <- tOptional tParameterTypeList lst sym
  decl' <- tDirectAbstractDeclarator' decl sym
  return (ParseItem l (CDirectAbstractDeclarator'Params lst' decl') sym)
tDirectAbstractDeclarator' item sym = Right (item { symbolTable = sym })

tTypedefName :: TypeAnnotator CTypedefName
tTypedefName (ParseItem l (CTypedefName identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  return (ParseItem l (CTypedefName identifier') sym)

tStatement :: TypeAnnotator CStatement
tStatement (ParseItem l (CStatementLabeled statement) _) sym = do
  statement' <- tLabeledStatement statement sym
  return . ParseItem l (CStatementLabeled statement')
         . symbolTable
         $ statement'
tStatement (ParseItem l (CStatementExpression statement) _) sym = do
  statement' <- tExpressionStatement statement sym
  return (ParseItem l (CStatementExpression statement') sym)
tStatement (ParseItem l (CStatementCompound statement) _) sym = do
  statement' <- tCompoundStatement statement sym
  return (ParseItem l (CStatementCompound statement') sym)
tStatement (ParseItem l (CStatementSelection statement) _) sym = do
  statement' <- tSelectionStatement statement sym
  return (ParseItem l (CStatementSelection statement') sym)
tStatement (ParseItem l (CStatementIteration statement) _) sym = do
  statement' <- tIterationStatement statement sym
  return (ParseItem l (CStatementIteration statement') sym)
tStatement (ParseItem l (CStatementJump statement) _) sym = do
  statement' <- tJumpStatement statement sym
  return (ParseItem l (CStatementJump statement') sym)

-- Add label to symbol table
tLabeledStatement :: TypeAnnotator CLabeledStatement
tLabeledStatement
     (ParseItem l (CLabeledStatementId identifier statement) _) sym = do
  identifier' <- tIdentifier identifier sym
  let CIdentifier i = parseItem identifier
  sym' <- addLabel i l sym
  statement' <- tStatement statement sym'
  return . ParseItem l (CLabeledStatementId identifier' statement')
         . symbolTable
         $ statement'
tLabeledStatement
     (ParseItem l (CLabeledStatementCase expr statement) _) sym = do
  expr' <- tConstantExpression expr sym
  statement' <- tStatement statement sym
  return . ParseItem l (CLabeledStatementCase expr' statement')
         . symbolTable
         $ statement'
tLabeledStatement (ParseItem l (CLabeledStatementDefault statement) _) sym = do
  statement' <- tStatement statement sym
  return . ParseItem l (CLabeledStatementDefault statement')
         . symbolTable
         $ statement'

tExpressionStatement :: TypeAnnotator CExpressionStatement
tExpressionStatement (ParseItem l (CExpressionStatement expr) _) sym = do
  expr' <- tOptional tExpression expr sym
  let sym' = maybe sym symbolTable expr'
  return . ParseItem l (CExpressionStatement expr') $ sym'

{- Symbols defined inside a compound statement are allowed to clash
   with those defined outside of it -}
tCompoundStatement :: TypeAnnotator CCompoundStatement
tCompoundStatement (ParseItem l (CCompoundStatement decl statement) _) sym = do
  let sym' = childSymbols sym
  decl' <- tOptional tDeclarationList decl sym'
  let sym'' = maybe sym' symbolTable decl'
  statement' <- tOptional tStatementList statement sym''
  return (ParseItem l (CCompoundStatement decl' statement') sym)

tStatementList :: TypeAnnotator CStatementList
tStatementList (ParseItem l (CStatementList statement listOpt) _) sym = do
  statement' <- tStatement statement sym
  listOpt' <- tOptional tStatementList listOpt (symbolTable statement')
  let sym' = case listOpt' of
               Just list -> symbolTable list
               Nothing -> symbolTable statement'
  return . ParseItem l (CStatementList statement' listOpt') $ sym'

{- Symbols defined inside an if or switch statement are allowed to clash
   with those defined outside of it -}
tSelectionStatement :: TypeAnnotator CSelectionStatement
tSelectionStatement
     (ParseItem l (CSelectionStatementIf expr statement elseopt) _) sym = do
  let sym' = childSymbols sym
  expr' <- tExpression expr sym'
  statement' <- tStatement statement (symbolTable expr')
  elseopt' <- tElseOptional elseopt (symbolTable expr')
  return (ParseItem l (CSelectionStatementIf expr' statement' elseopt') sym)
tSelectionStatement
     (ParseItem l (CSelectionStatementSwitch expr statement) _) sym = do
  expr' <- tExpression expr sym
  let sym' = childSymbols sym
  statement' <- tStatement statement sym'
  return (ParseItem l (CSelectionStatementSwitch expr' statement') sym)

tElseOptional :: TypeAnnotator CElseOptional
tElseOptional item@ParseItem { parseItem = CElseOptional statement } sym = do
  statement' <- tStatement statement sym
  return (item { symbolTable = symbolTable statement' })
tElseOptional item sym = return (item { symbolTable = sym })

{- Symbols defined inside a loop statement are allowed to clash
   with those defined outside of it -}
tIterationStatement :: TypeAnnotator CIterationStatement
tIterationStatement
     (ParseItem l (CIterationStatementWhile expr statement) _) sym = do
  expr' <- tExpression expr sym
  let sym' = childSymbols sym
  statement' <- tStatement statement sym'
  return (ParseItem l (CIterationStatementWhile expr' statement') sym)
tIterationStatement
     (ParseItem l (CIterationStatementDoWhile statement expr) _) sym = do
  let sym' = childSymbols sym
  statement' <- tStatement statement sym'
  expr' <- tExpression expr sym'
  return (ParseItem l (CIterationStatementDoWhile statement' expr') sym)
tIterationStatement
     (ParseItem l (CIterationStatementFor decl incr cond body) _) sym = do
  let sym' = childSymbols sym
  decl' <- tOptional tExpression decl sym'
  let sym'' = maybe sym' symbolTable decl'
  incr' <- tOptional tExpression incr sym''
  cond' <- tOptional tExpression cond sym''
  body' <- tStatement body sym''
  return (ParseItem l (CIterationStatementFor decl' incr' cond' body') sym)

tJumpStatement :: TypeAnnotator CJumpStatement
tJumpStatement (ParseItem l (CJumpStatementGoto identifier) _) sym = do
  identifier' <- tIdentifier identifier sym
  let CIdentifier label = parseItem identifier
  case M.lookup label (labels sym) of
    Nothing -> Left . SyntaxError l $ "Jump to an undefined label"
    Just _ -> return . ParseItem l (CJumpStatementGoto identifier') $ sym
tJumpStatement (ParseItem l CJumpStatementContinue _) sym =
  return (ParseItem l CJumpStatementContinue sym)
tJumpStatement (ParseItem l CJumpStatementBreak _) sym =
  return (ParseItem l CJumpStatementBreak sym)
tJumpStatement (ParseItem l (CJumpStatementReturn expr) _) sym = do
  expr' <- tOptional tExpression expr sym
  return (ParseItem l (CJumpStatementReturn expr') sym)

tExpression :: TypeAnnotator CExpression
tExpression (ParseItem l (CExpression assign expr) _) sym = do
  assign' <- tAssignmentExpression assign sym
  expr' <- tExpression' expr (symbolTable assign')
  return . ParseItem l (CExpression assign' expr') . symbolTable $ expr'

tExpression' :: TypeAnnotator CExpression'
tExpression' (ParseItem l CExpression'Empty _) sym =
  return (ParseItem l CExpression'Empty sym)
tExpression' (ParseItem l (CExpression' assign expr) _) sym = do
  assign' <- tAssignmentExpression assign sym
  expr' <- tExpression' expr (symbolTable assign')
  return . ParseItem l (CExpression' assign' expr') . symbolTable $ expr'

tAssignmentExpression :: TypeAnnotator CAssignmentExpression
tAssignmentExpression
     (ParseItem l (CAssignmentExpressionConditional expr) _) sym = do
  expr' <- tConditionalExpression expr sym
  return . ParseItem l (CAssignmentExpressionConditional expr')
         . symbolTable
         $ expr'
tAssignmentExpression
     (ParseItem l (CAssignmentExpression unary operator expr) _) sym = do
  unary' <- tUnaryExpression unary sym
  operator' <- tAssignmentOperator operator (symbolTable unary')
  expr' <- tAssignmentExpression expr (symbolTable unary')
  return . ParseItem l (CAssignmentExpression unary' operator' expr')
         . symbolTable
         $ expr'

tAssignmentOperator :: TypeAnnotator CAssignmentOperator
tAssignmentOperator item sym = return (item { symbolTable = sym })

tConditionalExpression :: TypeAnnotator CConditionalExpression
tConditionalExpression
     (ParseItem l (CConditionalExpression expr ternary) _) sym = do
  expr' <- tLogicalOrExpression expr sym
  ternary' <- tTernaryOptional ternary (symbolTable expr')
  return . ParseItem l (CConditionalExpression expr' ternary')
         . symbolTable
         $ expr'

tTernaryOptional :: TypeAnnotator CTernaryOptional
tTernaryOptional (ParseItem l CTernaryOptionalEmpty _) sym =
  return (ParseItem l CTernaryOptionalEmpty sym)
tTernaryOptional (ParseItem l (CTernaryOptional expr cond) _) sym = do
  expr' <- tExpression expr sym
  cond' <- tConditionalExpression cond (symbolTable expr')
  return . ParseItem l (CTernaryOptional expr' cond') . symbolTable $ expr'

tConstantExpression :: TypeAnnotator CConstantExpression
tConstantExpression (ParseItem l (CConstantExpression expr) _) sym = do
  expr' <- tConditionalExpression expr sym
  return (ParseItem l (CConstantExpression expr') sym)

tLogicalOrExpression :: TypeAnnotator CLogicalOrExpression
tLogicalOrExpression
     (ParseItem l (CLogicalOrExpression andExpr orExpr) _) sym = do
  andExpr' <- tLogicalAndExpression andExpr sym
  orExpr' <- tLogicalOrExpression' orExpr (symbolTable andExpr')
  return . ParseItem l (CLogicalOrExpression andExpr' orExpr')
         . symbolTable
         $ andExpr'

tLogicalOrExpression' :: TypeAnnotator CLogicalOrExpression'
tLogicalOrExpression' (ParseItem l CLogicalOrExpression'Empty _) sym =
  return (ParseItem l CLogicalOrExpression'Empty sym)
tLogicalOrExpression'
     (ParseItem l (CLogicalOrExpression' andExpr orExpr) _) sym = do
  andExpr' <- tLogicalAndExpression andExpr sym
  orExpr' <- tLogicalOrExpression' orExpr (symbolTable andExpr')
  return . ParseItem l (CLogicalOrExpression' andExpr' orExpr')
         . symbolTable
         $ andExpr'

tLogicalAndExpression :: TypeAnnotator CLogicalAndExpression
tLogicalAndExpression
     (ParseItem l (CLogicalAndExpression orExpr andExpr) _) sym = do
  orExpr' <- tInclusiveOrExpression orExpr sym
  andExpr' <- tLogicalAndExpression' andExpr (symbolTable orExpr')
  return . ParseItem l (CLogicalAndExpression orExpr' andExpr')
         . symbolTable
         $ andExpr'

tLogicalAndExpression' :: TypeAnnotator CLogicalAndExpression'
tLogicalAndExpression' (ParseItem l CLogicalAndExpression'Empty _) sym =
  return (ParseItem l CLogicalAndExpression'Empty sym)
tLogicalAndExpression'
     (ParseItem l (CLogicalAndExpression' orExpr andExpr) _) sym = do
  orExpr' <- tInclusiveOrExpression orExpr sym
  andExpr' <- tLogicalAndExpression' andExpr (symbolTable orExpr')
  return . ParseItem l (CLogicalAndExpression' orExpr' andExpr')
         . symbolTable
         $ andExpr'

tInclusiveOrExpression :: TypeAnnotator CInclusiveOrExpression
tInclusiveOrExpression
     (ParseItem l (CInclusiveOrExpression exclusive inclusive) _) sym = do
  exclusive' <- tExclusiveOrExpression exclusive sym
  inclusive' <- tInclusiveOrExpression' inclusive (symbolTable exclusive')
  return . ParseItem l (CInclusiveOrExpression exclusive' inclusive')
         . symbolTable
         $ inclusive'

tInclusiveOrExpression' :: TypeAnnotator CInclusiveOrExpression'
tInclusiveOrExpression' (ParseItem l CInclusiveOrExpression'Empty _) sym =
  return (ParseItem l CInclusiveOrExpression'Empty sym)
tInclusiveOrExpression'
     (ParseItem l (CInclusiveOrExpression' exclusive inclusive) _) sym = do
  exclusive' <- tExclusiveOrExpression exclusive sym
  inclusive' <- tInclusiveOrExpression' inclusive (symbolTable exclusive')
  return . ParseItem l (CInclusiveOrExpression' exclusive' inclusive')
         . symbolTable
         $ inclusive'

tExclusiveOrExpression :: TypeAnnotator CExclusiveOrExpression
tExclusiveOrExpression
     (ParseItem l (CExclusiveOrExpression andExpr orExpr) _) sym = do
  andExpr' <- tAndExpression andExpr sym
  orExpr' <- tExclusiveOrExpression' orExpr (symbolTable andExpr')
  return . ParseItem l (CExclusiveOrExpression andExpr' orExpr')
         . symbolTable
         $ orExpr'

tExclusiveOrExpression' :: TypeAnnotator CExclusiveOrExpression'
tExclusiveOrExpression' (ParseItem l CExclusiveOrExpression'Empty _) sym = do
  return (ParseItem l CExclusiveOrExpression'Empty sym)
tExclusiveOrExpression'
     (ParseItem l (CExclusiveOrExpression' andExpr orExpr) _) sym = do
  andExpr' <- tAndExpression andExpr sym
  orExpr' <- tExclusiveOrExpression' orExpr (symbolTable andExpr')
  return . ParseItem l (CExclusiveOrExpression' andExpr' orExpr')
         . symbolTable
         $ orExpr'

tAndExpression :: TypeAnnotator CAndExpression
tAndExpression (ParseItem l (CAndExpression eqExpr andExpr) _) sym = do
  eqExpr' <- tEqualityExpression eqExpr sym
  andExpr' <- tAndExpression' andExpr (symbolTable eqExpr')
  return . ParseItem l (CAndExpression eqExpr' andExpr')
         . symbolTable
         $ andExpr'

tAndExpression' :: TypeAnnotator CAndExpression'
tAndExpression' (ParseItem l CAndExpression'Empty _) sym =
  return (ParseItem l CAndExpression'Empty sym)
tAndExpression' (ParseItem l (CAndExpression' eqExpr andExpr) _) sym = do
  eqExpr' <- tEqualityExpression eqExpr sym
  andExpr' <- tAndExpression' andExpr (symbolTable eqExpr')
  return . ParseItem l (CAndExpression' eqExpr' andExpr')
         . symbolTable
         $ andExpr'

tEqualityExpression :: TypeAnnotator CEqualityExpression
tEqualityExpression (ParseItem l (CEqualityExpression relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp (symbolTable relExp')
  return . ParseItem l (CEqualityExpression relExp' eqExp')
         . symbolTable
         $ eqExp'

tEqualityExpression' :: TypeAnnotator CEqualityExpression'
tEqualityExpression' (ParseItem l CEqualityExpression'Empty _) sym =
  return (ParseItem l CEqualityExpression'Empty sym)
tEqualityExpression'
     (ParseItem l (CEqualityExpression'EQ relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp (symbolTable relExp')
  return . ParseItem l (CEqualityExpression'EQ relExp' eqExp')
         . symbolTable
         $ eqExp'
tEqualityExpression'
     (ParseItem l (CEqualityExpression'NEQ relExp eqExp) _) sym = do
  relExp' <- tRelationalExpression relExp sym
  eqExp' <- tEqualityExpression' eqExp (symbolTable relExp')
  return . ParseItem l (CEqualityExpression'NEQ relExp' eqExp')
         . symbolTable
         $ eqExp'

tRelationalExpression :: TypeAnnotator CRelationalExpression
tRelationalExpression
     (ParseItem l (CRelationalExpression shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp (symbolTable shiftExp')
  return . ParseItem l (CRelationalExpression shiftExp' relExp')
         . symbolTable
         $ relExp'

tRelationalExpression' :: TypeAnnotator CRelationalExpression'
tRelationalExpression' (ParseItem l CRelationalExpression'Empty _) sym =
  return (ParseItem l CRelationalExpression'Empty sym)
tRelationalExpression'
     (ParseItem l (CRelationalExpression'LT shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp (symbolTable shiftExp')
  return . ParseItem l (CRelationalExpression'LT shiftExp' relExp')
         . symbolTable
         $ relExp'
tRelationalExpression'
     (ParseItem l (CRelationalExpression'LTE shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp (symbolTable shiftExp')
  return . ParseItem l (CRelationalExpression'LTE shiftExp' relExp')
         . symbolTable
         $ relExp'
tRelationalExpression'
     (ParseItem l (CRelationalExpression'GT shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp (symbolTable shiftExp')
  return . ParseItem l (CRelationalExpression'GT shiftExp' relExp')
         . symbolTable
         $ relExp'
tRelationalExpression'
     (ParseItem l (CRelationalExpression'GTE shiftExp relExp) _) sym = do
  shiftExp' <- tShiftExpression shiftExp sym
  relExp' <- tRelationalExpression' relExp (symbolTable shiftExp')
  return . ParseItem l (CRelationalExpression'GTE shiftExp' relExp')
         . symbolTable
         $ relExp'

tShiftExpression :: TypeAnnotator CShiftExpression
tShiftExpression (ParseItem l (CShiftExpression addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp (symbolTable addExp')
  return . ParseItem l (CShiftExpression addExp' shiftExp')
         . symbolTable
         $ shiftExp'

tShiftExpression' :: TypeAnnotator CShiftExpression'
tShiftExpression' (ParseItem l CShiftExpression'Empty _) sym =
  return (ParseItem l CShiftExpression'Empty sym)
tShiftExpression'
     (ParseItem l (CShiftExpression'Right addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp (symbolTable addExp')
  return . ParseItem l (CShiftExpression'Right addExp' shiftExp')
         . symbolTable
         $ shiftExp'
tShiftExpression'
     (ParseItem l (CShiftExpression'Left addExp shiftExp) _) sym = do
  addExp' <- tAdditiveExpression addExp sym
  shiftExp' <- tShiftExpression' shiftExp (symbolTable addExp')
  return . ParseItem l (CShiftExpression'Left addExp' shiftExp')
         . symbolTable
         $ shiftExp'

tAdditiveExpression :: TypeAnnotator CAdditiveExpression
tAdditiveExpression
     (ParseItem l (CAdditiveExpression multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp (symbolTable multExp')
  return . ParseItem l (CAdditiveExpression multExp' addExp')
         . symbolTable
         $ addExp'

tAdditiveExpression' :: TypeAnnotator CAdditiveExpression'
tAdditiveExpression' (ParseItem l CAdditiveExpression'Empty _) sym =
  return (ParseItem l CAdditiveExpression'Empty sym)
tAdditiveExpression'
     (ParseItem l (CAdditiveExpression'Sub multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp (symbolTable multExp')
  return . ParseItem l (CAdditiveExpression'Sub multExp' addExp')
         . symbolTable
         $ addExp'
tAdditiveExpression'
     (ParseItem l (CAdditiveExpression'Add multExp addExp) _) sym = do
  multExp' <- tMultiplicativeExpression multExp sym
  addExp' <- tAdditiveExpression' addExp (symbolTable multExp')
  return . ParseItem l (CAdditiveExpression'Add multExp' addExp')
         . symbolTable
         $ addExp'

tMultiplicativeExpression :: TypeAnnotator CMultiplicativeExpression
tMultiplicativeExpression
     (ParseItem l (CMultiplicativeExpression castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp (symbolTable castExp')
  return . ParseItem l (CMultiplicativeExpression castExp' multExp')
         . symbolTable
         $ multExp'

tMultiplicativeExpression' :: TypeAnnotator CMultiplicativeExpression'
tMultiplicativeExpression'
     (ParseItem l CMultiplicativeExpression'Empty _) sym =
  return (ParseItem l CMultiplicativeExpression'Empty sym)
tMultiplicativeExpression'
     (ParseItem l (CMultiplicativeExpression'Mul castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp (symbolTable castExp')
  return . ParseItem l (CMultiplicativeExpression'Mul castExp' multExp')
         . symbolTable
         $ multExp'
tMultiplicativeExpression'
     (ParseItem l (CMultiplicativeExpression'Div castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp (symbolTable castExp')
  return . ParseItem l (CMultiplicativeExpression'Div castExp' multExp')
         . symbolTable
         $ multExp'
tMultiplicativeExpression'
     (ParseItem l (CMultiplicativeExpression'Mod castExp multExp) _) sym = do
  castExp' <- tCastExpression castExp sym
  multExp' <- tMultiplicativeExpression' multExp (symbolTable castExp')
  return . ParseItem l (CMultiplicativeExpression'Mod castExp' multExp')
         . symbolTable
         $ multExp'

tCastExpression :: TypeAnnotator CCastExpression
tCastExpression (ParseItem l (CCastExpressionUnary expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return . ParseItem l (CCastExpressionUnary expr') . symbolTable $ expr'
tCastExpression (ParseItem l (CCastExpression typeName expr) _) sym = do
  typeName' <- tTypeName typeName sym
  expr' <- tCastExpression expr sym
  return . ParseItem l (CCastExpression typeName' expr') . symbolTable $ expr'

tUnaryExpression :: TypeAnnotator CUnaryExpression
tUnaryExpression (ParseItem l (CUnaryExpressionPostfix expr) _) sym = do
  expr' <- tPostfixExpression expr sym
  return . ParseItem l (CUnaryExpressionPostfix expr') . symbolTable $ expr'
tUnaryExpression (ParseItem l (CUnaryExpressionInc expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return . ParseItem l (CUnaryExpressionInc expr') . symbolTable $ expr'
tUnaryExpression (ParseItem l (CUnaryExpressionDec expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return . ParseItem l (CUnaryExpressionDec expr') . symbolTable $ expr'
tUnaryExpression (ParseItem l (CUnaryExpressionUnaryOp op expr) _) sym = do
  op' <- tUnaryOperator op sym
  expr' <- tCastExpression expr sym
  return . ParseItem l (CUnaryExpressionUnaryOp op' expr')
         . symbolTable
         $ expr'
tUnaryExpression (ParseItem l (CUnaryExpressionSizeof expr) _) sym = do
  expr' <- tUnaryExpression expr sym
  return . ParseItem l (CUnaryExpressionSizeof expr') . symbolTable $ expr'
tUnaryExpression (ParseItem l (CUnaryExpressionSizeofType t) _) sym = do
  t' <- tTypeName t sym
  return (ParseItem l (CUnaryExpressionSizeofType t') sym)

tUnaryOperator :: TypeAnnotator CUnaryOperator
tUnaryOperator (ParseItem l op _) sym = return (ParseItem l op sym)

tPostfixExpression :: TypeAnnotator CPostfixExpression
tPostfixExpression
     (ParseItem l (CPostfixExpression primaryExp postfixExp) _) sym = do
  primaryExp' <- tPrimaryExpression primaryExp sym
  postfixExp' <- tPostfixExpression' postfixExp (symbolTable primaryExp')
  return . ParseItem l (CPostfixExpression primaryExp' postfixExp')
         . symbolTable
         $ postfixExp'

tPostfixExpression' :: TypeAnnotator CPostfixExpression'
tPostfixExpression' (ParseItem l CPostfixExpression'Empty _) sym =
  return (ParseItem l CPostfixExpression'Empty sym)
tPostfixExpression'
     (ParseItem l (CPostfixExpression'Bracket expr postfix) _) sym = do
  let sym' = childSymbols sym
  expr' <- tExpression expr sym'
  postfix' <- tPostfixExpression' postfix sym
  return . ParseItem l (CPostfixExpression'Bracket expr' postfix')
         . symbolTable
         $ postfix'
tPostfixExpression'
     (ParseItem l (CPostfixExpression'Paren exprList postfix) _) sym = do
  let sym' = childSymbols sym
  exprList' <- tOptional tArgumentExpressionList exprList sym'
  postfix' <- tPostfixExpression' postfix sym
  return . ParseItem l (CPostfixExpression'Paren exprList' postfix')
         . symbolTable
         $ postfix'
tPostfixExpression'
     (ParseItem l (CPostfixExpression'Dot identifier postfix) _) sym = do
  identifier' <- tIdentifier identifier sym
  postfix' <- tPostfixExpression' postfix sym
  return . ParseItem l (CPostfixExpression'Dot identifier' postfix')
         . symbolTable
         $ postfix'
tPostfixExpression'
     (ParseItem l (CPostfixExpression'Arrow identifier postfix) _) sym = do
  identifier' <- tIdentifier identifier sym
  postfix' <- tPostfixExpression' postfix sym
  return . ParseItem l (CPostfixExpression'Arrow identifier' postfix')
         . symbolTable
         $ postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Inc postfix) _) sym = do
  postfix' <- tPostfixExpression' postfix sym
  return . ParseItem l (CPostfixExpression'Inc postfix')
         . symbolTable
         $ postfix'
tPostfixExpression' (ParseItem l (CPostfixExpression'Dec postfix) _) sym = do
  postfix' <- tPostfixExpression' postfix sym
  return . ParseItem l (CPostfixExpression'Dec postfix')
         . symbolTable
         $ postfix'

tPrimaryExpression :: TypeAnnotator CPrimaryExpression
tPrimaryExpression (ParseItem l (CPrimaryExpressionParen expr) _) sym = do
  let sym' = childSymbols sym
  expr' <- tExpression expr sym'
  return (ParseItem l (CPrimaryExpressionParen expr') sym)
tPrimaryExpression (ParseItem l (CPrimaryExpressionConst c) _) sym = do
  return (ParseItem l (CPrimaryExpressionConst (c { symbolTable = sym })) sym)
tPrimaryExpression (ParseItem l (CPrimaryExpressionString s) _) sym = do
  return . ParseItem
             l
             (CPrimaryExpressionString s { symbolTable = sym })
             $ sym
tPrimaryExpression (ParseItem l (CPrimaryExpressionId i) _) sym = do
  i' <- tIdentifier i sym
  return (ParseItem l (CPrimaryExpressionId i') sym)

tArgumentExpressionList :: TypeAnnotator CArgumentExpressionList
tArgumentExpressionList
     (ParseItem l (CArgumentExpressionList expr exprList) _) sym = do
  expr' <- tAssignmentExpression expr sym
  exprList' <- tArgumentExpressionList' exprList sym
  return (ParseItem l (CArgumentExpressionList expr' exprList') sym)

tArgumentExpressionList' :: TypeAnnotator CArgumentExpressionList'
tArgumentExpressionList' (ParseItem l CArgumentExpressionList'Empty _) sym =
  return (ParseItem l CArgumentExpressionList'Empty sym)
tArgumentExpressionList'
     (ParseItem l (CArgumentExpressionList' expr exprList) _) sym = do
  expr' <- tAssignmentExpression expr sym
  exprList' <- tArgumentExpressionList' exprList sym
  return (ParseItem l (CArgumentExpressionList' expr' exprList') sym)

tConstant :: TypeAnnotator CConstant
tConstant item sym = Right (item { symbolTable = sym })

