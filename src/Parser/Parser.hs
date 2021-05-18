{-# LANGUAGE LambdaCase #-}

module Parser.Parser where

import Control.Applicative

import Parser.ParseItem
import Parser.ParserUtils
import Lexeme (CLexeme(..))
import Scanner (ScanItem(..))
import Utils (Error(..))

-- TODO: include filename in the error
parseCCode :: Input -> Either Error (ParseItem CTranslationUnit)
parseCCode input = do
  (notParsed, result) <- runParser cParser input
  if null notParsed
    then return result
    else Left . ParseError ("", (1, 1)) $ "Parser failed to consume entire input"

-- main parser
cParser :: PIParser CTranslationUnit
cParser = cTranslationUnitP <* singleP LEndMarker

cIdentifierP :: PIParser CIdentifier
cIdentifierP =
  parserToPIParser $ CIdentifier <$> labelP

cTranslationUnitP :: PIParser CTranslationUnit
cTranslationUnitP =
  parserToPIParser $
  liftA2
    CTranslationUnit
    cExternalDeclarationP
    (optionalParser cTranslationUnitP)

cExternalDeclarationP :: PIParser CExternalDeclaration
cExternalDeclarationP =
  parserToPIParser $
  CExternalDeclarationFunction <$> cFunctionDefinitionP <|>
  CExternalDeclaration <$> cDeclarationP

cFunctionDefinitionP :: PIParser CFunctionDefinition
cFunctionDefinitionP =
  parserToPIParser $
  CFunctionDefinition <$>
  cFunctionSpecifiersP <*>
  cDeclaratorP <*>
  (optionalParser cDeclarationListP) <*>
  cCompoundStatementP

-- interpret LLabel before open parenthesis as function name
-- rather than typedef name
cFunctionSpecifiersP :: Parser (Maybe (PI CDeclarationSpecifiers))
cFunctionSpecifiersP =
  Parser $ \case
    input@((ScanItem _ _ (LLabel _)):(ScanItem _ _ LParenthesisOpen):_) ->
      return (input, Nothing)
    input ->
      case runParser specifiersP input of
        Left _ -> return (input, Nothing)
        r -> fmap Just <$> r
    where
      specifiersP =
        parserToPIParser $ storageClassP <|> typeSpecifierP <|> typeQualifierP
      storageClassP =
        liftA2
          CDeclarationSpecifiersStorageClass
          cStorageClassSpecifierP
          cFunctionSpecifiersP
      typeSpecifierP =
        liftA2
          CDeclarationSpecifiersTypeSpecifier
          cTypeSpecifierP
          cFunctionSpecifiersP
      typeQualifierP =
        liftA2
          CDeclarationSpecifiersTypeQualifier
          cTypeQualifierP
          cFunctionSpecifiersP

cDeclarationP :: PIParser CDeclaration
cDeclarationP =
  parserToPIParser $
  liftA2
    CDeclaration
    (afterP cDeclarationSpecifiersP after)
    after
  where after = (optionalParser cInitDeclaratorListP) <* singleP LSemiColon

cDeclarationListP :: PIParser CDeclarationList
cDeclarationListP =
  parserToPIParser $
  liftA2 CDeclarationList cDeclarationP (optionalParser cDeclarationListP)

cDeclarationSpecifiersP :: PIParser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserToPIParser $
  liftA2
    CDeclarationSpecifiersStorageClass
    cStorageClassSpecifierP
    (optionalParser cDeclarationSpecifiersP) <|>
  liftA2
    CDeclarationSpecifiersTypeSpecifier
    cTypeSpecifierP
    (optionalParser cDeclarationSpecifiersP) <|>
  liftA2
    CDeclarationSpecifiersTypeQualifier
    cTypeQualifierP
    (optionalParser cDeclarationSpecifiersP)

cStorageClassSpecifierP :: PIParser CStorageClassSpecifier
cStorageClassSpecifierP =
  parserToPIParser $
  CStorageClassSpecifierAuto <$ singleP LAuto <|>
  CStorageClassSpecifierRegister <$ singleP LRegister <|>
  CStorageClassSpecifierStatic <$ singleP LStatic <|>
  CStorageClassSpecifierExtern <$ singleP LExtern <|>
  CStorageClassSpecifierTypedef <$ singleP LTypedef

cTypeSpecifierP :: PIParser CTypeSpecifier
cTypeSpecifierP =
  parserToPIParser $
  CTypeSpecifierVoid <$ singleP LVoid <|>
  CTypeSpecifierChar <$ singleP LChar <|>
  CTypeSpecifierShort <$ singleP LShort <|>
  CTypeSpecifierInt <$ singleP LInt <|>
  CTypeSpecifierLong <$ singleP LLong <|>
  CTypeSpecifierFloat <$ singleP LFloat <|>
  CTypeSpecifierDouble <$ singleP LDouble <|>
  CTypeSpecifierSigned <$ singleP LSigned <|>
  CTypeSpecifierUnsigned <$ singleP LUnsigned <|>
  CTypeSpecifierStructOrUnion <$> cStructOrUnionSpecifierP <|>
  CTypeSpecifierEnum <$> cEnumSpecifierP <|>
  CTypeSpecifierTypedef <$> cTypedefNameP

cTypeQualifierP :: PIParser CTypeQualifier
cTypeQualifierP =
  parserToPIParser $
  CTypeQualifierConst <$ singleP LConst <|>
  CTypeQualifierVolatile <$ singleP LVolatile

cStructOrUnionSpecifierP :: PIParser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserToPIParser $
  liftA3
    CStructOrUnionSpecifierList
    cStructOrUnionP
    (optionalParser cIdentifierP)
    (braceP cStructDeclarationListP) <|>
  liftA2
    CStructOrUnionSpecifier
    cStructOrUnionP
    cIdentifierP

cStructOrUnionP :: PIParser CStructOrUnion
cStructOrUnionP =
  parserToPIParser $
  CStructOrUnionStruct <$ singleP LStruct <|>
  CStructOrUnionUnion <$ singleP LUnion

cStructDeclarationListP :: PIParser CStructDeclarationList
cStructDeclarationListP =
  parserToPIParser $
  liftA2
    CStructDeclarationList
    cStructDeclarationP
    (optionalParser cStructDeclarationListP)

cInitDeclaratorListP :: PIParser CInitDeclaratorList
cInitDeclaratorListP =
  parserToPIParser $
  liftA2
    CInitDeclaratorList
    cInitDeclaratorP
    cInitDeclaratorListP'

cInitDeclaratorListP' :: PIParser CInitDeclaratorList'
cInitDeclaratorListP' =
  parserToPIParser $
  liftA2
    CInitDeclaratorList'
    (singleP LComma *> cInitDeclaratorP)
    cInitDeclaratorListP' <|>
  pure
    CInitDeclaratorList'Empty

cInitDeclaratorP :: PIParser CInitDeclarator
cInitDeclaratorP =
  parserToPIParser $
  liftA2
    CInitDeclarator
    cDeclaratorP
    cAssignInitializerOptionalP

cAssignInitializerOptionalP :: PIParser CAssignInitializerOptional
cAssignInitializerOptionalP =
  parserToPIParser $
  CAssignInitializerOptional <$> (singleP LAssign *> cInitializerP) <|>
  pure CAssignInitializerOptionalEmpty

cStructDeclarationP :: PIParser CStructDeclaration
cStructDeclarationP =
  parserToPIParser $
  liftA2
    CStructDeclaration
    (afterP cSpecifierQualifierListP after)
    after
  where after = cStructDeclaratorListP <* singleP LSemiColon

cSpecifierQualifierListP :: PIParser CSpecifierQualifierList
cSpecifierQualifierListP =
  parserToPIParser $
  liftA2
    CSpecifierQualifierListSpecifier
    cTypeSpecifierP
    (optionalParser cSpecifierQualifierListP) <|>
  liftA2
    CSpecifierQualifierListQualifier
    cTypeQualifierP
    (optionalParser cSpecifierQualifierListP)

cStructDeclaratorListP :: PIParser CStructDeclaratorList
cStructDeclaratorListP =
  parserToPIParser $
  liftA2
    CStructDeclaratorList
    cStructDeclaratorP
    cStructDeclaratorListP'

cStructDeclaratorListP' :: PIParser CStructDeclaratorList'
cStructDeclaratorListP' =
  parserToPIParser $
  liftA2
    CStructDeclaratorList'
    (singleP LComma *> cStructDeclaratorP)
    cStructDeclaratorListP' <|>
  pure
    CStructDeclaratorList'Empty

cStructDeclaratorP :: PIParser CStructDeclarator
cStructDeclaratorP =
  parserToPIParser $
  liftA2
    CStructDeclaratorField
    (optionalParser cDeclaratorP)
    (singleP LColon *> cConstantExpressionP) <|>
  CStructDeclarator <$> cDeclaratorP

cEnumSpecifierP :: PIParser CEnumSpecifier
cEnumSpecifierP =
  parserToPIParser $
  liftA2
    CEnumSpecifierList
    (singleP LEnum *> optionalParser cIdentifierP)
    (braceP cEnumeratorListP) <|>
  CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)

cEnumeratorListP :: PIParser CEnumeratorList
cEnumeratorListP =
  parserToPIParser $ liftA2 CEnumeratorList cEnumeratorP cEnumeratorListP'

cEnumeratorListP' :: PIParser CEnumeratorList'
cEnumeratorListP' =
  parserToPIParser $
  liftA2
    CEnumeratorList'
    (singleP LComma *> cEnumeratorP)
    cEnumeratorListP' <|>
  pure
    CEnumeratorList'Empty

cEnumeratorP :: PIParser CEnumerator
cEnumeratorP =
  parserToPIParser $
  liftA2
    CEnumeratorAssign
    cIdentifierP
    (singleP LAssign *> cConstantExpressionP) <|>
  fmap
    CEnumerator
    cIdentifierP

cDeclaratorP :: PIParser CDeclarator
cDeclaratorP =
  parserToPIParser $ liftA2 CDeclarator (optionalParser cPointerP) cDirectDeclaratorP

cDirectDeclaratorP :: PIParser CDirectDeclarator
cDirectDeclaratorP =
  parserToPIParser $
  liftA2
    CDirectDeclaratorId
    cIdentifierP
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclaratorParen
    (parenthesisP cDeclaratorP)
    cDirectDeclaratorP'

cDirectDeclaratorP' :: PIParser CDirectDeclarator'
cDirectDeclaratorP' =
  parserToPIParser $
  liftA2
    CDirectDeclarator'ConstExpr
    (bracketP (optionalParser cConstantExpressionP))
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclarator'IdList
    (parenthesisP (optionalParser cIdentifierListP))
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclarator'ParamTypeList
    (parenthesisP cParameterTypeListP)
    cDirectDeclaratorP' <|>
  pure
    CDirectDeclarator'Empty

cPointerP :: PIParser CPointer
cPointerP =
  parserToPIParser $
  liftA2
    CPointer
    (singleP LStar *> (optionalParser cTypeQualifierListP))
    (optionalParser cPointerP)

cTypeQualifierListP :: PIParser CTypeQualifierList
cTypeQualifierListP =
  parserToPIParser $
  liftA2
    CTypeQualifierList
    cTypeQualifierP
    (optionalParser cTypeQualifierListP)

cParameterTypeListP :: PIParser CParameterTypeList
cParameterTypeListP =
  parserToPIParser $
  liftA2
    CParameterTypeList
    cParameterListP
    cVarArgsOptionalP

cVarArgsOptionalP :: PIParser CVarArgsOptional
cVarArgsOptionalP =
  parserToPIParser $
  CVarArgsOptional <$ singleP LComma <* singleP LVarargs <|>
  pure CVarArgsOptionalEmpty

cParameterListP :: PIParser CParameterList
cParameterListP =
  parserToPIParser $
  liftA2
    CParameterList
    cParameterDeclarationP
    cParameterListP'

cParameterListP' :: PIParser CParameterList'
cParameterListP' =
  parserToPIParser $
  liftA2
    CParameterList'
    (singleP LComma *> cParameterDeclarationP)
    cParameterListP' <|>
  pure
    CParameterList'Empty

cParameterDeclarationP :: PIParser CParameterDeclaration
cParameterDeclarationP =
  parserToPIParser $
  liftA2
    CParameterDeclaration
    (afterP cDeclarationSpecifiersP cParameterDeclarationP')
    cParameterDeclarationP'

cParameterDeclarationP' :: PIParser CParameterDeclaration'
cParameterDeclarationP' =
  parserToPIParser $
  CParameterDeclaration' <$> cDeclaratorP <|>
  CParameterDeclaration'Abstract <$> (optionalParser cAbstractDeclaratorP)

cIdentifierListP :: PIParser CIdentifierList
cIdentifierListP =
  parserToPIParser $ liftA2 CIdentifierList cIdentifierP cIdentifierListP'

cIdentifierListP' :: PIParser CIdentifierList'
cIdentifierListP' =
  parserToPIParser $
  liftA2
    CIdentifierList'
    (singleP LComma *> cIdentifierP)
    cIdentifierListP' <|>
  pure
    CIdentifierList'Empty

cInitializerP :: PIParser CInitializer
cInitializerP =
  parserToPIParser $
  CInitializerAssignment <$> cAssignmentExpressionP <|>
  CInitializerInitList <$>
  bracketP (cInitializerListP <* (singleP LComma <|> pure LComma))

cInitializerListP :: PIParser CInitializerList
cInitializerListP =
  parserToPIParser $
  liftA2
    CInitializerList
    cInitializerP
    cInitializerListP'

cInitializerListP' :: PIParser CInitializerList'
cInitializerListP' =
  parserToPIParser $
  liftA2
    CInitializerList'
    (singleP LComma *> cInitializerP)
    cInitializerListP' <|>
  pure
    CInitializerList'Empty

cTypeNameP :: PIParser CTypeName
cTypeNameP =
  parserToPIParser $
  liftA2
    CTypeName
    cSpecifierQualifierListP
    (optionalParser cAbstractDeclaratorP)

cAbstractDeclaratorP :: PIParser CAbstractDeclarator
cAbstractDeclaratorP =
  parserToPIParser $
  liftA2
    CAbstractDeclaratorDirect
    (optionalParser cPointerP)
    cDirectAbstractDeclaratorP <|>
  CAbstractDeclaratorPointer <$> cPointerP

cDirectAbstractDeclaratorP :: PIParser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP =
  parserToPIParser $
  liftA2
    CDirectAbstractDeclaratorParen
    (parenthesisP cAbstractDeclaratorP)
    cDirectAbstractDeclaratorP' <|>
  liftA2
    CDirectAbstractDeclaratorIndexed
    (braceP (optionalParser cConstantExpressionP))
    cDirectAbstractDeclaratorP' <|>
  liftA2
    CDirectAbstractDeclaratorParams
    (braceP (optionalParser cParameterTypeListP))
    cDirectAbstractDeclaratorP'

cDirectAbstractDeclaratorP' :: PIParser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parserToPIParser $
  liftA2
    CDirectAbstractDeclarator'Const
    (bracketP (optionalParser cConstantExpressionP))
    cDirectAbstractDeclaratorP' <|>
  liftA2
    CDirectAbstractDeclarator'Params
    (parenthesisP (optionalParser cParameterTypeListP))
    cDirectAbstractDeclaratorP' <|>
  pure
    CDirectAbstractDeclarator'Empty

cTypedefNameP :: PIParser CTypedefName
cTypedefNameP =
  parserToPIParser $ CTypedefName <$> cIdentifierP

cStatementP :: PIParser CStatement
cStatementP =
  parserToPIParser $
  CStatementLabeled <$> cLabeledStatementP <|>
  CStatementExpression <$> cExpressionStatementP <|>
  CStatementCompound <$> cCompoundStatementP <|>
  CStatementSelection <$> cSelectionStatementP <|>
  CStatementIteration <$> cIterationStatementP <|>
  CStatementJump <$> cJumpStatementP

cLabeledStatementP :: PIParser CLabeledStatement
cLabeledStatementP =
  parserToPIParser $
  liftA2
    CLabeledStatementId
    cIdentifierP
    (singleP LColon *> cStatementP) <|>
  liftA2
    CLabeledStatementCase
    (singleP LCase *> cConstantExpressionP)
    (singleP LColon *> cStatementP) <|>
  fmap
    CLabeledStatementDefault
    (singleP LDefault *> singleP LColon *> cStatementP)

cExpressionStatementP :: PIParser CExpressionStatement
cExpressionStatementP =
  parserToPIParser $
  fmap
    CExpressionStatement
    ((optionalParser cExpressionP) <* singleP LSemiColon)

cCompoundStatementP :: PIParser CCompoundStatement
cCompoundStatementP =
  braceP $
  parserToPIParser $
  liftA2
    CCompoundStatement
    (afterP (optionalParser cDeclarationListP) (optionalParser cStatementListP))
    (optionalParser cStatementListP)

cStatementListP :: PIParser CStatementList
cStatementListP =
  parserToPIParser $
  liftA2
    CStatementList
    cStatementP
    (optionalParser cStatementListP)

cSelectionStatementP :: PIParser CSelectionStatement
cSelectionStatementP =
  parserToPIParser $
  liftA3
    CSelectionStatementIf
    (singleP LIf *> parenthesisP cExpressionP)
    cStatementP
    cElseOptionalP <|>
  liftA2
    CSelectionStatementSwitch
    (singleP LSwitch *> parenthesisP cExpressionP)
    cStatementP

cElseOptionalP :: PIParser CElseOptional
cElseOptionalP =
  parserToPIParser $
  CElseOptional <$> (singleP LElse *> cStatementP) <|>
  pure CElseOptionalEmpty

cIterationStatementP :: PIParser CIterationStatement
cIterationStatementP =
  parserToPIParser $
  liftA2
    CIterationStatementWhile
    (singleP LWhile *> parenthesisP cExpressionP)
    cStatementP <|>
  liftA2
    CIterationStatementDoWhile
    (singleP LDo *> cStatementP)
    (singleP LWhile *> parenthesisP cExpressionP <* singleP LSemiColon) <|>
  CIterationStatementFor <$>
  (singleP LFor *> singleP LParenthesisOpen *> (optionalParser cExpressionP)) <*>
  (singleP LSemiColon *> (optionalParser cExpressionP)) <*>
  (singleP LSemiColon *> (optionalParser cExpressionP) <* singleP LParenthesisClose) <*>
  cStatementP

cJumpStatementP :: PIParser CJumpStatement
cJumpStatementP =
  parserToPIParser $
  CJumpStatementGoto <$> (singleP LGoto *> cIdentifierP <* singleP LSemiColon) <|>
  CJumpStatementContinue <$ singleP LContinue <* singleP LSemiColon <|>
  CJumpStatementBreak <$ singleP LBreak <* singleP LSemiColon <|>
  CJumpStatementReturn <$> (singleP LReturn *> (optionalParser cExpressionP) <* singleP LSemiColon)

cExpressionP :: PIParser CExpression
cExpressionP =
  parserToPIParser $
  liftA2
    CExpression
    cAssignmentExpressionP
    cExpressionP'

cExpressionP' :: PIParser CExpression'
cExpressionP' =
  parserToPIParser $
  liftA2
    CExpression'
    (singleP LComma *> cAssignmentExpressionP)
    cExpressionP' <|>
  pure
    CExpression'Empty

cAssignmentExpressionP :: PIParser CAssignmentExpression
cAssignmentExpressionP =
  parserToPIParser $
  liftA3
    CAssignmentExpression
    cUnaryExpressionP
    cAssignmentOperatorP
    cAssignmentExpressionP <|>
  fmap
    CAssignmentExpressionConditional
    cConditionalExpressionP

cAssignmentOperatorP :: PIParser CAssignmentOperator
cAssignmentOperatorP =
  parserToPIParser $
  CAssignmentOperatorAssign <$ singleP LAssign <|>
  CAssignmentOperatorMul <$ singleP LMultiplicationAssign <|>
  CAssignmentOperatorDiv <$ singleP LDivisionAssign <|>
  CAssignmentOperatorMod <$ singleP LModuloAssign <|>
  CAssignmentOperatorAdd <$ singleP LPlusAssign <|>
  CAssignmentOperatorSub <$ singleP LMinusAssign <|>
  CAssignmentOperatorLShift <$ singleP LBitShiftLeftAssign <|>
  CAssignmentOperatorRShfit <$ singleP LBitShiftRightAssign <|>
  CAssignmentOperatorAnd <$ singleP LBitwiseAndAssign <|>
  CAssignmentOperatorXor <$ singleP LBitwiseXorAssign <|>
  CAssignmentOperatorOr <$ singleP LBitwiseOrAssign

cConditionalExpressionP :: PIParser CConditionalExpression
cConditionalExpressionP =
  parserToPIParser $
  liftA2
    CConditionalExpression
    cLogicalOrExpressionP
    cTernaryOptionalP

cTernaryOptionalP :: PIParser CTernaryOptional
cTernaryOptionalP =
  parserToPIParser $
  liftA2
    CTernaryOptional
    (singleP LTernary *> cExpressionP)
    (singleP LColon *> cConditionalExpressionP) <|>
  pure
    CTernaryOptionalEmpty

cConstantExpressionP :: PIParser CConstantExpression
cConstantExpressionP =
  parserToPIParser $ CConstantExpression <$> cConditionalExpressionP

cLogicalOrExpressionP :: PIParser CLogicalOrExpression
cLogicalOrExpressionP =
  parserToPIParser $
  liftA2
    CLogicalOrExpression
    cLogicalAndExpressionP
    cLogicalOrExpressionP'

cLogicalOrExpressionP' :: PIParser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parserToPIParser $
  liftA2
    CLogicalOrExpression'
    (singleP LOr *> cLogicalAndExpressionP)
    cLogicalOrExpressionP' <|>
  pure
    CLogicalOrExpression'Empty

cLogicalAndExpressionP :: PIParser CLogicalAndExpression
cLogicalAndExpressionP =
  parserToPIParser $
  liftA2
    CLogicalAndExpression
    cInclusiveOrExpressionP
    cLogicalAndExpressionP'

cLogicalAndExpressionP' :: PIParser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parserToPIParser $
  liftA2
    CLogicalAndExpression'
    (singleP LAnd *> cInclusiveOrExpressionP)
    cLogicalAndExpressionP' <|>
  pure
    CLogicalAndExpression'Empty

cInclusiveOrExpressionP :: PIParser CInclusiveOrExpression
cInclusiveOrExpressionP =
  parserToPIParser $
  liftA2
    CInclusiveOrExpression
    cExclusiveOrExpressionP
    cInclusiveOrExpressionP'

cInclusiveOrExpressionP' :: PIParser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parserToPIParser $
  liftA2
    CInclusiveOrExpression'
    (singleP LBitwiseOr *> cExclusiveOrExpressionP)
    cInclusiveOrExpressionP' <|>
  pure
    CInclusiveOrExpression'Empty

cExclusiveOrExpressionP :: PIParser CExclusiveOrExpression
cExclusiveOrExpressionP =
  parserToPIParser $
  liftA2
    CExclusiveOrExpression
    cAndExpressionP
    cExclusiveOrExpressionP'

cExclusiveOrExpressionP' :: PIParser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parserToPIParser $
  liftA2
    CExclusiveOrExpression'
    (singleP LBitwiseXor *> cAndExpressionP)
    cExclusiveOrExpressionP' <|>
  pure
    CExclusiveOrExpression'Empty

cAndExpressionP :: PIParser CAndExpression
cAndExpressionP =
  parserToPIParser $
  liftA2
    CAndExpression
    cEqualityExpressionP
    cAndExpressionP'

cAndExpressionP' :: PIParser CAndExpression'
cAndExpressionP' =
  parserToPIParser $
  liftA2
    CAndExpression'
    (singleP LAmp *> cEqualityExpressionP)
    cAndExpressionP' <|>
  pure
    CAndExpression'Empty

cEqualityExpressionP :: PIParser CEqualityExpression
cEqualityExpressionP =
  parserToPIParser $
  liftA2
    CEqualityExpression
    cRelationalExpressionP
    cEqualityExpressionP'

cEqualityExpressionP' :: PIParser CEqualityExpression'
cEqualityExpressionP' =
  parserToPIParser $
  liftA2
    CEqualityExpression'EQ
    (singleP LEquals *> cRelationalExpressionP)
    cEqualityExpressionP' <|>
  liftA2
    CEqualityExpression'NEQ
    (singleP LNotEquals *> cRelationalExpressionP)
    cEqualityExpressionP' <|>
  pure
    CEqualityExpression'Empty

cRelationalExpressionP :: PIParser CRelationalExpression
cRelationalExpressionP =
  parserToPIParser $
  liftA2
    CRelationalExpression
    cShiftExpressionP
    cRelationalExpressionP'

cRelationalExpressionP' :: PIParser CRelationalExpression'
cRelationalExpressionP' =
  parserToPIParser $
  liftA2
    CRelationalExpression'LT
    (singleP LLT *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  liftA2
    CRelationalExpression'LTE
    (singleP LLTE *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  liftA2
    CRelationalExpression'GT
    (singleP LGT *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  liftA2
    CRelationalExpression'GT
    (singleP LGTE *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  pure
    CRelationalExpression'Empty

cShiftExpressionP :: PIParser CShiftExpression
cShiftExpressionP =
  parserToPIParser $
  liftA2
    CShiftExpression
    cAdditiveExpressionP
    cShiftExpressionP'

cShiftExpressionP' :: PIParser CShiftExpression'
cShiftExpressionP' =
  parserToPIParser $
  liftA2
    CShiftExpression'Left
    (singleP LBitShiftLeft *> cAdditiveExpressionP)
    cShiftExpressionP' <|>
  liftA2
    CShiftExpression'Right
    (singleP LBitShiftRight *> cAdditiveExpressionP)
    cShiftExpressionP' <|>
  pure
    CShiftExpression'Empty

cAdditiveExpressionP :: PIParser CAdditiveExpression
cAdditiveExpressionP =
  parserToPIParser $
  liftA2
    CAdditiveExpression
    cMultiplicativeExpressionP
    cAdditiveExpressionP'

cAdditiveExpressionP' :: PIParser CAdditiveExpression'
cAdditiveExpressionP' =
  parserToPIParser $
  liftA2
    CAdditiveExpression'Add
    (singleP LPlus *> cMultiplicativeExpressionP)
    cAdditiveExpressionP' <|>
  liftA2
    CAdditiveExpression'Sub
    (singleP LMinus *> cMultiplicativeExpressionP)
    cAdditiveExpressionP' <|>
  pure
    CAdditiveExpression'Empty

cMultiplicativeExpressionP :: PIParser CMultiplicativeExpression
cMultiplicativeExpressionP =
  parserToPIParser $
  liftA2
    CMultiplicativeExpression
    cCastExpressionP
    cMultiplicativeExpressionP'

cMultiplicativeExpressionP' :: PIParser CMultiplicativeExpression'
cMultiplicativeExpressionP' =
  parserToPIParser $
  liftA2
    CMultiplicativeExpression'Mul
    (singleP LStar *> cCastExpressionP)
    cMultiplicativeExpressionP' <|>
  liftA2
    CMultiplicativeExpression'Div
    (singleP LDivision *> cCastExpressionP)
    cMultiplicativeExpressionP' <|>
  liftA2
    CMultiplicativeExpression'Mod
    (singleP LModulo *> cCastExpressionP)
    cMultiplicativeExpressionP' <|>
  pure
    CMultiplicativeExpression'Empty

cCastExpressionP :: PIParser CCastExpression
cCastExpressionP =
  parserToPIParser $
  liftA2
    CCastExpression
    (parenthesisP cTypeNameP)
    cCastExpressionP <|>
  fmap
    CCastExpressionUnary
    cUnaryExpressionP

cUnaryExpressionP :: PIParser CUnaryExpression
cUnaryExpressionP =
  parserToPIParser $
  CUnaryExpressionInc <$> (singleP LIncrement *> cUnaryExpressionP) <|>
  CUnaryExpressionDec <$> (singleP LDecrement *> cUnaryExpressionP) <|>
  CUnaryExpressionSizeofType <$> (singleP LSizeof *> parenthesisP cTypeNameP) <|>
  CUnaryExpressionSizeof <$> (singleP LSizeof *> cUnaryExpressionP) <|>
  CUnaryExpressionUnaryOp <$> cUnaryOperatorP <*> cCastExpressionP <|>
  CUnaryExpressionPostfix <$> cPostfixExpressionP

cUnaryOperatorP :: PIParser CUnaryOperator
cUnaryOperatorP =
  parserToPIParser $
  CUnaryOperatorAnd <$ singleP LAmp <|>
  CUnaryOperatorMul <$ singleP LStar <|>
  CUnaryOperatorAdd <$ singleP LPlus <|>
  CUnaryOperatorSub <$ singleP LMinus <|>
  CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot <|>
  CUnaryOperatorNot <$ singleP LNot

cPostfixExpressionP :: PIParser CPostfixExpression
cPostfixExpressionP =
  parserToPIParser $
  liftA2
    CPostfixExpression
    cPrimaryExpressionP
    cPostfixExpressionP'

cPostfixExpressionP' :: PIParser CPostfixExpression'
cPostfixExpressionP' =
  parserToPIParser $
  liftA2
    CPostfixExpression'Bracket
    (bracketP cExpressionP)
    cPostfixExpressionP' <|>
  liftA2
    CPostfixExpression'Paren
    (parenthesisP (optionalParser cArgumentExpressionListP))
    cPostfixExpressionP' <|>
  liftA2
    CPostfixExpression'Dot
    (singleP LDot *> cIdentifierP)
    cPostfixExpressionP' <|>
  liftA2
    CPostfixExpression'Arrow
    (singleP LArrow *> cIdentifierP)
    cPostfixExpressionP' <|>
  fmap
    CPostfixExpression'Inc
    (singleP LIncrement *> cPostfixExpressionP') <|>
  fmap
    CPostfixExpression'Dec
    (singleP LDecrement *> cPostfixExpressionP') <|>
  pure
    CPostfixExpression'Empty

cPrimaryExpressionP :: PIParser CPrimaryExpression
cPrimaryExpressionP =
  parserToPIParser $
  CPrimaryExpressionId <$> cIdentifierP <|>
  CPrimaryExpressionConst <$> cConstantP <|>
  CPrimaryExpressionString <$> stringLiteralP <|>
  CPrimaryExpressionParen <$> parenthesisP cExpressionP

cArgumentExpressionListP :: PIParser CArgumentExpressionList
cArgumentExpressionListP =
  parserToPIParser $
  liftA2
    CArgumentExpressionList
    cAssignmentExpressionP
    cArgumentExpressionListP'

cArgumentExpressionListP' :: PIParser CArgumentExpressionList'
cArgumentExpressionListP' =
  parserToPIParser $
  liftA2
    CArgumentExpressionList'
    (singleP LComma *> cAssignmentExpressionP)
    cArgumentExpressionListP' <|>
  pure
    CArgumentExpressionList'Empty

cConstantP :: PIParser CConstant
cConstantP =
  parserToPIParser $
  CConstantInt <$> intLiteralP <|>
  CConstantChar <$> charLiteralP <|>
  CConstantFloat <$> floatLiteralP <|>
  CConstantEnum <$> cIdentifierP

