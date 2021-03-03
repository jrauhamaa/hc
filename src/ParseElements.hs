module ParseElements where

-- TODO: eliminate left recursion
newtype CIdentifier = CIdentifier String

data CConstant
  -- float literal
  = CConstantFloat Double
  -- integer literal
  | CConstantInteger Int
  -- CEnumerationConstant
  | CConstantEnumeration CEnumerationConstant
  -- character literal
  | CConstantCharacter Char

data CPrimaryExpression
  = CPrimaryExpressionId CIdentifier
  | CPrimaryExpressionConst CConstant
  | CPrimaryExpressionStr String
  | CPrimaryExpressionParen CExpression

data CPostfixExpression
  -- CPrimaryExpression
  = CPostfixExpressionEmpty CPrimaryExpression
  -- CPostfixExpression [ CExpression ]
  | CPostfixExpressionIndexed CPostfixExpression CExpression
  -- CPostfixExpression ( CArgumentExpressionList (optional) )
  | CPostfixExpressionArgList CPostfixExpression CArgumentExpressionList
  -- CPostfixExpression . CIdentifier
  | CPostfixExpressionStructField CPostfixExpression CIdentifier
  -- CPostfixExpression -> CIdentifier
  | CPostfixExpressionStructPointer CPostfixExpression CIdentifier
  -- CPostfixExpression ++
  | CPostfixExpressionIncrement CPostfixExpression
  -- CPostfixExpression --
  | CPostfixExpressionDecrement CPostfixExpression

data CArgumentExpressionList
  -- CAssignmentExpression
  = CArgumentExpressionListEmpty CAssignmentExpression
  -- CArgumentExpressionList . CAssignmentExpression
  | CArgumentExpressionListMulti CArgumentExpressionList CAssignmentExpression

data CUnaryExpression
  -- CPostfixExpression
  = CUnaryExpressionEmpty CPostfixExpression
  -- ++ CUnaryExpression
  | CUnaryExpressionIncr
  -- -- CUnaryExpression
  | CUnaryExpressionDecr
  -- CUnaryOperator CCastExpression
  | CUnaryExpressionCast CUnaryOperator CCastExpression
  -- sizeof CUnaryExpression
  | CUnaryExpressionSizeof CUnaryExpression
  -- sizeof ( CTypeName )
  | CUnaryExpressionSizeofType CTypeName

data CUnaryOperator
  = CUnaryOperatorAddress       -- &
  | CUnaryOperatorMultiply      -- *
  | CUnaryOperatorPlus          -- +
  | CUnaryOperatorMinus         -- -
  | CUnaryOperatorBitwiseNot    -- ~
  | CUnaryOperatorNot           -- !

data CCastExpression
  -- CUnaryExpression
  = CCastExpressionEmpty CUnaryExpression
  -- ( CTypeName ) CCastExpression
  | CCastExpressionCast CTypeName CCastExpression

data CMultiplicativeExpression
  -- CCastExpression
  = CMultiplicativeExpressionEmpty CCastExpression
  -- CMultiplicativeExpression * CCastExpression
  | CMultiplicativeExpressionMult CMultiplicativeExpression CCastExpression
  -- CMultiplicativeExpression / CCastExpression
  | CMultiplicativeExpressionDiv CMultiplicativeExpression CCastExpression
  -- CMultiplicativeExpression % CCastExpression
  | CMultiplicativeExpressionMod CMultiplicativeExpression CCastExpression

data CAdditiveExpression
  -- CMultiplicativeExpression
  = CAdditiveExpressionEmpty CMultiplicativeExpression
  -- CAdditiveExpression + CMultiplicativeExpression
  | CAdditiveExpressionPlus CAdditiveExpression CMultiplicativeExpression
  -- CAdditiveExpression - CMultiplicativeExpression
  | CAdditiveExpressionMinus CAdditiveExpression CMultiplicativeExpression

data CShiftExpression
  -- CAdditiveExpression
  = CShiftExpressionEmpty CAdditiveExpression
  -- CShiftExpression << CAdditiveExpression
  | CShiftExpressionLeft CShiftExpression CAdditiveExpression
  -- CShiftExpression >> CAdditiveExpression
  | CShiftExpressionRight CShiftExpression CAdditiveExpression

data CRelationalExpression
  -- CShiftExpression
  = CRelationalExpressionEmpty CShiftExpression
  -- CRelationalExpression < CShiftExpression
  | CRelationalExpressionLT CRelationalExpression CShiftExpression
  -- CRelationalExpression > CShiftExpression
  | CRelationalExpressionGT CRelationalExpression CShiftExpression
  -- CRelationalExpression <= CShiftExpression
  | CRelationalExpressionLTE CRelationalExpression CShiftExpression
  -- CRelationalExpression >= CShiftExpression
  | CRelationalExpressionGTE CRelationalExpression CShiftExpression

data CEqualityExpression
  -- CRelationalExpression
  = CEqualityExpressionEmpty CRelationalExpression
  -- CEqualityExpression == CRelationalExpression
  | CEqualityExpressionEQ CEqualityExpression CRelationalExpression
  -- CEqualityExpression != CRelationalExpression
  | CEqualityExpressionNEQ CEqualityExpression CRelationalExpression

data CAndExpression
  -- CEqualityExpression
  = CAndExpressionEmpty CEqualityExpression
  -- CAndExpression & CEqualityExpression
  | CAndExpression CAndExpression CEqualityExpression

data CExclusiveOrExpression
  -- CAndExpression
  = CExclusiveOrExpressionEmpty CAndExpression
  -- CExclusiveOrExpression ^ CAndExpression
  | CExclusiveOrExpression CExclusiveOrExpression CAndExpression

data CInclusiveOrExpression
  -- CExclusiveOrExpression
  = COrExpressionEmpty CExclusiveOrExpression
  -- CInclusiveOrExpression | CExclusiveOrExpression
  | COrExpression CInclusiveOrExpression CExclusiveOrExpression

data CLogicalAndExpression
  -- CInclusiveOrExpression
  = CLogicalAndExpressionEmpty CInclusiveOrExpression
  -- CLogicalAndExpression && CInclusiveOrExpression
  | CLogicalAndExpression CLogicalAndExpression CInclusiveOrExpression

data CLogicalOrExpression
  -- CLogicalAndExpression
  = CLogicalOrExpressionEmpty CLogicalAndExpression
  -- CLogicalOrExpression || CLogicalAndExpression
  | CLogicalOrExpression CLogicalOrExpression CLogicalAndExpression

data CConditionalExpression
  -- CLogicalOrExpression
  = CConditionalExpressionEmpty CLogicalOrExpression
  -- CLogicalOrExpression ? CExpression : CConditionalExpression
  | CConditionalExpression
      CLogicalOrExpression
      CExpression
      CConditionalExpression

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionEmpty CConditionalExpression
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpression
      CUnaryExpression
      CAssignmentOperator
      CAssignmentExpression

data CAssignmentOperator
  = CAssignmentOperatorAssign   -- =
  | CAssignmentOperatorMul      -- *=
  | CAssignmentOperatorDiv      -- /=
  | CAssignmentOperatorMod      -- %=
  | CAssignmentOperatorAdd      -- +=
  | CAssignmentOperatorSub      -- -=
  | CAssignmentOperatorLShift   -- <<=
  | CAssignmentOperatorRShfit   -- >>=
  | CAssignmentOperatorAnd      -- &=
  | CAssignmentOperatorXor      -- ^=
  | CAssignmentOperatorOr       -- |=

data CExpression
  -- CAssignmentExpression
  = CExpressionEmpty CAssignmentExpression
  -- CExpression , CAssignmentExpression
  | CExpression CExpression CAssignmentExpression

-- CConditionalExpression
newtype CConstantExpression = CConstantExpression CConditionalExpression

data CDeclaration
  -- CDeclarationSpecifiers CInitDeclaratorList (optional)
      =
  CDeclaration CDeclarationSpecifiers CInitDeclaratorList

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclarationSpecifiersStorageCLass
      CStorageClassSpecifier
      CDeclarationSpecifiers
  -- CTypeSpecifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeSpecifier CTypeSpecifier CDeclarationSpecifiers
  -- CTypeQualifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeQualifier CTypeQualifier CDeclarationSpecifiers

data CInitDeclaratorList
  -- CInitDeclarator
  = CInitDeclaratorListSingleton CInitDeclarator
  -- CInitDeclaratorList , CInitDeclarator
  | CInitDeclaratorList CInitDeclaratorList CInitDeclarator

data CInitDeclarator
  -- CDeclarator
  = CInitDeclaratorEmpty CDeclarator
  -- CDeclarator = CInitializer
  | CInitDeclarator CDeclarator CInitializer

data CStorageClassSpecifier
  = CStorageClassSpecifierTypedef
  | CStorageClassSpecifierExtern    -- extern
  | CStorageClassSpecifierStatic    -- static
  | CStorageClassSpecifierAuto      -- auto
  | CStorageClassSpecifierRegister  -- register

data CTypeSpecifier
  = CTypeSpecifierVoid          -- void
  | CTypeSpecifierChar          -- char
  | CTypeSpecifierShort         -- short
  | CTypeSpecifierInt           -- int
  | CTypeSpecifierLong          -- long
  | CTypeSpecifierFloat         -- float
  | CTypeSpecifierDouble        -- double
  | CTypeSpecifierSigned        -- signed
  | CTypeSpecifierUnsigned      -- unsigned
  -- CStructOrUnionSpecifier
  | CTypeSpecifierStructOrUnion CStructOrUnionSpecifier
  -- CEnumSpecifier
  | CTypeSpecifierEnum CEnumSpecifier
  -- CTypedefName
  | CTypeSpecifierTypedef CTypedefName

data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifier (optional) { CStructDeclarationList }
  = CStructOrUnionSpecifierList
      CStructOrUnion
      CIdentifier
      CStructDeclarationList
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier CStructOrUnion CIdentifier

data CStructOrUnion
  = CStruct     -- struct
  | CUnion      -- union

data CStructDeclarationList -- NOTE: different from CStructDeclaratorList
  -- CStructDeclaration
  = CStructDeclarationListSingleton CStructDeclaration
  -- CStructDeclarationList CStructDeclaration
  | CStructDeclarationList CStructDeclarationList CStructDeclaration

data CStructDeclaration -- NOTE: different from CStructDeclarator
      =
  CStructDeclaration CSpecifierQualifierList CStructDeclarationList

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierList (optional)
  = CSpecifierQualifierListSpecifier CTypeSpecifier CSpecifierQualifierList
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecifierQualifierListQualifier CTypeQualifier CSpecifierQualifierList

data CStructDeclaratorList -- NOTE: different from CStructDeclarationList
  -- CStructDeclarator
  = CStructDeclaratorListSingleton CStructDeclarator
  -- CStructDeclaratorList CStructDeclarator
  | CStructDeclaratorList CStructDeclaratorList CStructDeclarator

data CStructDeclarator -- NOTE: different from CStructDeclaration
  -- CDeclarator
  = CStructDeclarator CDeclarator
  -- CDeclarator (optional) : CConstantExpression
  | CStructDeclaratorInit CDeclarator CConstantExpression

data CEnumSpecifier
  -- enum CIdentifier (optional) { CEnumeratorList }
  = CEnumSpecifierList CIdentifier CEnumeratorList
  -- enum CIdentifier
  | CEnumSpecifier CIdentifier

data CEnumeratorList
  -- CEnumerator
  = CEnumeratorListSingleton CEnumerator
  -- CEnumeratorList , CEnumerator
  | CEnumeratorList CEnumerator

data CEnumerator
  -- CEnumerationConstant
  = CEnumerator CEnumerationConstant
  -- CEnumerationConstant = CConstantExpression
  | CEnumeratorAssign CEnumerationConstant CConstantExpression

-- CIdentifier
newtype CEnumerationConstant = CEnumerationConstant CIdentifier

data CTypeQualifier
  = CTypeQualifierConst         -- const
  | CTypeQualifierVolatile      -- volatile

data CDeclarator
  -- CPointer (optional) CDirectDeclarator
      =
  CDeclarator CPointer CDirectDeclarator

data CDirectDeclarator
  -- CIdentifier
  = CDirectDeclaratorId CIdentifier
  -- ( CDeclarator )
  | CDirectDeclaratorParen CDeclarator
  -- CDirectDeclarator [ CConstantExpression (optional) ]
  | CDirectDeclarator CConstantExpression
  -- CDirectDeclarator ( CParameterTypeList )
  | CDirectDeclaratorParamTypeList CDirectDeclarator CParameterTypeList
  -- CDirectDeclarator ( CIdentifierList (optional) )
  | CDirectDeclaratorIdList CDirectDeclarator CIdentifierList

data CPointer
  -- * CTypeQualifierList (optional)
  = CPointerSingle CTypeQualifierList
  -- * CTypeQualifierList (optional) CPointer
  | CPointerMulti CTypeQualifierList CPointer

data CTypeQualifierList
  -- CTypeQualifier
  = CTypeQualifierListSingleton CTypeQualifier
  -- CTypeQualifierList CTypeQualifier
  | CTypeQualifierList CTypeQualifierList CTypeQualifier

data CParameterTypeList
  -- CParameterList
  = CParameterTypeList CParameterList
  -- CParameterTypeList , ...
  | CParameterTypeListVarargs CParameterList

data CParameterList
  -- CParameterDeclaration
  = CParameterListSingleton CParameterDeclaration
  -- CParameterList CParameterDeclaration
  | CParameterList CParameterList CParameterDeclaration

-- CParameterList , CParameterDeclaration
data CParameterDeclaration
  = CParameterDeclaration CParameterList CParameterDeclaration

data CIdentifierList
  -- CIdentifier
  = CIdentifierListSingleton CIdentifier
  -- CIdentifierList , CIdentifier
  | CidentifierList CIdentifierList CIdentifier

-- CSpecifierQualifierList CAbstractDeclarator (optional)
data CTypeName = CTypeName CSpecifierQualifierList CAbstractDeclarator

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer CPointer
  -- CPointer (optional) CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect CPointer CDirectAbstractDeclarator

data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator )
  = CDirectAbstractDeclaratorParenthesized CAbstractDeclarator
  -- CDirectAbstractDeclarator (optional) [ CConstantExpression (optional) ]
  | CDirectAbstractDeclaratorConstant
      CDirectAbstractDeclarator
      CConstantExpression
  -- CDirectAbstractDeclarator (optional) ( CParamTypeList (optional) )
  | CDirectAbstractDeclaratorParamList
      CDirectAbstractDeclarator
      CParameterTypeList

-- CIdentifier
newtype CTypedefName = CTypedefName CIdentifier

data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment CAssignmentExpression
  -- { CInitializerList }
  | CInitializerBracketList CInitializerList
  -- { CInitializerList , }
  | CInitializerBracketListComma CInitializerList

data CInitializerList
  -- CInitializer
  = CInitializerListSingleton CInitializer
  -- CInitializerList , CInitializer
  | CInitializerList CInitializerList CInitializer

data CStatement
  -- CLabaledStatement
  = CStatementLabeled CLabaledStatement
  -- CCompoundStatement
  | CStatementCompound CCompoundStatement
  -- CExpressionStatement
  | CStatementExpression CExpressionStatement
  -- CSelectionStatement
  | CStatementSelection CSelectionStatement
  -- CIterationStatement
  | CStatementIteration CIterationStatement
  -- CJumpStatement
  | CStatementJump CJumpStatement

data CLabaledStatement
  -- CIdentifier : CStatement
  = CLabaledStatementId CIdentifier CStatement
  -- case CConstantExpression : CStatement
  | CLabaledStatementCase CConstantExpression CStatement
  -- default : CStatement
  | CLabaledStatementDefault CStatement

-- { CDeclarationList (optional) CStatementList (optional) }
data CCompoundStatement = CCompoundStatement CDeclarationList CStatementList

data CDeclarationList
  -- CDeclaration
  = CDeclarationListSingleton CDeclaration
  -- CDeclarationList CDeclaration
  | CDeclarationList CDeclarationList CDeclaration

data CStatementList
  -- CStatement
  = CStatementListSingleton CStatement
  -- CStatementList CStatement
  | CStatementList CStatementList CStatement

-- CExpression (optional) ;
newtype CExpressionStatement = CExpressionStatement CExpression

data CSelectionStatement
  -- if ( CExpression ) CStatement
  = CSelectionStatementIf CExpression CStatement
  -- if ( CExpression ) CStatement else CStatement
  | CSelectionStatementIfElse CExpression CStatement CStatement
  -- switch ( CExpression ) CStatement
  | CSelectionStatementSwitch CExpression CStatement

data CIterationStatement
  -- while ( CExpression ) CStatement
  = CIterationStatementWhile CExpression CStatement
  -- do CStatement while ( CExpression ) ;
  | CIterationStatementDoWhile CStatement CExpression
  -- for ( CExpression (optional) ; CExpression (optional) ;
  -- CExpression (optional) ) CStatement
  | CIterationStatementFor CExpression CExpression CExpression CStatement

data CJumpStatement
  -- goto CIdentifier ;
  = CJumpStatementGoto CIdentifier
  -- continue ;
  | CJumpStatementContinue
  -- break ;
  | CJumpStatementBreak
  -- return CExpression (optional) ;
  | CJumpStatementReturn CExpression

data CTranslationUnit
  -- CExternalDeclaration
  = CTranslationUnitExternal CExternalDeclaration
  -- CTranslationUnit CExternalDeclaration
  | CTranslationUnitTranslation CTranslationUnit CExternalDeclaration

data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction CFunctionDefinition
  -- CDeclaration
  | CExternalDeclaration CDeclaration

data CFunctionDefinition
  -- CDeclarationSpecifiers (optional) CDeclarator
  = CFunctionDefinitionSpecifiers CDeclarationSpecifiers CDeclarator
  -- CDeclarationList (optional) CCompoundStatement
  | CFunctionDefinitionList CDeclarationList CCompoundStatement
