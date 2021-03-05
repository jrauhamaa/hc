module ParseElements where

newtype CIdentifier =
  CIdentifier String

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
  -- CIdentifier
  = CPrimaryExpressionId CIdentifier
  -- CConstant
  | CPrimaryExpressionConst CConstant
  -- string literal
  | CPrimaryExpressionStr String
  -- ( CExpression )
  | CPrimaryExpressionParen CExpression

-- CPrimaryExpression CPostfixExpression'
data CPostfixExpression =
  CPostfixExpression CPrimaryExpression CPostfixExpression'

data CPostfixExpression'
  -- empty
  = CPostfixExpression'Empty
  -- [ CExpression ] CPostfixExpression'
  | CPostfixExpression'Indexed CExpression CPostfixExpression'
  -- ( CArgumentExpressionList (optional) ) CPostfixExpression'
  | CPostfixExpression'ArgList
      (Maybe CArgumentExpressionList)
      CPostfixExpression'
  -- . CIdentifier CPostfixExpression'
  | CPostfixExpression'StructField CIdentifier CPostfixExpression'
  -- -> CIdentifier CPostfixExpression'
  | CPostfixExpression'StructPointer CIdentifier CPostfixExpression'
  -- ++ CPostfixExpression'
  | CPostfixExpression'Increment CPostfixExpression'
  -- -- CPostfixExpression'
  | CPostfixExpression'Decrement CPostfixExpression'

-- CAssignmentExpression CArgumentExpressionList'
data CArgumentExpressionList =
  CArgumentExpressionList CAssignmentExpression CArgumentExpressionList'

data CArgumentExpressionList'
  -- empty
  = CArgumentExpressionList'Empty
  -- . CAssignmentExpression CArgumentExpressionList'
  | CArgumentExpressionList' CAssignmentExpression CArgumentExpressionList'

-- TODO: Avoid infinite recursion here. CUnaryExpression can derive
--       CPostfixExpression which after many steps can derive CUnaryExpression.
data CUnaryExpression
  -- CPostfixExpression
  = CUnaryExpressionSingleton CPostfixExpression
  -- ++ CUnaryExpression
  | CUnaryExpressionIncr CUnaryExpression
  -- -- CUnaryExpression
  | CUnaryExpressionDecr CUnaryExpression
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
  = CCastExpressionSingleton CUnaryExpression
  -- ( CTypeName ) CCastExpression
  | CCastExpression CTypeName CCastExpression

-- CCastExpression CMultiplicativeExpression'
data CMultiplicativeExpression =
  CMultiplicativeExpression CCastExpression CMultiplicativeExpression'

data CMultiplicativeExpression'
  -- empty
  = CMultiplicativeExpression'Empty
  -- * CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mul CCastExpression CMultiplicativeExpression'
  -- / CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Div CCastExpression CMultiplicativeExpression'
  -- % CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mod CCastExpression CMultiplicativeExpression'

-- CMultiplicativeExpression CAdditiveExpression'
data CAdditiveExpression =
  CAdditiveExpression CMultiplicativeExpression CAdditiveExpression'

data CAdditiveExpression'
  -- empty
  = CAdditiveExpression'Empty
  -- + CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Plus CMultiplicativeExpression CAdditiveExpression'
  -- - CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Minus CMultiplicativeExpression CAdditiveExpression'

-- CAdditiveExpression CShiftExpression'
data CShiftExpression =
  CShiftExpression CAdditiveExpression CShiftExpression'

data CShiftExpression'
  -- empty
  = CShiftExpression'Empty
  -- << CAdditiveExpression CShiftExpression'
  | CShiftExpression'Left CAdditiveExpression CShiftExpression'
  -- >> CAdditiveExpression CShiftExpression'
  | CShiftExpression'Right CAdditiveExpression CShiftExpression'

-- CShiftExpression CRelationalExpression'
data CRelationalExpression =
  CRelationalExpression CShiftExpression CRelationalExpression'

data CRelationalExpression'
  -- empty
  = CRelationalExpression'Empty
  -- < CShiftExpression CRelationalExpression'
  | CRelationalExpression'LT CShiftExpression CRelationalExpression'
  -- > CShiftExpression CRelationalExpression'
  | CRelationalExpression'LTE CShiftExpression CRelationalExpression'
  -- <= CShiftExpression CRelationalExpression'
  | CRelationalExpression'GT CShiftExpression CRelationalExpression'
  -- >= CShiftExpression CRelationalExpression'
  | CRelationalExpression'GTE CShiftExpression CRelationalExpression'

-- CRelationalExpression CEqualityExpression'
data CEqualityExpression =
  CEqualityExpression CRelationalExpression CEqualityExpression'

data CEqualityExpression'
  -- empty
  = CEqualityExpression'Empty
  -- == CRelationalExpression CEqualityExpression'
  | CEqualityExpression'EQ CRelationalExpression CEqualityExpression'
  -- != CRelationalExpression CEqualityExpression'
  | CEqualityExpression'NEQ

-- CEqualityExpression CAndExpression'
data CAndExpression =
  CAndExpression CEqualityExpression CAndExpression'

data CAndExpression'
  -- empty
  = CAndExpression'Empty
  -- & CEqualityExpression CAndExpression'
  | CAndExpression' CEqualityExpression CAndExpression'

-- CAndExpression CExclusiveOrExpression'
data CExclusiveOrExpression =
  CExclusiveOrExpression CAndExpression CExclusiveOrExpression'

data CExclusiveOrExpression'
  -- empty
  = CExclusiveOrExpression'Empty
  -- ^ CAndExpression CExclusiveOrExpression'
  | CExclusiveOrExpression' CAndExpression CExclusiveOrExpression'

-- CExclusiveOrExpression CInclusiveOrExpression'
data CInclusiveOrExpression =
  CInclusiveOrExpression CExclusiveOrExpression CInclusiveOrExpression'

data CInclusiveOrExpression'
  -- empty
  = CInclusiveOrExpression'Empty
  -- | CExclusiveOrExpression CInclusiveOrExpression'
  | CInclusiveOrExpression' CExclusiveOrExpression CInclusiveOrExpression'

-- CInclusiveOrExpression CLogicalAndExpression'
data CLogicalAndExpression =
  CLogicalAndExpression CInclusiveOrExpression CLogicalAndExpression'

data CLogicalAndExpression'
  -- empty
  = CLogicalAndExpression'Empty
  -- && CInclusiveOrExpression CLogicalAndExpression'
  | CLogicalAndExpression' CInclusiveOrExpression CLogicalAndExpression'

-- CLogicalAndExpression CLogicalOrExpression'
data CLogicalOrExpression =
  CLogicalOrExpression CLogicalAndExpression CLogicalOrExpression'

data CLogicalOrExpression'
  -- empty
  = CLogicalOrExpression'Empty
  -- || CLogicalAndExpression CLogicalOrExpression'
  | CLogicalOrExpression' CLogicalAndExpression CLogicalOrExpression'

data CConditionalExpression
  -- CLogicalOrExpression
  = CConditionalExpressionSingleton CLogicalOrExpression
  -- CLogicalOrExpression ? CExpression : CConditionalExpression
  | CConditionalExpression
      CLogicalOrExpression
      CExpression
      CConditionalExpression

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionSingleton CConditionalExpression
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
  = CExpressionSingleton CAssignmentExpression
  -- CExpression , CAssignmentExpression
  | CExpression CExpression CAssignmentExpression

-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression CConditionalExpression

-- CDeclarationSpecifiers CInitDeclaratorList (optional)
data CDeclaration =
  CDeclaration CDeclarationSpecifiers (Maybe CInitDeclaratorList)

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclarationSpecifiersStorageCLass
      CStorageClassSpecifier
      (Maybe CDeclarationSpecifiers)
  -- CTypeSpecifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeSpecifier
      CTypeSpecifier
      (Maybe CDeclarationSpecifiers)
  -- CTypeQualifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeQualifier
      CTypeQualifier
      (Maybe CDeclarationSpecifiers)

-- CInitDeclarator CInitDeclaratorList'
data CInitDeclaratorList =
  CInitDeclaratorList CInitDeclarator CInitDeclaratorList'

data CInitDeclaratorList'
  -- empty
  = CInitDeclaratorList'Empty
  -- , CInitDeclarator CInitDeclaratorList'
  | CInitDeclaratorList' CInitDeclarator CInitDeclaratorList'

data CInitDeclarator
  -- CDeclarator
  = CInitDeclaratorSingleton CDeclarator
  -- CDeclarator = CInitializer
  | CInitDeclarator CDeclarator CInitializer

data CStorageClassSpecifier
  = CStorageClassSpecifierTypedef   -- typedef
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
      (Maybe CIdentifier)
      CStructDeclarationList
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier CStructOrUnion CIdentifier

data CStructOrUnion
  = CStruct     -- struct
  | CUnion      -- union

-- NOTE: different from CStructDeclaratorList
data CStructDeclarationList
  -- CStructDeclaration
  = CStructDeclarationListSingleton CStructDeclaration
  -- CStructDeclaration CStructDeclarationList
  | CStructDeclarationList CStructDeclaration CStructDeclarationList

-- NOTE: different from CStructDeclarator
-- CSpecifierQualifierList CStructDeclaratorList ;
data CStructDeclaration =
  CStructDeclaration CSpecifierQualifierList CStructDeclaratorList

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierList (optional)
  = CSpecifierQualifierListSpecifier
      CTypeSpecifier
      (Maybe CSpecifierQualifierList)
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecifierQualifierListQualifier
      CTypeQualifier
      (Maybe CSpecifierQualifierList)

-- NOTE: different from CStructDeclarationList
-- CStructDeclarator CStructDeclaratorList'
data CStructDeclaratorList =
  CStructDeclaratorList CStructDeclarator CStructDeclaratorList'

data CStructDeclaratorList'
  -- empty
  = CStructDeclaratorList'Empty
  -- , CStructDeclarator CStructDeclaratorList'
  | CStructDeclaratorList' CStructDeclarator CStructDeclaratorList'

data CStructDeclarator -- NOTE: different from CStructDeclaration
  -- CDeclarator
  = CStructDeclarator CDeclarator
  -- CDeclarator (optional) : CConstantExpression
  | CStructDeclaratorInit (Maybe CDeclarator) CConstantExpression

data CEnumSpecifier
  -- enum CIdentifier (optional) { CEnumeratorList }
  = CEnumSpecifierList (Maybe CIdentifier) CEnumeratorList
  -- enum CIdentifier
  | CEnumSpecifier CIdentifier

-- CEnumerator CEnumeratorList'
data CEnumeratorList =
  CEnumeratorList CEnumerator CEnumeratorList'

data CEnumeratorList'
  -- empty
  = CEnumeratorList'Empty
  -- , CEnumerator CEnumeratorList'
  | CEnumeratorList' CEnumerator CEnumeratorList'

data CEnumerator
  -- CEnumerationConstant
  = CEnumerator CEnumerationConstant
  -- CEnumerationConstant = CConstantExpression
  | CEnumeratorAssign CEnumerationConstant CConstantExpression

-- CIdentifier
newtype CEnumerationConstant =
  CEnumerationConstant CIdentifier

data CTypeQualifier
  = CTypeQualifierConst         -- const
  | CTypeQualifierVolatile      -- volatile

-- CPointer (optional) CDirectDeclarator
data CDeclarator =
  CDeclarator (Maybe CPointer) CDirectDeclarator

data CDirectDeclarator
  -- CIdentifier CDirectDeclarator'
  = CDirectDeclaratorId CIdentifier CDirectDeclarator'
  -- ( CDeclarator ) CDirectDeclarator'
  | CDirectDeclaratorParen CDeclarator CDirectDeclarator'

data CDirectDeclarator'
  -- empty
  = CDirectDeclarator'Empty
  -- [ CConstantExpression (optional) ] CDirectDeclarator'
  | CDirectDeclarator'Indexed (Maybe CConstantExpression) CDirectDeclarator'
  -- ( CParameterTypeList ) CDirectDeclarator'
  | CDirectDeclarator'ParamTypeList CParameterTypeList CDirectDeclarator'
  -- ( CIdentifierList (optional) ) CDirectDeclarator'
  | CDirectDeclarator'IdLIst (Maybe CIdentifierList) CDirectDeclarator'

data CPointer
  -- * CTypeQualifierList (optional)
  = CPointerSingle (Maybe CTypeQualifierList)
  -- * CTypeQualifierList (optional) CPointer
  | CPointerMulti (Maybe CTypeQualifierList) CPointer

data CTypeQualifierList
  -- CTypeQualifier
  = CTypeQualifierListSingleton CTypeQualifier
  -- CTypeQualifier CTypeQualifierList
  | CTypeQualifierList CTypeQualifier CTypeQualifierList

data CParameterTypeList
  -- CParameterList
  = CParameterTypeList CParameterList
  -- CParameterTypeList , ...
  | CParameterTypeListVarargs CParameterList

-- CParameterDeclaration CParameterList'
data CParameterList =
  CParameterList CParameterDeclaration CParameterList'

data CParameterList'
  -- empty
  = CParameterList'Empty
  -- , CParameterDeclaration CParameterList'
  | CParameterList' CParameterDeclaration CParameterList'

-- CParameterList , CParameterDeclaration
data CParameterDeclaration
  -- CDeclarationSpecifiers CDeclarator
  = CParameterDeclaration CDeclarationSpecifiers CDeclarator
  -- CDeclarationSpecifiers CAbstractDeclarator (optional)
  | CParameterDeclarationAbstract
      CDeclarationSpecifiers
      (Maybe CAbstractDeclarator)

-- CIdentifier CIdentifierList'
data CIdentifierList =
  CIdentifierList CIdentifier CIdentifierList'

data CIdentifierList'
  -- empty
  = CIdentifierList'Empty
  -- , CIdentifier CIdentifierList'
  | CIdentifierList' CIdentifier CIdentifierList'

-- CSpecifierQualifierList CAbstractDeclarator (optional)
data CTypeName =
  CTypeName CSpecifierQualifierList (Maybe CAbstractDeclarator)

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer CPointer
  -- CPointer (optional) CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect (Maybe CPointer) CDirectAbstractDeclarator

-- TODO: Figure out how to implement this
data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator )
  = CDirectAbstractDeclaratorParenthesized CAbstractDeclarator
  -- CDirectAbstractDeclarator (optional) [ CConstantExpression (optional) ]
  | CDirectAbstractDeclaratorConstant
      (Maybe CDirectAbstractDeclarator)
      (Maybe CConstantExpression)
  -- CDirectAbstractDeclarator (optional) ( CParamTypeList (optional) )
  | CDirectAbstractDeclaratorParamList
      (Maybe CDirectAbstractDeclarator)
      (Maybe CParameterTypeList)

-- CIdentifier
newtype CTypedefName =
  CTypedefName CIdentifier

data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment CAssignmentExpression
  -- { CInitializerList }
  | CInitializerBracketList CInitializerList
  -- { CInitializerList , }
  | CInitializerBracketListComma CInitializerList

-- CInitializer CInitializerList'
data CInitializerList =
  CInitializerList CInitializer CInitializerList'

data CInitializerList'
  -- empty
  = CInitializerList'Empty
  -- , CInitializer CInitializerList'
  | CInitializerList' CInitializer CInitializerList'

data CStatement
  -- CLabeledStatement
  = CStatementLabeled CLabeledStatement
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

data CLabeledStatement
  -- CIdentifier : CStatement
  = CLabeledStatementId CIdentifier CStatement
  -- case CConstantExpression : CStatement
  | CLabeledStatementCase CConstantExpression CStatement
  -- default : CStatement
  | CLabeledStatementDefault CStatement

-- { CDeclarationList (optional) CStatementList (optional) }
data CCompoundStatement =
  CCompoundStatement (Maybe CDeclarationList) (Maybe CStatementList)

data CDeclarationList
  -- CDeclaration
  = CDeclarationListSingleton CDeclaration
  -- CDeclarationList CDeclaration
  | CDeclarationList CDeclaration CDeclarationList

data CStatementList
  -- CStatement
  = CStatementListSingleton CStatement
  -- CStatementList CStatement
  | CStatementList CStatement CStatementList

-- CExpression (optional) ;
newtype CExpressionStatement =
  CExpressionStatement (Maybe CExpression)

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
  | CIterationStatementFor
      (Maybe CExpression)
      (Maybe CExpression)
      (Maybe CExpression)
      CStatement

data CJumpStatement
  -- goto CIdentifier ;
  = CJumpStatementGoto CIdentifier
  -- continue ;
  | CJumpStatementContinue
  -- break ;
  | CJumpStatementBreak
  -- return CExpression (optional) ;
  | CJumpStatementReturn (Maybe CExpression)

data CTranslationUnit
  -- CExternalDeclaration
  = CTranslationUnitExternal CExternalDeclaration
  -- CExternalDeclaration CTranslationUnit
  | CTranslationUnitTranslation CExternalDeclaration CTranslationUnit

data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction CFunctionDefinition
  -- CDeclaration
  | CExternalDeclaration CDeclaration

data CFunctionDefinition
  -- CDeclarationSpecifiers (optional) CDeclarator
  = CFunctionDefinitionSpecifiers (Maybe CDeclarationSpecifiers) CDeclarator
  -- CDeclarationList (optional) CCompoundStatement
  | CFunctionDefinitionList (Maybe CDeclarationList) CCompoundStatement
