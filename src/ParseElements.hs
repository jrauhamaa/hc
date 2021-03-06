module ParseElements where

newtype CIdentifier =
  CIdentifier String
  deriving (Show, Eq)

data CConstant
  -- float literal
  = CConstantFloat Double
  -- integer literal
  | CConstantInteger Int
  -- CEnumerationConstant
  | CConstantEnumeration CEnumerationConstant
  -- character literal
  | CConstantCharacter Char
  deriving (Show, Eq)

data CPrimaryExpression
  -- CIdentifier
  = CPrimaryExpressionId CIdentifier
  -- CConstant
  | CPrimaryExpressionConst CConstant
  -- string literal
  | CPrimaryExpressionStr String
  -- ( CExpression )
  | CPrimaryExpressionParen CExpression
  deriving (Show, Eq)

-- CPrimaryExpression CPostfixExpression'
data CPostfixExpression =
  CPostfixExpression CPrimaryExpression CPostfixExpression'
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- CAssignmentExpression CArgumentExpressionList'
data CArgumentExpressionList =
  CArgumentExpressionList CAssignmentExpression CArgumentExpressionList'
  deriving (Show, Eq)

data CArgumentExpressionList'
  -- empty
  = CArgumentExpressionList'Empty
  -- . CAssignmentExpression CArgumentExpressionList'
  | CArgumentExpressionList' CAssignmentExpression CArgumentExpressionList'
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data CUnaryOperator
  = CUnaryOperatorAddress       -- &
  | CUnaryOperatorMultiply      -- *
  | CUnaryOperatorPlus          -- +
  | CUnaryOperatorMinus         -- -
  | CUnaryOperatorBitwiseNot    -- ~
  | CUnaryOperatorNot           -- !
  deriving (Show, Eq)

data CCastExpression
  -- CUnaryExpression
  = CCastExpressionSingleton CUnaryExpression
  -- ( CTypeName ) CCastExpression
  | CCastExpression CTypeName CCastExpression
  deriving (Show, Eq)

-- CCastExpression CMultiplicativeExpression'
data CMultiplicativeExpression =
  CMultiplicativeExpression CCastExpression CMultiplicativeExpression'
  deriving (Show, Eq)

data CMultiplicativeExpression'
  -- empty
  = CMultiplicativeExpression'Empty
  -- * CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mul CCastExpression CMultiplicativeExpression'
  -- / CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Div CCastExpression CMultiplicativeExpression'
  -- % CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mod CCastExpression CMultiplicativeExpression'
  deriving (Show, Eq)

-- CMultiplicativeExpression CAdditiveExpression'
data CAdditiveExpression =
  CAdditiveExpression CMultiplicativeExpression CAdditiveExpression'
  deriving (Show, Eq)

data CAdditiveExpression'
  -- empty
  = CAdditiveExpression'Empty
  -- + CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Plus CMultiplicativeExpression CAdditiveExpression'
  -- - CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Minus CMultiplicativeExpression CAdditiveExpression'
  deriving (Show, Eq)

-- CAdditiveExpression CShiftExpression'
data CShiftExpression =
  CShiftExpression CAdditiveExpression CShiftExpression'
  deriving (Show, Eq)

data CShiftExpression'
  -- empty
  = CShiftExpression'Empty
  -- << CAdditiveExpression CShiftExpression'
  | CShiftExpression'Left CAdditiveExpression CShiftExpression'
  -- >> CAdditiveExpression CShiftExpression'
  | CShiftExpression'Right CAdditiveExpression CShiftExpression'
  deriving (Show, Eq)

-- CShiftExpression CRelationalExpression'
data CRelationalExpression =
  CRelationalExpression CShiftExpression CRelationalExpression'
  deriving (Show, Eq)

data CRelationalExpression'
  -- empty
  = CRelationalExpression'Empty
  -- < CShiftExpression CRelationalExpression'
  | CRelationalExpression'LT CShiftExpression CRelationalExpression'
  -- <= CShiftExpression CRelationalExpression'
  | CRelationalExpression'LTE CShiftExpression CRelationalExpression'
  -- > CShiftExpression CRelationalExpression'
  | CRelationalExpression'GT CShiftExpression CRelationalExpression'
  -- >= CShiftExpression CRelationalExpression'
  | CRelationalExpression'GTE CShiftExpression CRelationalExpression'
  deriving (Show, Eq)

-- CRelationalExpression CEqualityExpression'
data CEqualityExpression =
  CEqualityExpression CRelationalExpression CEqualityExpression'
  deriving (Show, Eq)

data CEqualityExpression'
  -- empty
  = CEqualityExpression'Empty
  -- == CRelationalExpression CEqualityExpression'
  | CEqualityExpression'EQ CRelationalExpression CEqualityExpression'
  -- != CRelationalExpression CEqualityExpression'
  | CEqualityExpression'NEQ CRelationalExpression CEqualityExpression'
  deriving (Show, Eq)

-- CEqualityExpression CAndExpression'
data CAndExpression =
  CAndExpression CEqualityExpression CAndExpression'
  deriving (Show, Eq)

data CAndExpression'
  -- empty
  = CAndExpression'Empty
  -- & CEqualityExpression CAndExpression'
  | CAndExpression' CEqualityExpression CAndExpression'
  deriving (Show, Eq)

-- CAndExpression CExclusiveOrExpression'
data CExclusiveOrExpression =
  CExclusiveOrExpression CAndExpression CExclusiveOrExpression'
  deriving (Show, Eq)

data CExclusiveOrExpression'
  -- empty
  = CExclusiveOrExpression'Empty
  -- ^ CAndExpression CExclusiveOrExpression'
  | CExclusiveOrExpression' CAndExpression CExclusiveOrExpression'
  deriving (Show, Eq)

-- CExclusiveOrExpression CInclusiveOrExpression'
data CInclusiveOrExpression =
  CInclusiveOrExpression CExclusiveOrExpression CInclusiveOrExpression'
  deriving (Show, Eq)

data CInclusiveOrExpression'
  -- empty
  = CInclusiveOrExpression'Empty
  -- | CExclusiveOrExpression CInclusiveOrExpression'
  | CInclusiveOrExpression' CExclusiveOrExpression CInclusiveOrExpression'
  deriving (Show, Eq)

-- CInclusiveOrExpression CLogicalAndExpression'
data CLogicalAndExpression =
  CLogicalAndExpression CInclusiveOrExpression CLogicalAndExpression'
  deriving (Show, Eq)

data CLogicalAndExpression'
  -- empty
  = CLogicalAndExpression'Empty
  -- && CInclusiveOrExpression CLogicalAndExpression'
  | CLogicalAndExpression' CInclusiveOrExpression CLogicalAndExpression'
  deriving (Show, Eq)

-- CLogicalAndExpression CLogicalOrExpression'
data CLogicalOrExpression =
  CLogicalOrExpression CLogicalAndExpression CLogicalOrExpression'
  deriving (Show, Eq)

data CLogicalOrExpression'
  -- empty
  = CLogicalOrExpression'Empty
  -- || CLogicalAndExpression CLogicalOrExpression'
  | CLogicalOrExpression' CLogicalAndExpression CLogicalOrExpression'
  deriving (Show, Eq)

data CConditionalExpression
  -- CLogicalOrExpression
  = CConditionalExpressionSingleton CLogicalOrExpression
  -- CLogicalOrExpression ? CExpression : CConditionalExpression
  | CConditionalExpression
      CLogicalOrExpression
      CExpression
      CConditionalExpression
  deriving (Show, Eq)

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionSingleton CConditionalExpression
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpression
      CUnaryExpression
      CAssignmentOperator
      CAssignmentExpression
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- CAssignmentExpression CExpression'
data CExpression =
  CExpression CAssignmentExpression CExpression'
  deriving (Show, Eq)

-- , CAssignmentExpression CExpression'
data CExpression' =
  CExpression' CAssignmentExpression CExpression'
  deriving (Show, Eq)

-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression CConditionalExpression
  deriving (Show, Eq)

-- CDeclarationSpecifiers CInitDeclaratorList (optional)
data CDeclaration =
  CDeclaration CDeclarationSpecifiers (Maybe CInitDeclaratorList)
  deriving (Show, Eq)

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclarationSpecifiersStorageClass
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
  deriving (Show, Eq)

-- CInitDeclarator CInitDeclaratorList'
data CInitDeclaratorList =
  CInitDeclaratorList CInitDeclarator CInitDeclaratorList'
  deriving (Show, Eq)

data CInitDeclaratorList'
  -- empty
  = CInitDeclaratorList'Empty
  -- , CInitDeclarator CInitDeclaratorList'
  | CInitDeclaratorList' CInitDeclarator CInitDeclaratorList'
  deriving (Show, Eq)

data CInitDeclarator
  -- CDeclarator
  = CInitDeclaratorSingleton CDeclarator
  -- CDeclarator = CInitializer
  | CInitDeclarator CDeclarator CInitializer
  deriving (Show, Eq)

data CStorageClassSpecifier
  = CStorageClassSpecifierTypedef   -- typedef
  | CStorageClassSpecifierExtern    -- extern
  | CStorageClassSpecifierStatic    -- static
  | CStorageClassSpecifierAuto      -- auto
  | CStorageClassSpecifierRegister  -- register
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifier (optional) { CStructDeclarationList }
  = CStructOrUnionSpecifierList
      CStructOrUnion
      (Maybe CIdentifier)
      CStructDeclarationList
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier CStructOrUnion CIdentifier
  deriving (Show, Eq)

data CStructOrUnion
  = CStruct     -- struct
  | CUnion      -- union
  deriving (Show, Eq)

-- NOTE: different from CStructDeclaratorList
data CStructDeclarationList
  -- CStructDeclaration
  = CStructDeclarationListSingleton CStructDeclaration
  -- CStructDeclaration CStructDeclarationList
  | CStructDeclarationList CStructDeclaration CStructDeclarationList
  deriving (Show, Eq)

-- NOTE: different from CStructDeclarator
-- CSpecifierQualifierList CStructDeclaratorList ;
data CStructDeclaration =
  CStructDeclaration CSpecifierQualifierList CStructDeclaratorList
  deriving (Show, Eq)

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierList (optional)
  = CSpecifierQualifierListSpecifier
      CTypeSpecifier
      (Maybe CSpecifierQualifierList)
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecifierQualifierListQualifier
      CTypeQualifier
      (Maybe CSpecifierQualifierList)
  deriving (Show, Eq)

-- NOTE: different from CStructDeclarationList
-- CStructDeclarator CStructDeclaratorList'
data CStructDeclaratorList =
  CStructDeclaratorList CStructDeclarator CStructDeclaratorList'
  deriving (Show, Eq)

data CStructDeclaratorList'
  -- empty
  = CStructDeclaratorList'Empty
  -- , CStructDeclarator CStructDeclaratorList'
  | CStructDeclaratorList' CStructDeclarator CStructDeclaratorList'
  deriving (Show, Eq)

data CStructDeclarator -- NOTE: different from CStructDeclaration
  -- CDeclarator
  = CStructDeclarator CDeclarator
  -- CDeclarator (optional) : CConstantExpression
  | CStructDeclaratorInit (Maybe CDeclarator) CConstantExpression
  deriving (Show, Eq)

data CEnumSpecifier
  -- enum CIdentifier (optional) { CEnumeratorList }
  = CEnumSpecifierList (Maybe CIdentifier) CEnumeratorList
  -- enum CIdentifier
  | CEnumSpecifier CIdentifier
  deriving (Show, Eq)

-- CEnumerator CEnumeratorList'
data CEnumeratorList =
  CEnumeratorList CEnumerator CEnumeratorList'
  deriving (Show, Eq)

data CEnumeratorList'
  -- empty
  = CEnumeratorList'Empty
  -- , CEnumerator CEnumeratorList'
  | CEnumeratorList' CEnumerator CEnumeratorList'
  deriving (Show, Eq)

data CEnumerator
  -- CEnumerationConstant
  = CEnumerator CEnumerationConstant
  -- CEnumerationConstant = CConstantExpression
  | CEnumeratorAssign CEnumerationConstant CConstantExpression
  deriving (Show, Eq)

-- CIdentifier
newtype CEnumerationConstant =
  CEnumerationConstant CIdentifier
  deriving (Show, Eq)

data CTypeQualifier
  = CTypeQualifierConst         -- const
  | CTypeQualifierVolatile      -- volatile
  deriving (Show, Eq)

-- CPointer (optional) CDirectDeclarator
data CDeclarator =
  CDeclarator (Maybe CPointer) CDirectDeclarator
  deriving (Show, Eq)

data CDirectDeclarator
  -- CIdentifier CDirectDeclarator'
  = CDirectDeclaratorId CIdentifier CDirectDeclarator'
  -- ( CDeclarator ) CDirectDeclarator'
  | CDirectDeclaratorParen CDeclarator CDirectDeclarator'
  deriving (Show, Eq)

data CDirectDeclarator'
  -- empty
  = CDirectDeclarator'Empty
  -- [ CConstantExpression (optional) ] CDirectDeclarator'
  | CDirectDeclarator'Indexed (Maybe CConstantExpression) CDirectDeclarator'
  -- ( CParameterTypeList ) CDirectDeclarator'
  | CDirectDeclarator'ParamTypeList CParameterTypeList CDirectDeclarator'
  -- ( CIdentifierList (optional) ) CDirectDeclarator'
  | CDirectDeclarator'IdList (Maybe CIdentifierList) CDirectDeclarator'
  deriving (Show, Eq)

data CPointer
  -- * CTypeQualifierList (optional)
  = CPointerSingle (Maybe CTypeQualifierList)
  -- * CTypeQualifierList (optional) CPointer
  | CPointerMulti (Maybe CTypeQualifierList) CPointer
  deriving (Show, Eq)

data CTypeQualifierList
  -- CTypeQualifier
  = CTypeQualifierListSingleton CTypeQualifier
  -- CTypeQualifier CTypeQualifierList
  | CTypeQualifierList CTypeQualifier CTypeQualifierList
  deriving (Show, Eq)

data CParameterTypeList
  -- CParameterList
  = CParameterTypeList CParameterList
  -- CParameterList , ...
  | CParameterTypeListVarargs CParameterList
  deriving (Show, Eq)

-- CParameterDeclaration CParameterList'
data CParameterList =
  CParameterList CParameterDeclaration CParameterList'
  deriving (Show, Eq)

data CParameterList'
  -- empty
  = CParameterList'Empty
  -- , CParameterDeclaration CParameterList'
  | CParameterList' CParameterDeclaration CParameterList'
  deriving (Show, Eq)

-- CParameterList , CParameterDeclaration
data CParameterDeclaration
  -- CDeclarationSpecifiers CDeclarator
  = CParameterDeclaration CDeclarationSpecifiers CDeclarator
  -- CDeclarationSpecifiers CAbstractDeclarator (optional)
  | CParameterDeclarationAbstract
      CDeclarationSpecifiers
      (Maybe CAbstractDeclarator)
  deriving (Show, Eq)

-- CIdentifier CIdentifierList'
data CIdentifierList =
  CIdentifierList CIdentifier CIdentifierList'
  deriving (Show, Eq)

data CIdentifierList'
  -- empty
  = CIdentifierList'Empty
  -- , CIdentifier CIdentifierList'
  | CIdentifierList' CIdentifier CIdentifierList'
  deriving (Show, Eq)

-- CSpecifierQualifierList CAbstractDeclarator (optional)
data CTypeName =
  CTypeName CSpecifierQualifierList (Maybe CAbstractDeclarator)
  deriving (Show, Eq)

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer CPointer
  -- CPointer (optional) CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect (Maybe CPointer) CDirectAbstractDeclarator
  deriving (Show, Eq)

data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator ) CDirectAbstractDeclarator'
  = CDirectAbstractDeclaratorParens
      CAbstractDeclarator
      CDirectAbstractDeclarator'
  -- [ CConstantExpression (optional) ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorConst
      (Maybe CConstantExpression)
      CDirectAbstractDeclarator'
  -- ( CParameterTypeList (optional) ) CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorParams
      (Maybe CParameterTypeList)
      CDirectAbstractDeclarator'
  deriving (Show, Eq)

data CDirectAbstractDeclarator'
  -- empty
  = CDirectAbstractDeclarator'Empty
  -- [ CConstantExpression (optional) ]
  | CDirectAbstractDeclarator'Const (Maybe CConstantExpression)
  -- ( CParameterTypeList (optional) )
  | CDirectAbstractDeclarator'Params (Maybe CParameterTypeList)
  deriving (Show, Eq)

-- CIdentifier
newtype CTypedefName =
  CTypedefName CIdentifier
  deriving (Show, Eq)

data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment CAssignmentExpression
  -- { CInitializerList }
  | CInitializerBracketList CInitializerList
  -- { CInitializerList , }
  | CInitializerBracketListComma CInitializerList
  deriving (Show, Eq)

-- CInitializer CInitializerList'
data CInitializerList =
  CInitializerList CInitializer CInitializerList'
  deriving (Show, Eq)

data CInitializerList'
  -- empty
  = CInitializerList'Empty
  -- , CInitializer CInitializerList'
  | CInitializerList' CInitializer CInitializerList'
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data CLabeledStatement
  -- CIdentifier : CStatement
  = CLabeledStatementId CIdentifier CStatement
  -- case CConstantExpression : CStatement
  | CLabeledStatementCase CConstantExpression CStatement
  -- default : CStatement
  | CLabeledStatementDefault CStatement
  deriving (Show, Eq)

-- { CDeclarationList (optional) CStatementList (optional) }
data CCompoundStatement =
  CCompoundStatement (Maybe CDeclarationList) (Maybe CStatementList)
  deriving (Show, Eq)

data CDeclarationList
  -- CDeclaration
  = CDeclarationListSingleton CDeclaration
  -- CDeclarationList CDeclaration
  | CDeclarationList CDeclaration CDeclarationList
  deriving (Show, Eq)

data CStatementList
  -- CStatement
  = CStatementListSingleton CStatement
  -- CStatementList CStatement
  | CStatementList CStatement CStatementList
  deriving (Show, Eq)

-- CExpression (optional) ;
newtype CExpressionStatement =
  CExpressionStatement (Maybe CExpression)
  deriving (Show, Eq)

data CSelectionStatement
  -- if ( CExpression ) CStatement
  = CSelectionStatementIf CExpression CStatement
  -- if ( CExpression ) CStatement else CStatement
  | CSelectionStatementIfElse CExpression CStatement CStatement
  -- switch ( CExpression ) CStatement
  | CSelectionStatementSwitch CExpression CStatement
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data CJumpStatement
  -- goto CIdentifier ;
  = CJumpStatementGoto CIdentifier
  -- continue ;
  | CJumpStatementContinue
  -- break ;
  | CJumpStatementBreak
  -- return CExpression (optional) ;
  | CJumpStatementReturn (Maybe CExpression)
  deriving (Show, Eq)

data CTranslationUnit
  -- CExternalDeclaration
  = CTranslationUnitExternal CExternalDeclaration
  -- CExternalDeclaration CTranslationUnit
  | CTranslationUnitTranslation CExternalDeclaration CTranslationUnit
  deriving (Show, Eq)

data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction CFunctionDefinition
  -- CDeclaration
  | CExternalDeclaration CDeclaration
  deriving (Show, Eq)

data CFunctionDefinition
  -- CDeclarationSpecifiers (optional) CDeclarator
  = CFunctionDefinitionSpecifiers (Maybe CDeclarationSpecifiers) CDeclarator
  -- CDeclarationList (optional) CCompoundStatement
  | CFunctionDefinitionList (Maybe CDeclarationList) CCompoundStatement
  deriving (Show, Eq)
