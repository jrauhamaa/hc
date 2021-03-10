module ParseElements where

import Scanner (Coordinates)

data ParseElement a =
  ParseElement
    { parseLoc :: Coordinates
    , parseElem :: a
    }
  deriving (Show, Eq)

instance Functor ParseElement where
  fmap fab pea = ParseElement (parseLoc pea) (fab $ parseElem pea)

instance Applicative ParseElement where
  pure a = ParseElement (1, 1) a
  peab <*> pea = ParseElement (parseLoc pea) (parseElem peab $ parseElem pea)

type PE = ParseElement

newtype CIdentifier =
  CIdentifier String
  deriving (Show, Eq)

data CIdentifierOptional
  = CIdentifierOptional CIdentifier
  | CIdentifierOptionalEmpty
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
      CArgumentExpressionListOptional
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
  CArgumentExpressionList
    CAssignmentExpression
    CArgumentExpressionList'
  deriving (Show, Eq)

data CArgumentExpressionListOptional
  = CArgumentExpressionListOptional CArgumentExpressionList
  | CArgumentExpressionListOptionalEmpty
  deriving (Show, Eq)

data CArgumentExpressionList'
  -- empty
  = CArgumentExpressionList'Empty
  -- . CAssignmentExpression CArgumentExpressionList'
  | CArgumentExpressionList'
      CAssignmentExpression
      CArgumentExpressionList'
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
  = CUnaryOperatorAddress -- &
  | CUnaryOperatorMultiply -- *
  | CUnaryOperatorPlus -- +
  | CUnaryOperatorMinus -- -
  | CUnaryOperatorBitwiseNot -- ~
  | CUnaryOperatorNot -- !
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
  | CMultiplicativeExpression'Mul
      CCastExpression
      CMultiplicativeExpression'
  -- / CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Div
      CCastExpression
      CMultiplicativeExpression'
  -- % CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mod
      CCastExpression
      CMultiplicativeExpression'
  deriving (Show, Eq)

-- CMultiplicativeExpression CAdditiveExpression'
data CAdditiveExpression =
  CAdditiveExpression CMultiplicativeExpression CAdditiveExpression'
  deriving (Show, Eq)

data CAdditiveExpression'
  -- empty
  = CAdditiveExpression'Empty
  -- + CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Plus
      CMultiplicativeExpression
      CAdditiveExpression'
  -- - CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Minus
      CMultiplicativeExpression
      CAdditiveExpression'
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
  CInclusiveOrExpression
    CExclusiveOrExpression
    CInclusiveOrExpression'
  deriving (Show, Eq)

data CInclusiveOrExpression'
  -- empty
  = CInclusiveOrExpression'Empty
  -- | CExclusiveOrExpression CInclusiveOrExpression'
  | CInclusiveOrExpression'
      CExclusiveOrExpression
      CInclusiveOrExpression'
  deriving (Show, Eq)

-- CInclusiveOrExpression CLogicalAndExpression'
data CLogicalAndExpression =
  CLogicalAndExpression CInclusiveOrExpression CLogicalAndExpression'
  deriving (Show, Eq)

data CLogicalAndExpression'
  -- empty
  = CLogicalAndExpression'Empty
  -- && CInclusiveOrExpression CLogicalAndExpression'
  | CLogicalAndExpression'
      CInclusiveOrExpression
      CLogicalAndExpression'
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
  = CAssignmentOperatorAssign -- =
  | CAssignmentOperatorMul -- *=
  | CAssignmentOperatorDiv -- /=
  | CAssignmentOperatorMod -- %=
  | CAssignmentOperatorAdd -- +=
  | CAssignmentOperatorSub -- -=
  | CAssignmentOperatorLShift -- <<=
  | CAssignmentOperatorRShfit -- >>=
  | CAssignmentOperatorAnd -- &=
  | CAssignmentOperatorXor -- ^=
  | CAssignmentOperatorOr -- |=
  deriving (Show, Eq)

-- CAssignmentExpression CExpression'
data CExpression =
  CExpression CAssignmentExpression CExpression'
  deriving (Show, Eq)

data CExpression'
  -- empty
  = CExpression'Empty
  -- , CAssignmentExpression CExpression'
  | CExpression' CAssignmentExpression CExpression'
  deriving (Show, Eq)

data CExpressionOptional
  = CExpressionOptional CExpression
  | CExpressionOptionalEmpty
  deriving (Show, Eq)

-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression CConditionalExpression
  deriving (Show, Eq)

data CConstantExpressionOptional
  = CConstantExpressionOptional CConstantExpression
  | CConstantExpressionOptionalEmpty
  deriving (Show, Eq)

-- CDeclarationSpecifiers CInitDeclaratorList (optional)
data CDeclaration =
  CDeclaration CDeclarationSpecifiers CInitDeclaratorListOptional
  deriving (Show, Eq)

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclarationSpecifiersStorageClass
      CStorageClassSpecifier
      CDeclarationSpecifiersOptional
  -- CTypeSpecifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeSpecifier
      CTypeSpecifier
      CDeclarationSpecifiersOptional
  -- CTypeQualifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeQualifier
      CTypeQualifier
      CDeclarationSpecifiersOptional
  deriving (Show, Eq)

data CDeclarationSpecifiersOptional
  = CDeclarationSpecifiersOptional CDeclarationSpecifiers
  | CDeclarationSpecifiersOptionalEmpty
  deriving (Show, Eq)

-- CInitDeclarator CInitDeclaratorList'
data CInitDeclaratorList =
  CInitDeclaratorList CInitDeclarator CInitDeclaratorList'
  deriving (Show, Eq)

data CInitDeclaratorListOptional
  = CInitDeclaratorListOptional CInitDeclaratorList
  | CInitDeclaratorListOptionalEmpty
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
  = CStorageClassSpecifierTypedef -- typedef
  | CStorageClassSpecifierExtern -- extern
  | CStorageClassSpecifierStatic -- static
  | CStorageClassSpecifierAuto -- auto
  | CStorageClassSpecifierRegister -- register
  deriving (Show, Eq)

data CTypeSpecifier
  = CTypeSpecifierVoid -- void
  | CTypeSpecifierChar -- char
  | CTypeSpecifierShort -- short
  | CTypeSpecifierInt -- int
  | CTypeSpecifierLong -- long
  | CTypeSpecifierFloat -- float
  | CTypeSpecifierDouble -- double
  | CTypeSpecifierSigned -- signed
  | CTypeSpecifierUnsigned -- unsigned
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
      CIdentifierOptional
      CStructDeclarationList
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier CStructOrUnion CIdentifier
  deriving (Show, Eq)

data CStructOrUnion
  = CStruct -- struct
  | CUnion -- union
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
      CSpecifierQualifierListOptional
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecifierQualifierListQualifier
      CTypeQualifier
      CSpecifierQualifierListOptional
  deriving (Show, Eq)

data CSpecifierQualifierListOptional
  = CSpecifierQualifierListOptional CSpecifierQualifierList
  | CSpecifierQualifierListOptionalEmpty
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
  | CStructDeclaratorInit CDeclaratorOptional CConstantExpression
  deriving (Show, Eq)

data CEnumSpecifier
  -- enum CIdentifier (optional) { CEnumeratorList }
  = CEnumSpecifierList CIdentifierOptional CEnumeratorList
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
  = CTypeQualifierConst -- const
  | CTypeQualifierVolatile -- volatile
  deriving (Show, Eq)

-- CPointer (optional) CDirectDeclarator
data CDeclarator =
  CDeclarator CPointerOptional CDirectDeclarator
  deriving (Show, Eq)

data CDeclaratorOptional
  = CDeclaratorOptional CDeclarator
  | CDeclaratorOptionalEmpty
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
  | CDirectDeclarator'Indexed
      CConstantExpressionOptional
      CDirectDeclarator'
  -- ( CParameterTypeList ) CDirectDeclarator'
  | CDirectDeclarator'ParamTypeList
      CParameterTypeList
      CDirectDeclarator'
  -- ( CIdentifierList (optional) ) CDirectDeclarator'
  | CDirectDeclarator'IdList
      CIdentifierListOptional
      CDirectDeclarator'
  deriving (Show, Eq)

data CPointer
  -- * CTypeQualifierList (optional)
  = CPointerSingle CTypeQualifierListOptional
  -- * CTypeQualifierList (optional) CPointer
  | CPointerMulti CTypeQualifierListOptional CPointer
  deriving (Show, Eq)

data CPointerOptional
  = CPointerOptional CPointer
  | CPointerOptionalEmpty
  deriving (Show, Eq)

data CTypeQualifierList
  -- CTypeQualifier
  = CTypeQualifierListSingleton CTypeQualifier
  -- CTypeQualifier CTypeQualifierList
  | CTypeQualifierList CTypeQualifier CTypeQualifierList
  deriving (Show, Eq)

data CTypeQualifierListOptional
  = CTypeQualifierListOptional CTypeQualifierList
  | CTypeQualifierListOptionalEmpty
  deriving (Show, Eq)

data CParameterTypeList
  -- CParameterList
  = CParameterTypeList CParameterList
  -- CParameterList , ...
  | CParameterTypeListVarargs CParameterList
  deriving (Show, Eq)

data CParameterTypeListOptional
  = CParameterTypeListOptional CParameterTypeList
  | CParameterTypeListOptionalEmpty
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
      CAbstractDeclaratorOptional
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

data CIdentifierListOptional
  = CIdentifierListOptional CIdentifierList
  | CIdentifierListOptionalEmpty
  deriving (Show, Eq)

-- CSpecifierQualifierList CAbstractDeclarator (optional)
data CTypeName =
  CTypeName CSpecifierQualifierList CAbstractDeclaratorOptional
  deriving (Show, Eq)

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer CPointer
  -- CPointer (optional) CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect
      CPointerOptional
      CDirectAbstractDeclarator
  deriving (Show, Eq)

data CAbstractDeclaratorOptional
  = CAbstractDeclaratorOptional CAbstractDeclarator
  | CAbstractDeclaratorOptionalEmpty
  deriving (Show, Eq)

data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator ) CDirectAbstractDeclarator'
  = CDirectAbstractDeclaratorParens
      CAbstractDeclarator
      CDirectAbstractDeclarator'
  -- [ CConstantExpression (optional) ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorConst
      CConstantExpressionOptional
      CDirectAbstractDeclarator'
  -- ( CParameterTypeList (optional) ) CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorParams
      CParameterTypeListOptional
      CDirectAbstractDeclarator'
  deriving (Show, Eq)

data CDirectAbstractDeclarator'
  -- empty
  = CDirectAbstractDeclarator'Empty
  -- [ CConstantExpression (optional) ]
  | CDirectAbstractDeclarator'Const CConstantExpressionOptional
  -- ( CParameterTypeList (optional) )
  | CDirectAbstractDeclarator'Params CParameterTypeListOptional
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
  CCompoundStatement CDeclarationListOptional CStatementListOptional
  deriving (Show, Eq)

data CDeclarationList
  -- CDeclaration
  = CDeclarationListSingleton CDeclaration
  -- CDeclarationList CDeclaration
  | CDeclarationList CDeclaration CDeclarationList
  deriving (Show, Eq)

data CDeclarationListOptional
  = CDeclarationListOptional CDeclarationList
  | CDeclarationListOptionalEmpty
  deriving (Show, Eq)

data CStatementList
  -- CStatement
  = CStatementListSingleton CStatement
  -- CStatementList CStatement
  | CStatementList CStatement CStatementList
  deriving (Show, Eq)

data CStatementListOptional
  = CStatementListOptional CStatementList
  | CStatementListOptionalEmpty
  deriving (Show, Eq)

-- CExpression (optional) ;
newtype CExpressionStatement =
  CExpressionStatement CExpressionOptional
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
      CExpressionOptional
      CExpressionOptional
      CExpressionOptional
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
  | CJumpStatementReturn CExpressionOptional
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
  = CFunctionDefinitionSpecifiers
      CDeclarationSpecifiersOptional
      CDeclarator
  -- CDeclarationList (optional) CCompoundStatement
  | CFunctionDefinitionList
      CDeclarationListOptional
      CCompoundStatement
  deriving (Show, Eq)
