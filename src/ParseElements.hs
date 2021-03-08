module ParseElements where

import Scanner (Coordinates)

data ParseElement a =
  ParseElement
    { coordinates :: Coordinates
    , element :: a
    }
  deriving (Show, Eq)

instance Functor ParseElement where
  fmap fab pea = ParseElement (coordinates pea) (fab $ element pea)

instance Applicative ParseElement where
  pure a = ParseElement (1, 1) a
  peab <*> pea = ParseElement (coordinates pea) (element peab $ element pea)

type PE = ParseElement

newtype CIdentifier =
  CIdentifier (PE String)
  deriving (Show, Eq)

data CIdentifierOptional
  = CIdentifierOptional (PE CIdentifier)
  | CIdentifierOptionalEmpty
  deriving (Show, Eq)

data CConstant
  -- float literal
  = CConstantFloat (PE Double)
  -- integer literal
  | CConstantInteger (PE Int)
  -- CEnumerationConstant
  | CConstantEnumeration (PE CEnumerationConstant)
  -- character literal
  | CConstantCharacter (PE Char)
  deriving (Show, Eq)

data CPrimaryExpression
  -- CIdentifier
  = CPrimaryExpressionId (PE CIdentifier)
  -- CConstant
  | CPrimaryExpressionConst (PE CConstant)
  -- string literal
  | CPrimaryExpressionStr (PE String)
  -- ( CExpression )
  | CPrimaryExpressionParen (PE CExpression)
  deriving (Show, Eq)

-- CPrimaryExpression CPostfixExpression'
data CPostfixExpression =
  CPostfixExpression (PE CPrimaryExpression) (PE CPostfixExpression')
  deriving (Show, Eq)

data CPostfixExpression'
  -- empty
  = CPostfixExpression'Empty
  -- [ CExpression ] CPostfixExpression'
  | CPostfixExpression'Indexed (PE CExpression) (PE CPostfixExpression')
  -- ( CArgumentExpressionList (optional) ) CPostfixExpression'
  | CPostfixExpression'ArgList
      (PE CArgumentExpressionListOptional)
      (PE CPostfixExpression')
  -- . CIdentifier CPostfixExpression'
  | CPostfixExpression'StructField (PE CIdentifier) (PE CPostfixExpression')
  -- -> CIdentifier CPostfixExpression'
  | CPostfixExpression'StructPointer (PE CIdentifier) (PE CPostfixExpression')
  -- ++ CPostfixExpression'
  | CPostfixExpression'Increment (PE CPostfixExpression')
  -- -- CPostfixExpression'
  | CPostfixExpression'Decrement (PE CPostfixExpression')
  deriving (Show, Eq)

-- CAssignmentExpression CArgumentExpressionList'
data CArgumentExpressionList =
  CArgumentExpressionList
    (PE CAssignmentExpression)
    (PE CArgumentExpressionList')
  deriving (Show, Eq)

data CArgumentExpressionListOptional
  = CArgumentExpressionListOptional (PE CArgumentExpressionList)
  | CArgumentExpressionListOptionalEmpty
  deriving (Show, Eq)

data CArgumentExpressionList'
  -- empty
  = CArgumentExpressionList'Empty
  -- . CAssignmentExpression CArgumentExpressionList'
  | CArgumentExpressionList'
      (PE CAssignmentExpression)
      (PE CArgumentExpressionList')
  deriving (Show, Eq)

data CUnaryExpression
  -- CPostfixExpression
  = CUnaryExpressionSingleton (PE CPostfixExpression)
  -- ++ CUnaryExpression
  | CUnaryExpressionIncr (PE CUnaryExpression)
  -- -- CUnaryExpression
  | CUnaryExpressionDecr (PE CUnaryExpression)
  -- CUnaryOperator CCastExpression
  | CUnaryExpressionCast (PE CUnaryOperator) (PE CCastExpression)
  -- sizeof CUnaryExpression
  | CUnaryExpressionSizeof (PE CUnaryExpression)
  -- sizeof ( CTypeName )
  | CUnaryExpressionSizeofType (PE CTypeName)
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
  = CCastExpressionSingleton (PE CUnaryExpression)
  -- ( CTypeName ) CCastExpression
  | CCastExpression (PE CTypeName) (PE CCastExpression)
  deriving (Show, Eq)

-- CCastExpression CMultiplicativeExpression'
data CMultiplicativeExpression =
  CMultiplicativeExpression (PE CCastExpression) (PE CMultiplicativeExpression')
  deriving (Show, Eq)

data CMultiplicativeExpression'
  -- empty
  = CMultiplicativeExpression'Empty
  -- * CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mul
      (PE CCastExpression)
      (PE CMultiplicativeExpression')
  -- / CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Div
      (PE CCastExpression)
      (PE CMultiplicativeExpression')
  -- % CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mod
      (PE CCastExpression)
      (PE CMultiplicativeExpression')
  deriving (Show, Eq)

-- CMultiplicativeExpression CAdditiveExpression'
data CAdditiveExpression =
  CAdditiveExpression (PE CMultiplicativeExpression) (PE CAdditiveExpression')
  deriving (Show, Eq)

data CAdditiveExpression'
  -- empty
  = CAdditiveExpression'Empty
  -- + CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Plus
      (PE CMultiplicativeExpression)
      (PE CAdditiveExpression')
  -- - CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Minus
      (PE CMultiplicativeExpression)
      (PE CAdditiveExpression')
  deriving (Show, Eq)

-- CAdditiveExpression CShiftExpression'
data CShiftExpression =
  CShiftExpression (PE CAdditiveExpression) (PE CShiftExpression')
  deriving (Show, Eq)

data CShiftExpression'
  -- empty
  = CShiftExpression'Empty
  -- << CAdditiveExpression CShiftExpression'
  | CShiftExpression'Left (PE CAdditiveExpression) (PE CShiftExpression')
  -- >> CAdditiveExpression CShiftExpression'
  | CShiftExpression'Right (PE CAdditiveExpression) (PE CShiftExpression')
  deriving (Show, Eq)

-- CShiftExpression CRelationalExpression'
data CRelationalExpression =
  CRelationalExpression (PE CShiftExpression) (PE CRelationalExpression')
  deriving (Show, Eq)

data CRelationalExpression'
  -- empty
  = CRelationalExpression'Empty
  -- < CShiftExpression CRelationalExpression'
  | CRelationalExpression'LT (PE CShiftExpression) (PE CRelationalExpression')
  -- <= CShiftExpression CRelationalExpression'
  | CRelationalExpression'LTE (PE CShiftExpression) (PE CRelationalExpression')
  -- > CShiftExpression CRelationalExpression'
  | CRelationalExpression'GT (PE CShiftExpression) (PE CRelationalExpression')
  -- >= CShiftExpression CRelationalExpression'
  | CRelationalExpression'GTE (PE CShiftExpression) (PE CRelationalExpression')
  deriving (Show, Eq)

-- CRelationalExpression CEqualityExpression'
data CEqualityExpression =
  CEqualityExpression (PE CRelationalExpression) (PE CEqualityExpression')
  deriving (Show, Eq)

data CEqualityExpression'
  -- empty
  = CEqualityExpression'Empty
  -- == CRelationalExpression CEqualityExpression'
  | CEqualityExpression'EQ (PE CRelationalExpression) (PE CEqualityExpression')
  -- != CRelationalExpression CEqualityExpression'
  | CEqualityExpression'NEQ (PE CRelationalExpression) (PE CEqualityExpression')
  deriving (Show, Eq)

-- CEqualityExpression CAndExpression'
data CAndExpression =
  CAndExpression (PE CEqualityExpression) (PE CAndExpression')
  deriving (Show, Eq)

data CAndExpression'
  -- empty
  = CAndExpression'Empty
  -- & CEqualityExpression CAndExpression'
  | CAndExpression' (PE CEqualityExpression) (PE CAndExpression')
  deriving (Show, Eq)

-- CAndExpression CExclusiveOrExpression'
data CExclusiveOrExpression =
  CExclusiveOrExpression (PE CAndExpression) (PE CExclusiveOrExpression')
  deriving (Show, Eq)

data CExclusiveOrExpression'
  -- empty
  = CExclusiveOrExpression'Empty
  -- ^ CAndExpression CExclusiveOrExpression'
  | CExclusiveOrExpression' (PE CAndExpression) (PE CExclusiveOrExpression')
  deriving (Show, Eq)

-- CExclusiveOrExpression CInclusiveOrExpression'
data CInclusiveOrExpression =
  CInclusiveOrExpression
    (PE CExclusiveOrExpression)
    (PE CInclusiveOrExpression')
  deriving (Show, Eq)

data CInclusiveOrExpression'
  -- empty
  = CInclusiveOrExpression'Empty
  -- | CExclusiveOrExpression CInclusiveOrExpression'
  | CInclusiveOrExpression'
      (PE CExclusiveOrExpression)
      (PE CInclusiveOrExpression')
  deriving (Show, Eq)

-- CInclusiveOrExpression CLogicalAndExpression'
data CLogicalAndExpression =
  CLogicalAndExpression (PE CInclusiveOrExpression) (PE CLogicalAndExpression')
  deriving (Show, Eq)

data CLogicalAndExpression'
  -- empty
  = CLogicalAndExpression'Empty
  -- && CInclusiveOrExpression CLogicalAndExpression'
  | CLogicalAndExpression'
      (PE CInclusiveOrExpression)
      (PE CLogicalAndExpression')
  deriving (Show, Eq)

-- CLogicalAndExpression CLogicalOrExpression'
data CLogicalOrExpression =
  CLogicalOrExpression (PE CLogicalAndExpression) (PE CLogicalOrExpression')
  deriving (Show, Eq)

data CLogicalOrExpression'
  -- empty
  = CLogicalOrExpression'Empty
  -- || CLogicalAndExpression CLogicalOrExpression'
  | CLogicalOrExpression' (PE CLogicalAndExpression) (PE CLogicalOrExpression')
  deriving (Show, Eq)

data CConditionalExpression
  -- CLogicalOrExpression
  = CConditionalExpressionSingleton (PE CLogicalOrExpression)
  -- CLogicalOrExpression ? CExpression : CConditionalExpression
  | CConditionalExpression
      (PE CLogicalOrExpression)
      (PE CExpression)
      (PE CConditionalExpression)
  deriving (Show, Eq)

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionSingleton (PE CConditionalExpression)
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpression
      (PE CUnaryExpression)
      (PE CAssignmentOperator)
      (PE CAssignmentExpression)
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
  CExpression (PE CAssignmentExpression) (PE CExpression')
  deriving (Show, Eq)

-- , CAssignmentExpression CExpression'
data CExpression' =
  CExpression' (PE CAssignmentExpression) (PE CExpression')
  deriving (Show, Eq)

data CExpressionOptional
  = CExpressionOptional (PE CExpression)
  | CExpressionOptionalEmpty
  deriving (Show, Eq)

-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression (PE CConditionalExpression)
  deriving (Show, Eq)

data CConstantExpressionOptional
  = CConstantExpressionOptional (PE CConstantExpression)
  | CConstantExpressionOptionalEmpty
  deriving (Show, Eq)

-- CDeclarationSpecifiers CInitDeclaratorList (optional)
data CDeclaration =
  CDeclaration (PE CDeclarationSpecifiers) (PE CInitDeclaratorListOptional)
  deriving (Show, Eq)

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclarationSpecifiersStorageClass
      (PE CStorageClassSpecifier)
      (PE CDeclarationSpecifiersOptional)
  -- CTypeSpecifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeSpecifier
      (PE CTypeSpecifier)
      (PE CDeclarationSpecifiersOptional)
  -- CTypeQualifier CDeclarationSpecifiers (optional)
  | CDeclarationSpecifiersTypeQualifier
      (PE CTypeQualifier)
      (PE CDeclarationSpecifiersOptional)
  deriving (Show, Eq)

data CDeclarationSpecifiersOptional
  = CDeclarationSpecifiersOptional (PE CDeclarationSpecifiers)
  | CDeclarationSpecifiersOptionalEmpty
  deriving (Show, Eq)

-- CInitDeclarator CInitDeclaratorList'
data CInitDeclaratorList =
  CInitDeclaratorList (PE CInitDeclarator) (PE CInitDeclaratorList')
  deriving (Show, Eq)

data CInitDeclaratorListOptional
  = CInitDeclaratorListOptional (PE CInitDeclaratorList)
  | CInitDeclaratorListOptionalEmpty
  deriving (Show, Eq)

data CInitDeclaratorList'
  -- empty
  = CInitDeclaratorList'Empty
  -- , CInitDeclarator CInitDeclaratorList'
  | CInitDeclaratorList' (PE CInitDeclarator) (PE CInitDeclaratorList')
  deriving (Show, Eq)

data CInitDeclarator
  -- CDeclarator
  = CInitDeclaratorSingleton (PE CDeclarator)
  -- CDeclarator = CInitializer
  | CInitDeclarator (PE CDeclarator) (PE CInitializer)
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
  | CTypeSpecifierStructOrUnion (PE CStructOrUnionSpecifier)
  -- CEnumSpecifier
  | CTypeSpecifierEnum (PE CEnumSpecifier)
  -- CTypedefName
  | CTypeSpecifierTypedef (PE CTypedefName)
  deriving (Show, Eq)

data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifier (optional) { CStructDeclarationList }
  = CStructOrUnionSpecifierList
      (PE CStructOrUnion)
      (PE CIdentifierOptional)
      (PE CStructDeclarationList)
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier (PE CStructOrUnion) (PE CIdentifier)
  deriving (Show, Eq)

data CStructOrUnion
  = CStruct -- struct
  | CUnion -- union
  deriving (Show, Eq)

-- NOTE: different from CStructDeclaratorList
data CStructDeclarationList
  -- CStructDeclaration
  = CStructDeclarationListSingleton (PE CStructDeclaration)
  -- CStructDeclaration CStructDeclarationList
  | CStructDeclarationList (PE CStructDeclaration) (PE CStructDeclarationList)
  deriving (Show, Eq)

-- NOTE: different from CStructDeclarator
-- CSpecifierQualifierList CStructDeclaratorList ;
data CStructDeclaration =
  CStructDeclaration (PE CSpecifierQualifierList) (PE CStructDeclaratorList)
  deriving (Show, Eq)

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierList (optional)
  = CSpecifierQualifierListSpecifier
      (PE CTypeSpecifier)
      (PE CSpecifierQualifierListOptional)
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecifierQualifierListQualifier
      (PE CTypeQualifier)
      (PE CSpecifierQualifierListOptional)
  deriving (Show, Eq)

data CSpecifierQualifierListOptional
  = CSpecifierQualifierListOptional (PE CSpecifierQualifierList)
  | CSpecifierQualifierListOptionalEmpty
  deriving (Show, Eq)

-- NOTE: different from CStructDeclarationList
-- CStructDeclarator CStructDeclaratorList'
data CStructDeclaratorList =
  CStructDeclaratorList (PE CStructDeclarator) (PE CStructDeclaratorList')
  deriving (Show, Eq)

data CStructDeclaratorList'
  -- empty
  = CStructDeclaratorList'Empty
  -- , CStructDeclarator CStructDeclaratorList'
  | CStructDeclaratorList' (PE CStructDeclarator) (PE CStructDeclaratorList')
  deriving (Show, Eq)

data CStructDeclarator -- NOTE: different from CStructDeclaration
  -- CDeclarator
  = CStructDeclarator (PE CDeclarator)
  -- CDeclarator (optional) : CConstantExpression
  | CStructDeclaratorInit (PE CDeclaratorOptional) (PE CConstantExpression)
  deriving (Show, Eq)

data CEnumSpecifier
  -- enum CIdentifier (optional) { CEnumeratorList }
  = CEnumSpecifierList (PE CIdentifierOptional) (PE CEnumeratorList)
  -- enum CIdentifier
  | CEnumSpecifier (PE CIdentifier)
  deriving (Show, Eq)

-- CEnumerator CEnumeratorList'
data CEnumeratorList =
  CEnumeratorList (PE CEnumerator) (PE CEnumeratorList')
  deriving (Show, Eq)

data CEnumeratorList'
  -- empty
  = CEnumeratorList'Empty
  -- , CEnumerator CEnumeratorList'
  | CEnumeratorList' (PE CEnumerator) (PE CEnumeratorList')
  deriving (Show, Eq)

data CEnumerator
  -- CEnumerationConstant
  = CEnumerator (PE CEnumerationConstant)
  -- CEnumerationConstant = CConstantExpression
  | CEnumeratorAssign (PE CEnumerationConstant) (PE CConstantExpression)
  deriving (Show, Eq)

-- CIdentifier
newtype CEnumerationConstant =
  CEnumerationConstant (PE CIdentifier)
  deriving (Show, Eq)

data CTypeQualifier
  = CTypeQualifierConst -- const
  | CTypeQualifierVolatile -- volatile
  deriving (Show, Eq)

-- CPointer (optional) CDirectDeclarator
data CDeclarator =
  CDeclarator (PE CPointerOptional) (PE CDirectDeclarator)
  deriving (Show, Eq)

data CDeclaratorOptional
  = CDeclaratorOptional (PE CDeclarator)
  | CDeclaratorOptionalEmpty
  deriving (Show, Eq)

data CDirectDeclarator
  -- CIdentifier CDirectDeclarator'
  = CDirectDeclaratorId (PE CIdentifier) (PE CDirectDeclarator')
  -- ( CDeclarator ) CDirectDeclarator'
  | CDirectDeclaratorParen (PE CDeclarator) (PE CDirectDeclarator')
  deriving (Show, Eq)

data CDirectDeclarator'
  -- empty
  = CDirectDeclarator'Empty
  -- [ CConstantExpression (optional) ] CDirectDeclarator'
  | CDirectDeclarator'Indexed
      (PE CConstantExpressionOptional)
      (PE CDirectDeclarator')
  -- ( CParameterTypeList ) CDirectDeclarator'
  | CDirectDeclarator'ParamTypeList
      (PE CParameterTypeList)
      (PE CDirectDeclarator')
  -- ( CIdentifierList (optional) ) CDirectDeclarator'
  | CDirectDeclarator'IdList
      (PE CIdentifierListOptional)
      (PE CDirectDeclarator')
  deriving (Show, Eq)

data CPointer
  -- * CTypeQualifierList (optional)
  = CPointerSingle (PE CTypeQualifierListOptional)
  -- * CTypeQualifierList (optional) CPointer
  | CPointerMulti (PE CTypeQualifierListOptional) (PE CPointer)
  deriving (Show, Eq)

data CPointerOptional
  = CPointerOptional (PE CPointer)
  | CPointerOptionalEmpty
  deriving (Show, Eq)

data CTypeQualifierList
  -- CTypeQualifier
  = CTypeQualifierListSingleton (PE CTypeQualifier)
  -- CTypeQualifier CTypeQualifierList
  | CTypeQualifierList (PE CTypeQualifier) (PE CTypeQualifierList)
  deriving (Show, Eq)

data CTypeQualifierListOptional
  = CTypeQualifierListOptional (PE CTypeQualifierList)
  | CTypeQualifierListOptionalEmpty
  deriving (Show, Eq)

data CParameterTypeList
  -- CParameterList
  = CParameterTypeList (PE CParameterList)
  -- CParameterList , ...
  | CParameterTypeListVarargs (PE CParameterList)
  deriving (Show, Eq)

data CParameterTypeListOptional
  = CParameterTypeListOptional (PE CParameterTypeList)
  | CParameterTypeListOptionalEmpty
  deriving (Show, Eq)

-- CParameterDeclaration CParameterList'
data CParameterList =
  CParameterList (PE CParameterDeclaration) (PE CParameterList')
  deriving (Show, Eq)

data CParameterList'
  -- empty
  = CParameterList'Empty
  -- , CParameterDeclaration CParameterList'
  | CParameterList' (PE CParameterDeclaration) (PE CParameterList')
  deriving (Show, Eq)

-- CParameterList , CParameterDeclaration
data CParameterDeclaration
  -- CDeclarationSpecifiers CDeclarator
  = CParameterDeclaration (PE CDeclarationSpecifiers) (PE CDeclarator)
  -- CDeclarationSpecifiers CAbstractDeclarator (optional)
  | CParameterDeclarationAbstract
      (PE CDeclarationSpecifiers)
      (PE CAbstractDeclaratorOptional)
  deriving (Show, Eq)

-- CIdentifier CIdentifierList'
data CIdentifierList =
  CIdentifierList (PE CIdentifier) (PE CIdentifierList')
  deriving (Show, Eq)

data CIdentifierList'
  -- empty
  = CIdentifierList'Empty
  -- , CIdentifier CIdentifierList'
  | CIdentifierList' (PE CIdentifier) (PE CIdentifierList')
  deriving (Show, Eq)

data CIdentifierListOptional
  = CIdentifierListOptional (PE CIdentifierList)
  | CIdentifierListOptionalEmpty
  deriving (Show, Eq)

-- CSpecifierQualifierList CAbstractDeclarator (optional)
data CTypeName =
  CTypeName (PE CSpecifierQualifierList) (PE CAbstractDeclaratorOptional)
  deriving (Show, Eq)

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer (PE CPointer)
  -- CPointer (optional) CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect
      (PE CPointerOptional)
      (PE CDirectAbstractDeclarator)
  deriving (Show, Eq)

data CAbstractDeclaratorOptional
  = CAbstractDeclaratorOptional (PE CAbstractDeclarator)
  | CAbstractDeclaratorOptionalEmpty
  deriving (Show, Eq)

data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator ) CDirectAbstractDeclarator'
  = CDirectAbstractDeclaratorParens
      (PE CAbstractDeclarator)
      (PE CDirectAbstractDeclarator')
  -- [ CConstantExpression (optional) ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorConst
      (PE CConstantExpressionOptional)
      (PE CDirectAbstractDeclarator')
  -- ( CParameterTypeList (optional) ) CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorParams
      (PE CParameterTypeListOptional)
      (PE CDirectAbstractDeclarator')
  deriving (Show, Eq)

data CDirectAbstractDeclarator'
  -- empty
  = CDirectAbstractDeclarator'Empty
  -- [ CConstantExpression (optional) ]
  | CDirectAbstractDeclarator'Const (PE CConstantExpressionOptional)
  -- ( CParameterTypeList (optional) )
  | CDirectAbstractDeclarator'Params (PE CParameterTypeListOptional)
  deriving (Show, Eq)

-- CIdentifier
newtype CTypedefName =
  CTypedefName (PE CIdentifier)
  deriving (Show, Eq)

data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment (PE CAssignmentExpression)
  -- { CInitializerList }
  | CInitializerBracketList (PE CInitializerList)
  -- { CInitializerList , }
  | CInitializerBracketListComma (PE CInitializerList)
  deriving (Show, Eq)

-- CInitializer CInitializerList'
data CInitializerList =
  CInitializerList (PE CInitializer) (PE CInitializerList')
  deriving (Show, Eq)

data CInitializerList'
  -- empty
  = CInitializerList'Empty
  -- , CInitializer CInitializerList'
  | CInitializerList' (PE CInitializer) (PE CInitializerList')
  deriving (Show, Eq)

data CStatement
  -- CLabeledStatement
  = CStatementLabeled (PE CLabeledStatement)
  -- CCompoundStatement
  | CStatementCompound (PE CCompoundStatement)
  -- CExpressionStatement
  | CStatementExpression (PE CExpressionStatement)
  -- CSelectionStatement
  | CStatementSelection (PE CSelectionStatement)
  -- CIterationStatement
  | CStatementIteration (PE CIterationStatement)
  -- CJumpStatement
  | CStatementJump (PE CJumpStatement)
  deriving (Show, Eq)

data CLabeledStatement
  -- CIdentifier : CStatement
  = CLabeledStatementId (PE CIdentifier) (PE CStatement)
  -- case CConstantExpression : CStatement
  | CLabeledStatementCase (PE CConstantExpression) (PE CStatement)
  -- default : CStatement
  | CLabeledStatementDefault (PE CStatement)
  deriving (Show, Eq)

-- { CDeclarationList (optional) CStatementList (optional) }
data CCompoundStatement =
  CCompoundStatement (PE CDeclarationListOptional) (PE CStatementListOptional)
  deriving (Show, Eq)

data CDeclarationList
  -- CDeclaration
  = CDeclarationListSingleton (PE CDeclaration)
  -- CDeclarationList CDeclaration
  | CDeclarationList (PE CDeclaration) (PE CDeclarationList)
  deriving (Show, Eq)

data CDeclarationListOptional
  = CDeclarationListOptional (PE CDeclarationList)
  | CDeclarationListOptionalEmpty
  deriving (Show, Eq)

data CStatementList
  -- CStatement
  = CStatementListSingleton (PE CStatement)
  -- CStatementList CStatement
  | CStatementList (PE CStatement) (PE CStatementList)
  deriving (Show, Eq)

data CStatementListOptional
  = CStatementListOptional (PE CStatementList)
  | CStatementListOptionalEmpty
  deriving (Show, Eq)

-- CExpression (optional) ;
newtype CExpressionStatement =
  CExpressionStatement (PE CExpressionOptional)
  deriving (Show, Eq)

data CSelectionStatement
  -- if ( CExpression ) CStatement
  = CSelectionStatementIf (PE CExpression) (PE CStatement)
  -- if ( CExpression ) CStatement else CStatement
  | CSelectionStatementIfElse (PE CExpression) (PE CStatement) (PE CStatement)
  -- switch ( CExpression ) CStatement
  | CSelectionStatementSwitch (PE CExpression) (PE CStatement)
  deriving (Show, Eq)

data CIterationStatement
  -- while ( CExpression ) CStatement
  = CIterationStatementWhile (PE CExpression) (PE CStatement)
  -- do CStatement while ( CExpression ) ;
  | CIterationStatementDoWhile (PE CStatement) (PE CExpression)
  -- for ( CExpression (optional) ; CExpression (optional) ;
  -- CExpression (optional) ) CStatement
  | CIterationStatementFor
      (PE CExpressionOptional)
      (PE CExpressionOptional)
      (PE CExpressionOptional)
      (PE CStatement)
  deriving (Show, Eq)

data CJumpStatement
  -- goto CIdentifier ;
  = CJumpStatementGoto (PE CIdentifier)
  -- continue ;
  | CJumpStatementContinue
  -- break ;
  | CJumpStatementBreak
  -- return CExpression (optional) ;
  | CJumpStatementReturn (PE CExpressionOptional)
  deriving (Show, Eq)

data CTranslationUnit
  -- CExternalDeclaration
  = CTranslationUnitExternal (PE CExternalDeclaration)
  -- CExternalDeclaration CTranslationUnit
  | CTranslationUnitTranslation (PE CExternalDeclaration) (PE CTranslationUnit)
  deriving (Show, Eq)

data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction (PE CFunctionDefinition)
  -- CDeclaration
  | CExternalDeclaration (PE CDeclaration)
  deriving (Show, Eq)

data CFunctionDefinition
  -- CDeclarationSpecifiers (optional) CDeclarator
  = CFunctionDefinitionSpecifiers
      (PE CDeclarationSpecifiersOptional)
      (PE CDeclarator)
  -- CDeclarationList (optional) CCompoundStatement
  | CFunctionDefinitionList
      (PE CDeclarationListOptional)
      (PE CCompoundStatement)
  deriving (Show, Eq)
