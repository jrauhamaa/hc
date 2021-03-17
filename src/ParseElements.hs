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
  CIdentifier (PE String)
  deriving (Show, Eq)

data CIdentifierOptional
  = CIdentifierOptionalEmpty
  | CIdentifierOptional (PE CIdentifier)
  deriving (Show, Eq)

-- CExternalDeclaration CTranslationUnitOptional
data CTranslationUnit =
  CTranslationUnit (PE CExternalDeclaration) (PE CTranslationUnitOptional)
  deriving (Show, Eq)

data CTranslationUnitOptional
  = CTranslationUnitOptionalEmpty
  | CTranslationUnitOptional (PE CTranslationUnit)
  deriving (Show, Eq)

data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction (PE CFunctionDefinition)
  -- CDeclaration
  | CExternalDeclaration (PE CDeclaration)
  deriving (Show, Eq)

-- CDeclarationSpecifiersOptional
--   CDeclarator
--   CDeclarationListOptional
--   CCompoundStatement
data CFunctionDefinition =
  CFunctionDefinition
    (PE CDeclarationSpecifiersOptional)
    (PE CDeclarator)
    (PE CDeclarationListOptional)
    (PE CCompoundStatement)
  deriving (Show, Eq)

-- CDeclarationSpecifiers CInitDeclaratorListOptional ;
data CDeclaration =
  CDeclaration (PE CDeclarationSpecifiers) (PE CInitDeclaratorListOptional)
  deriving (Show, Eq)

-- CDeclaration CDeclarationListOptional
data CDeclarationList =
  CDeclarationList (PE CDeclaration) (PE CDeclarationListOptional)
  deriving (Show, Eq)

data CDeclarationListOptional
  = CDeclarationListOptionalEmpty
  | CDeclarationListOptional (PE CDeclarationList)
  deriving (Show, Eq)

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiersOptional
  = CDeclarationSpecifiersStorageClass
      (PE CStorageClassSpecifier)
      (PE CDeclarationSpecifiersOptional)
  -- CTypeSpecifier CDeclarationSpecifiersOptional
  | CDeclarationSpecifiersTypeSpecifier
      (PE CTypeSpecifier)
      (PE CDeclarationSpecifiersOptional)
  -- CTypeQualifier CDeclarationSpecifiersOptional
  | CDeclarationSpecifiersTypeQualifier
      (PE CTypeQualifier)
      (PE CDeclarationSpecifiersOptional)
  deriving (Show, Eq)

data CDeclarationSpecifiersOptional
  = CDeclarationSpecifiersOptionalEmpty
  | CDeclarationSpecifiersOptional (PE CDeclarationSpecifiers)
  deriving (Show, Eq)

data CStorageClassSpecifier
  = CStorageClassSpecifierAuto      -- auto
  | CStorageClassSpecifierRegister  -- register
  | CStorageClassSpecifierStatic    -- static
  | CStorageClassSpecifierExtern    -- extern
  | CStorageClassSpecifierTypedef   -- typedef
  deriving (Show, Eq)

data CTypeSpecifier
  = CTypeSpecifierVoid      -- void
  | CTypeSpecifierChar      -- char
  | CTypeSpecifierShort     -- short
  | CTypeSpecifierInt       -- int
  | CTypeSpecifierLong      -- long
  | CTypeSpecifierFloat     -- float
  | CTypeSpecifierDouble    -- double
  | CTypeSpecifierSigned    -- signed
  | CTypeSpecifierUnsigned  -- unsigned
  -- CStructOrUnionSpecifier
  | CTypeSpecifierStructOrUnion (PE CStructOrUnionSpecifier)
  -- CEnumSpecifier
  | CTypeSpecifierEnum (PE CEnumSpecifier)
  -- CTypedefName
  | CTypeSpecifierTypedef (PE CTypedefName)
  deriving (Show, Eq)

data CTypeQualifier
  = CTypeQualifierConst     -- const
  | CTypeQualifierVolatile  -- volatile
  deriving (Show, Eq)

data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifierOptional { CStructDeclarationList }
  = CStructOrUnionSpecifierList
      (PE CStructOrUnion)
      (PE CIdentifierOptional)
      (PE CStructDeclarationList)
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier (PE CStructOrUnion) (PE CIdentifier)
  deriving (Show, Eq)

data CStructOrUnion
  = CStructOrUnionStruct    -- struct
  | CStructOrUnionUnion     -- union
  deriving (Show, Eq)

-- struct CDeclaration CStructDeclarationListOptional
data CStructDeclarationList =
  CStructDeclarationList (PE CDeclaration) (PE CStructDeclarationListOptional)
  deriving (Show, Eq)

data CStructDeclarationListOptional
  = CStructDeclarationListOptionalEmpty
  | CStructDeclarationListOptional (PE CStructDeclarationList)
  deriving (Show, Eq)

-- CInitDeclarator CInitDeclaratorList'
data CInitDeclaratorList =
  CInitDeclaratorList (PE CInitDeclarator) (PE CInitDeclaratorList')
  deriving (Show, Eq)

data CInitDeclaratorListOptional
  = CInitDeclaratorListOptionalEmpty
  | CInitDeclaratorListOptional (PE CInitDeclaratorList)
  deriving (Show, Eq)

data CInitDeclaratorList'
  -- empty
  = CInitDeclaratorList'Empty
  -- , CInitDeclarator CInitDeclaratorList'
  | CInitDeclaratorList' (PE CInitDeclarator) (PE CInitDeclaratorList')
  deriving (Show, Eq)

-- CDeclarator CAssignInitializerOptional
data CInitDeclarator =
  CInitDeclarator (PE CDeclarator) (PE CAssignInitializerOptional)
  deriving (Show, Eq)

data CAssignInitializerOptional
  -- empty
  = CAssignInitializerOptionalEmpty
  -- = CInitializer
  | CAssignInitializerOptional (PE CInitializer)
  deriving (Show, Eq)

-- CSpecifierQualifierList CStructDeclaratorList ;
data CStructDeclaration =
  CStructDeclaration (PE CSpecifierQualifierList) (PE CStructDeclaratorList)
  deriving (Show, Eq)

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierListOptional
  = CSpecifierQualifierListSpecifier
      (PE CTypeSpecifier)
      (PE CSpecifierQualifierListOptional)
  -- CTypeQualifier CSpecifierQualifierListOptional
  | CSpecifierQualifierListQualifier
      (PE CTypeQualifier)
      (PE CSpecifierQualifierListOptional)
  deriving (Show, Eq)

data CSpecifierQualifierListOptional
  = CSpecifierQualifierListOptionalEmpty
  | CSpecifierQualifierListOptional (PE CSpecifierQualifierList)
  deriving (Show, Eq)

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

data CStructDeclarator
  -- CDeclarator
  = CStructDeclarator (PE CDeclarator)
  -- CDeclaratorOptional : CConstantExpression
  | CStructDeclaratorField (PE CDeclaratorOptional) (PE CConstantExpression)
  deriving (Show, Eq)

data CEnumSpecifier
  -- enum CIdentifierOptional { CEnumeratorList }
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
  -- CIdentifier
  = CEnumerator (PE CIdentifier)
  -- CIdentifier = CConstantExpression
  | CEnumeratorAssign (PE CIdentifier) (PE CConstantExpression)
  deriving (Show, Eq)

-- CPointerOptional CDirectDeclarator
data CDeclarator =
  CDeclarator (PE CPointerOptional) (PE CDirectDeclarator)
  deriving (Show, Eq)

data CDeclaratorOptional
  = CDeclaratorOptionalEmpty
  | CDeclaratorOptional (PE CDeclarator)
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
  -- [ CConstantExpressionOptional ] CDirectDeclarator'
  | CDirectDeclarator'ConstExpr
      (PE CConstantExpressionOptional)
      (PE CDirectDeclarator')
  -- ( CParameterTypeList ) CDirectDeclarator'
  | CDirectDeclarator'ParamTypeList
      (PE CParameterTypeList)
      (PE CDirectDeclarator')
  -- ( CIdentifierListOptional ) CDirectDeclarator'
  | CDirectDeclarator'IdList
      (PE CIdentifierListOptional)
      (PE CDirectDeclarator')
  deriving (Show, Eq)

-- * CTypeQualifierListOptional CPointerOptional
data CPointer =
  CPointer (PE CTypeQualifierListOptional) (PE CPointerOptional)
  deriving (Show, Eq)

data CPointerOptional
  = CPointerOptionalEmpty
  | CPointerOptional (PE CPointer)
  deriving (Show, Eq)

-- CTypeQualifier CTypeQualifierListOptional
data CTypeQualifierList =
  CTypeQualifierList (PE CTypeQualifier) (PE CTypeQualifierListOptional)
  deriving (Show, Eq)

data CTypeQualifierListOptional
  = CTypeQualifierListOptionalEmpty
  | CTypeQualifierListOptional (PE CTypeQualifierList)
  deriving (Show, Eq)

-- CParameterList CVarArgsOptional
data CParameterTypeList
  = CParameterTypeList (PE CParameterList) (PE CVarArgsOptional)
  deriving (Show, Eq)

data CParameterTypeListOptional
  = CParameterTypeListOptionalEmpty
  | CParameterTypeListOptional (PE CParameterTypeList)
  deriving (Show, Eq)

data CVarArgsOptional
  -- empty
  = CVarArgsOptionalEmpty
  -- , ...
  | CVarArgsOptional
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

-- CDeclarationSpecifiers CParameterDeclaration'
data CParameterDeclaration
  = CParameterDeclaration
      (PE CDeclarationSpecifiers)
      (PE CParameterDeclaration')
  deriving (Show, Eq)

data CParameterDeclaration'
  -- CDeclarator
  = CParameterDeclaration' (PE CDeclarator)
  -- CAbstractDeclaratorOptional
  | CParameterDeclaration'Abstract (PE CAbstractDeclaratorOptional)
  deriving (Show, Eq)

-- CIdentifier CIdentifierList'
data CIdentifierList =
  CIdentifierList (PE CIdentifier) (PE CIdentifierList')
  deriving (Show, Eq)

data CIdentifierListOptional
  = CIdentifierListOptionalEmpty
  | CIdentifierListOptional (PE CIdentifierList)
  deriving (Show, Eq)

data CIdentifierList'
  -- empty
  = CIdentifierList'Empty
  -- , CIdentifier CIdentifierList'
  | CIdentifierList' (PE CIdentifier) (PE CIdentifierList')
  deriving (Show, Eq)

data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment (PE CAssignmentExpression)
  -- { CInitializerList }
  -- { CInitializerList , }
  | CInitializerInitList (PE CInitializerList)
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

-- CSpecifierQualifierList CAbstractDeclaratorOptional
data CTypeName =
  CTypeName (PE CSpecifierQualifierList) (PE CAbstractDeclaratorOptional)
  deriving (Show, Eq)

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer (PE CPointer)
  -- CPointerOptional CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect
      (PE CPointerOptional)
      (PE CDirectAbstractDeclarator)
  deriving (Show, Eq)

data CAbstractDeclaratorOptional
  = CAbstractDeclaratorOptionalEmpty
  | CAbstractDeclaratorOptional (PE CAbstractDeclarator)
  deriving (Show, Eq)

data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator ) CDirectAbstractDeclarator'
  = CDirectAbstractDeclaratorParens
      (PE CAbstractDeclarator)
      (PE CDirectAbstractDeclarator')
  -- [ CConstantExpressionOptional ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorConst
      (PE CConstantExpressionOptional)
      (PE CDirectAbstractDeclarator')
  -- [ CParameterTypeListOptional ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorParams
      (PE CParameterTypeListOptional)
      (PE CDirectAbstractDeclarator')
  deriving (Show, Eq)

data CDirectAbstractDeclarator'
  -- [ CConstantExpressionOptional ] CDirectAbstractDeclarator'
  = CDirectAbstractDeclarator'Const
      (PE CConstantExpressionOptional)
      (PE CDirectAbstractDeclarator')
  -- [ CParameterTypeListOptional ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclarator'Params
      (PE CParameterTypeListOptional)
      (PE CDirectAbstractDeclarator')
  deriving (Show, Eq)

-- CIdentifier
newtype CTypedefName =
  CTypedefName (PE CIdentifier)
  deriving (Show, Eq)

data CStatement
  -- CLabeledStatement
  = CStatementLabeled (PE CLabeledStatement)
  -- CExpressionStatement
  | CStatementExpression (PE CExpressionStatement)
  -- CCompoundStatement
  | CStatementCompound (PE CCompoundStatement)
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

-- CExpressionOptional ;
newtype CExpressionStatement =
  CExpressionStatement (PE CExpressionOptional)
  deriving (Show, Eq)

-- { CDeclarationListOptional CStatementListOptional }
data CCompoundStatement =
  CCompoundStatement (PE CDeclarationListOptional) (PE CStatementListOptional)
  deriving (Show, Eq)

-- CStatement CStatementListOptional
data CStatementList =
  CStatementList (PE CStatement) (PE CStatementListOptional)
  deriving (Show, Eq)

data CStatementListOptional
  = CStatementListOptionalEmpty
  | CStatementListOptional (PE CStatementList)
  deriving (Show, Eq)

data CSelectionStatement
  -- if ( CExpression ) CStatement CElseOptional
  = CSelectionStatementIf (PE CExpression) (PE CStatement) (PE CElseOptional)
  -- switch ( CExpression ) CStatement
  | CSelectionStatementSwitch (PE CExpression) (PE CStatement)
  deriving (Show, Eq)

data CElseOptional
  -- empty
  = CElseOptionalEmpty
  -- else CStatement
  | CElseOptional (PE CStatement)
  deriving (Show, Eq)

data CIterationStatement
  -- while ( CExpression ) CStatement
  = CIterationStatementWhile (PE CExpression) (PE CStatement)
  -- do CStatement while ( CExpression ) ;
  | CIterationStatementDoWhile (PE CStatement) (PE CExpression)
  -- for ( CExpressionOptional
  --     ; CExpressionOptional
  --     ; CExpressionOptional
  --     ) CStatement
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
  -- return CExpressionOptional ;
  | CJumpStatementReturn (PE CExpressionOptional)
  deriving (Show, Eq)

-- CAssignmentExpression CExpression'
data CExpression =
  CExpression (PE CAssignmentExpression) (PE CExpression')
  deriving (Show, Eq)

data CExpressionOptional
  = CExpressionOptionalEmpty
  | CExpressionOptional (PE CExpression)
  deriving (Show, Eq)

data CExpression'
  -- empty
  = CExpression'Empty
  -- , CAssignmentExpression CExpression'
  | CExpression' (PE CAssignmentExpression) (PE CExpression')
  deriving (Show, Eq)

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionConditional (PE CConditionalExpression)
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpression
      (PE CUnaryExpression)
      (PE CAssignmentOperator)
      (PE CAssignmentExpression)
  deriving (Show, Eq)

data CAssignmentOperator
  = CAssignmentOperatorAssign       -- =
  | CAssignmentOperatorMul          -- *=
  | CAssignmentOperatorDiv          -- /=
  | CAssignmentOperatorMod          -- %=
  | CAssignmentOperatorAdd          -- +=
  | CAssignmentOperatorSub          -- -=
  | CAssignmentOperatorLShift       -- <<=
  | CAssignmentOperatorRShfit       -- >>=
  | CAssignmentOperatorAnd          -- &=
  | CAssignmentOperatorXor          -- ^=
  | CAssignmentOperatorOr           -- |=
  deriving (Show, Eq)

-- CLogicalOrExpression CTernaryOptional
data CConditionalExpression =
  CConditionalExpression (PE CLogicalOrExpression) (PE CTernaryOptional)
  deriving (Show, Eq)

data CTernaryOptional
  -- empty
  = CTernaryOptionalEmpty
  -- ? CExpression : CConditionalExpression
  | CTernaryOptional (PE CExpression) (PE CConditionalExpression)
  deriving (Show, Eq)

-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression (PE CConditionalExpression)
  deriving (Show, Eq)

data CConstantExpressionOptional
  = CConstantExpressionOptionalEmpty
  | CConstantExpressionOptional (PE CConstantExpression)
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

-- CLogicalAndExpression CInclusiveOrExpression CLogicalAndExpression'
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
  | CEqualityExpression'NEQ
      (PE CRelationalExpression)
      (PE CEqualityExpression')
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

-- CMultiplicativeExpression CAdditiveExpression'
data CAdditiveExpression =
  CAdditiveExpression (PE CMultiplicativeExpression) (PE CAdditiveExpression')
  deriving (Show, Eq)

data CAdditiveExpression'
  -- empty
  = CAdditiveExpression'Empty
  -- + CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Add
      (PE CMultiplicativeExpression)
      (PE CAdditiveExpression')
  -- - CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Sub
      (PE CMultiplicativeExpression)
      (PE CAdditiveExpression')
  deriving (Show, Eq)

-- CCastExpression CMultiplicativeExpression'
data CMultiplicativeExpression =
  CMultiplicativeExpression
    (PE CCastExpression)
    (PE CMultiplicativeExpression')
  deriving (Show, Eq)

-- empty option not documented in the book
-- but its absence would result in infinite recursion
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

data CCastExpression
  -- CUnaryExpression
  = CCastExpressionUnary (PE CUnaryExpression)
  -- ( CTypeName ) CCastExpression
  | CCastExpression (PE CTypeName) (PE CCastExpression)
  deriving (Show, Eq)

data CUnaryExpression
  -- CPostfixExpression
  = CUnaryExpressionPostfix (PE CPostfixExpression)
  -- ++ CUnaryExpression
  | CUnaryExpressionInc (PE CUnaryExpression)
  -- -- CUnaryExpression
  | CUnaryExpressionDec (PE CUnaryExpression)
  -- CUnaryOperator CCastExpression
  | CUnaryExpressionUnaryOp (PE CUnaryOperator) (PE CCastExpression)
  -- sizeof CUnaryExpression
  | CUnaryExpressionSizeof (PE CUnaryExpression)
  -- sizeof ( CTypeName )
  | CUnaryExpressionSizeofType (PE CTypeName)
  deriving (Show, Eq)

data CUnaryOperator
  = CUnaryOperatorAnd           -- &
  | CUnaryOperatorMul           -- *
  | CUnaryOperatorAdd           -- +
  | CUnaryOperatorSub           -- -
  | CUnaryOperatorBitwiseNot    -- ~
  | CUnaryOperatorNot           -- !
  deriving (Show, Eq)

-- CPrimaryExpression CPostfixExpression'
data CPostfixExpression =
  CPostfixExpression (PE CPrimaryExpression) (PE CPostfixExpression')
  deriving (Show, Eq)

data CPostfixExpression'
  -- empty
  = CPostfixExpression'Empty
  -- [ CExpression ] CPostfixExpression'
  | CPostfixExpression'Bracket (PE CExpression) (PE CPostfixExpression')
  -- ( CArgumentExpressionListOptional ) CPostfixExpression'
  | CPostfixExpression'Paren
      (PE CArgumentExpressionListOptional)
      (PE CPostfixExpression')
  -- . CIdentifier CPostfixExpression'
  | CPostfixExpression'Dot (PE CIdentifier) (PE CPostfixExpression')
  -- -> CIdentifier CPostfixExpression'
  | CPostfixExpression'Arrow (PE CIdentifier) (PE CPostfixExpression')
  -- ++ CPostfixExpression'
  | CPostfixExpression'Inc (PE CPostfixExpression')
  -- -- CPostfixExpression'
  | CPostfixExpression'Dec (PE CPostfixExpression')
  deriving (Show, Eq)

data CPrimaryExpression
  -- CIdentifier
  = CPrimaryExpressionId (PE CIdentifier)
  -- CConstant
  | CPrimaryExpressionConst (PE CConstant)
  -- string literal
  | CPrimaryExpressionString (PE String)
  -- ( CExpression )
  | CPrimaryExpressionParen (PE CExpression)
  deriving (Show, Eq)

-- CAssignmentExpression CArgumentExpressionList'
data CArgumentExpressionList =
  CArgumentExpressionList
    (PE CAssignmentExpression)
    (PE CArgumentExpressionList')
  deriving (Show, Eq)

data CArgumentExpressionListOptional
  = CArgumentExpressionListOptionalEmpty
  | CArgumentExpressionListOptional (PE CArgumentExpressionList)
  deriving (Show, Eq)

data CArgumentExpressionList'
  -- empty
  = CArgumentExpressionList'Empty
  -- , CAssignmentExpression CArgumentExpressionList'
  | CArgumentExpressionList'
      (PE CAssignmentExpression)
      (PE CArgumentExpressionList')
  deriving (Show, Eq)

data CConstant
  -- int literal
  = CConstantInt (PE Int)
  -- char literal
  | CConstantChar (PE Char)
  -- float literal
  | CConstantFloat (PE Double)
  -- CIdentifier
  | CConstantEnum (PE CIdentifier)
  deriving (Show, Eq)

