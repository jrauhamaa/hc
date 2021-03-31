module ParseItem where

import Lexeme (CLexeme)
import Scanner (Coordinates, ScanItem(..))

data ParseItem a =
  ParseItem
    { parseLoc :: Coordinates
    , scanItems :: [ScanItem CLexeme]
    , parseItem :: a
    }
  deriving (Eq)

instance Show a => Show (ParseItem a) where
  show (ParseItem l items item) =
    mconcat
      [ "{ "
      , show l
      , " "
      , show $ mconcat $ map scanStr items
      , " "
      , show item
      ]

instance Functor ParseItem where
  fmap fab pia =
    ParseItem
      { parseLoc = parseLoc pia
      , scanItems = scanItems pia
      , parseItem = fab $ parseItem pia
      }

instance Applicative ParseItem where
  pure a = ParseItem (1, 1) [] a
  piab <*> pia =
    ParseItem
      { parseLoc = parseLoc pia
      , scanItems = scanItems piab <> scanItems piab
      , parseItem = parseItem piab $ parseItem pia
      }

type PI = ParseItem

newtype CIdentifier =
  CIdentifier (PI String)
  deriving (Show, Eq)

----------------
-- PARSEITEMS --
----------------

data CIdentifierOptional
  = CIdentifierOptionalEmpty
  | CIdentifierOptional (PI CIdentifier)
  deriving (Show, Eq)

-- CExternalDeclaration CTranslationUnitOptional
data CTranslationUnit =
  CTranslationUnit (PI CExternalDeclaration) (PI CTranslationUnitOptional)
  deriving (Show, Eq)

data CTranslationUnitOptional
  = CTranslationUnitOptionalEmpty
  | CTranslationUnitOptional (PI CTranslationUnit)
  deriving (Show, Eq)

data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction (PI CFunctionDefinition)
  -- CDeclaration
  | CExternalDeclaration (PI CDeclaration)
  deriving (Show, Eq)

-- CDeclarationSpecifiersOptional
--   CDeclarator
--   CDeclarationListOptional
--   CCompoundStatement
data CFunctionDefinition =
  CFunctionDefinition
    (PI CDeclarationSpecifiersOptional)
    (PI CDeclarator)
    (PI CDeclarationListOptional)
    (PI CCompoundStatement)
  deriving (Show, Eq)

-- CDeclarationSpecifiers CInitDeclaratorListOptional ;
data CDeclaration =
  CDeclaration (PI CDeclarationSpecifiers) (PI CInitDeclaratorListOptional)
  deriving (Show, Eq)

-- CDeclaration CDeclarationListOptional
data CDeclarationList =
  CDeclarationList (PI CDeclaration) (PI CDeclarationListOptional)
  deriving (Show, Eq)

data CDeclarationListOptional
  = CDeclarationListOptionalEmpty
  | CDeclarationListOptional (PI CDeclarationList)
  deriving (Show, Eq)

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiersOptional
  = CDeclarationSpecifiersStorageClass
      (PI CStorageClassSpecifier)
      (PI CDeclarationSpecifiersOptional)
  -- CTypeSpecifier CDeclarationSpecifiersOptional
  | CDeclarationSpecifiersTypeSpecifier
      (PI CTypeSpecifier)
      (PI CDeclarationSpecifiersOptional)
  -- CTypeQualifier CDeclarationSpecifiersOptional
  | CDeclarationSpecifiersTypeQualifier
      (PI CTypeQualifier)
      (PI CDeclarationSpecifiersOptional)
  deriving (Show, Eq)

data CDeclarationSpecifiersOptional
  = CDeclarationSpecifiersOptionalEmpty
  | CDeclarationSpecifiersOptional (PI CDeclarationSpecifiers)
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
  | CTypeSpecifierStructOrUnion (PI CStructOrUnionSpecifier)
  -- CEnumSpecifier
  | CTypeSpecifierEnum (PI CEnumSpecifier)
  -- CTypedefName
  | CTypeSpecifierTypedef (PI CTypedefName)
  deriving (Show, Eq)

data CTypeQualifier
  = CTypeQualifierConst     -- const
  | CTypeQualifierVolatile  -- volatile
  deriving (Show, Eq)

data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifierOptional { CStructDeclarationList }
  = CStructOrUnionSpecifierList
      (PI CStructOrUnion)
      (PI CIdentifierOptional)
      (PI CStructDeclarationList)
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier (PI CStructOrUnion) (PI CIdentifier)
  deriving (Show, Eq)

data CStructOrUnion
  = CStructOrUnionStruct    -- struct
  | CStructOrUnionUnion     -- union
  deriving (Show, Eq)

-- CStructDeclaration CStructDeclarationListOptional
data CStructDeclarationList =
  CStructDeclarationList
    (PI CStructDeclaration)
    (PI CStructDeclarationListOptional)
  deriving (Show, Eq)

data CStructDeclarationListOptional
  = CStructDeclarationListOptionalEmpty
  | CStructDeclarationListOptional (PI CStructDeclarationList)
  deriving (Show, Eq)

-- CInitDeclarator CInitDeclaratorList'
data CInitDeclaratorList =
  CInitDeclaratorList (PI CInitDeclarator) (PI CInitDeclaratorList')
  deriving (Show, Eq)

data CInitDeclaratorListOptional
  = CInitDeclaratorListOptionalEmpty
  | CInitDeclaratorListOptional (PI CInitDeclaratorList)
  deriving (Show, Eq)

data CInitDeclaratorList'
  -- empty
  = CInitDeclaratorList'Empty
  -- , CInitDeclarator CInitDeclaratorList'
  | CInitDeclaratorList' (PI CInitDeclarator) (PI CInitDeclaratorList')
  deriving (Show, Eq)

-- CDeclarator CAssignInitializerOptional
data CInitDeclarator =
  CInitDeclarator (PI CDeclarator) (PI CAssignInitializerOptional)
  deriving (Show, Eq)

data CAssignInitializerOptional
  -- empty
  = CAssignInitializerOptionalEmpty
  -- = CInitializer
  | CAssignInitializerOptional (PI CInitializer)
  deriving (Show, Eq)

-- CSpecifierQualifierList CStructDeclaratorList ;
data CStructDeclaration =
  CStructDeclaration (PI CSpecifierQualifierList) (PI CStructDeclaratorList)
  deriving (Show, Eq)

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierListOptional
  = CSpecifierQualifierListSpecifier
      (PI CTypeSpecifier)
      (PI CSpecifierQualifierListOptional)
  -- CTypeQualifier CSpecifierQualifierListOptional
  | CSpecifierQualifierListQualifier
      (PI CTypeQualifier)
      (PI CSpecifierQualifierListOptional)
  deriving (Show, Eq)

data CSpecifierQualifierListOptional
  = CSpecifierQualifierListOptionalEmpty
  | CSpecifierQualifierListOptional (PI CSpecifierQualifierList)
  deriving (Show, Eq)

-- CStructDeclarator CStructDeclaratorList'
data CStructDeclaratorList =
  CStructDeclaratorList (PI CStructDeclarator) (PI CStructDeclaratorList')
  deriving (Show, Eq)

data CStructDeclaratorList'
  -- empty
  = CStructDeclaratorList'Empty
  -- , CStructDeclarator CStructDeclaratorList'
  | CStructDeclaratorList' (PI CStructDeclarator) (PI CStructDeclaratorList')
  deriving (Show, Eq)

data CStructDeclarator
  -- CDeclarator
  = CStructDeclarator (PI CDeclarator)
  -- CDeclaratorOptional : CConstantExpression
  | CStructDeclaratorField (PI CDeclaratorOptional) (PI CConstantExpression)
  deriving (Show, Eq)

data CEnumSpecifier
  -- enum CIdentifierOptional { CEnumeratorList }
  = CEnumSpecifierList (PI CIdentifierOptional) (PI CEnumeratorList)
  -- enum CIdentifier
  | CEnumSpecifier (PI CIdentifier)
  deriving (Show, Eq)

-- CEnumerator CEnumeratorList'
data CEnumeratorList =
  CEnumeratorList (PI CEnumerator) (PI CEnumeratorList')
  deriving (Show, Eq)

data CEnumeratorList'
  -- empty
  = CEnumeratorList'Empty
  -- , CEnumerator CEnumeratorList'
  | CEnumeratorList' (PI CEnumerator) (PI CEnumeratorList')
  deriving (Show, Eq)

data CEnumerator
  -- CIdentifier
  = CEnumerator (PI CIdentifier)
  -- CIdentifier = CConstantExpression
  | CEnumeratorAssign (PI CIdentifier) (PI CConstantExpression)
  deriving (Show, Eq)

-- CPointerOptional CDirectDeclarator
data CDeclarator =
  CDeclarator (PI CPointerOptional) (PI CDirectDeclarator)
  deriving (Show, Eq)

data CDeclaratorOptional
  = CDeclaratorOptionalEmpty
  | CDeclaratorOptional (PI CDeclarator)
  deriving (Show, Eq)

data CDirectDeclarator
  -- CIdentifier CDirectDeclarator'
  = CDirectDeclaratorId (PI CIdentifier) (PI CDirectDeclarator')
  -- ( CDeclarator ) CDirectDeclarator'
  | CDirectDeclaratorParen (PI CDeclarator) (PI CDirectDeclarator')
  deriving (Show, Eq)

data CDirectDeclarator'
  -- empty
  = CDirectDeclarator'Empty
  -- [ CConstantExpressionOptional ] CDirectDeclarator'
  | CDirectDeclarator'ConstExpr
      (PI CConstantExpressionOptional)
      (PI CDirectDeclarator')
  -- ( CParameterTypeList ) CDirectDeclarator'
  | CDirectDeclarator'ParamTypeList
      (PI CParameterTypeList)
      (PI CDirectDeclarator')
  -- ( CIdentifierListOptional ) CDirectDeclarator'
  | CDirectDeclarator'IdList
      (PI CIdentifierListOptional)
      (PI CDirectDeclarator')
  deriving (Show, Eq)

-- * CTypeQualifierListOptional CPointerOptional
data CPointer =
  CPointer (PI CTypeQualifierListOptional) (PI CPointerOptional)
  deriving (Show, Eq)

data CPointerOptional
  = CPointerOptionalEmpty
  | CPointerOptional (PI CPointer)
  deriving (Show, Eq)

-- CTypeQualifier CTypeQualifierListOptional
data CTypeQualifierList =
  CTypeQualifierList (PI CTypeQualifier) (PI CTypeQualifierListOptional)
  deriving (Show, Eq)

data CTypeQualifierListOptional
  = CTypeQualifierListOptionalEmpty
  | CTypeQualifierListOptional (PI CTypeQualifierList)
  deriving (Show, Eq)

-- CParameterList CVarArgsOptional
data CParameterTypeList
  = CParameterTypeList (PI CParameterList) (PI CVarArgsOptional)
  deriving (Show, Eq)

data CParameterTypeListOptional
  = CParameterTypeListOptionalEmpty
  | CParameterTypeListOptional (PI CParameterTypeList)
  deriving (Show, Eq)

data CVarArgsOptional
  -- empty
  = CVarArgsOptionalEmpty
  -- , ...
  | CVarArgsOptional
  deriving (Show, Eq)

-- CParameterDeclaration CParameterList'
data CParameterList =
  CParameterList (PI CParameterDeclaration) (PI CParameterList')
  deriving (Show, Eq)

data CParameterList'
  -- empty
  = CParameterList'Empty
  -- , CParameterDeclaration CParameterList'
  | CParameterList' (PI CParameterDeclaration) (PI CParameterList')
  deriving (Show, Eq)

-- CDeclarationSpecifiers CParameterDeclaration'
data CParameterDeclaration
  = CParameterDeclaration
      (PI CDeclarationSpecifiers)
      (PI CParameterDeclaration')
  deriving (Show, Eq)

data CParameterDeclaration'
  -- CDeclarator
  = CParameterDeclaration' (PI CDeclarator)
  -- CAbstractDeclaratorOptional
  | CParameterDeclaration'Abstract (PI CAbstractDeclaratorOptional)
  deriving (Show, Eq)

-- CIdentifier CIdentifierList'
data CIdentifierList =
  CIdentifierList (PI CIdentifier) (PI CIdentifierList')
  deriving (Show, Eq)

data CIdentifierListOptional
  = CIdentifierListOptionalEmpty
  | CIdentifierListOptional (PI CIdentifierList)
  deriving (Show, Eq)

data CIdentifierList'
  -- empty
  = CIdentifierList'Empty
  -- , CIdentifier CIdentifierList'
  | CIdentifierList' (PI CIdentifier) (PI CIdentifierList')
  deriving (Show, Eq)

data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment (PI CAssignmentExpression)
  -- { CInitializerList }
  -- { CInitializerList , }
  | CInitializerInitList (PI CInitializerList)
  deriving (Show, Eq)

-- CInitializer CInitializerList'
data CInitializerList =
  CInitializerList (PI CInitializer) (PI CInitializerList')
  deriving (Show, Eq)

data CInitializerList'
  -- empty
  = CInitializerList'Empty
  -- , CInitializer CInitializerList'
  | CInitializerList' (PI CInitializer) (PI CInitializerList')
  deriving (Show, Eq)

-- CSpecifierQualifierList CAbstractDeclaratorOptional
data CTypeName =
  CTypeName (PI CSpecifierQualifierList) (PI CAbstractDeclaratorOptional)
  deriving (Show, Eq)

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer (PI CPointer)
  -- CPointerOptional CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect
      (PI CPointerOptional)
      (PI CDirectAbstractDeclarator)
  deriving (Show, Eq)

data CAbstractDeclaratorOptional
  = CAbstractDeclaratorOptionalEmpty
  | CAbstractDeclaratorOptional (PI CAbstractDeclarator)
  deriving (Show, Eq)

-- ( CAbstractDeclarator ) CDirectAbstractDeclarator'
data CDirectAbstractDeclarator
  = CDirectAbstractDeclarator
      (PI CAbstractDeclarator)
      (PI CDirectAbstractDeclarator')
  deriving (Show, Eq)

data CDirectAbstractDeclarator'
  -- [ CConstantExpressionOptional ] CDirectAbstractDeclarator'
  = CDirectAbstractDeclarator'Const
      (PI CConstantExpressionOptional)
      (PI CDirectAbstractDeclarator')
  -- ( CParameterTypeListOptional ) CDirectAbstractDeclarator'
  | CDirectAbstractDeclarator'Params
      (PI CParameterTypeListOptional)
      (PI CDirectAbstractDeclarator')
  -- empty
  | CDirectAbstractDeclarator'Empty
  deriving (Show, Eq)

-- CIdentifier
newtype CTypedefName =
  CTypedefName (PI CIdentifier)
  deriving (Show, Eq)

data CStatement
  -- CLabeledStatement
  = CStatementLabeled (PI CLabeledStatement)
  -- CExpressionStatement
  | CStatementExpression (PI CExpressionStatement)
  -- CCompoundStatement
  | CStatementCompound (PI CCompoundStatement)
  -- CSelectionStatement
  | CStatementSelection (PI CSelectionStatement)
  -- CIterationStatement
  | CStatementIteration (PI CIterationStatement)
  -- CJumpStatement
  | CStatementJump (PI CJumpStatement)
  deriving (Show, Eq)

data CLabeledStatement
  -- CIdentifier : CStatement
  = CLabeledStatementId (PI CIdentifier) (PI CStatement)
  -- case CConstantExpression : CStatement
  | CLabeledStatementCase (PI CConstantExpression) (PI CStatement)
  -- default : CStatement
  | CLabeledStatementDefault (PI CStatement)
  deriving (Show, Eq)

-- CExpressionOptional ;
newtype CExpressionStatement =
  CExpressionStatement (PI CExpressionOptional)
  deriving (Show, Eq)

-- { CDeclarationListOptional CStatementListOptional }
data CCompoundStatement =
  CCompoundStatement (PI CDeclarationListOptional) (PI CStatementListOptional)
  deriving (Show, Eq)

-- CStatement CStatementListOptional
data CStatementList =
  CStatementList (PI CStatement) (PI CStatementListOptional)
  deriving (Show, Eq)

data CStatementListOptional
  = CStatementListOptionalEmpty
  | CStatementListOptional (PI CStatementList)
  deriving (Show, Eq)

data CSelectionStatement
  -- if ( CExpression ) CStatement CElseOptional
  = CSelectionStatementIf (PI CExpression) (PI CStatement) (PI CElseOptional)
  -- switch ( CExpression ) CStatement
  | CSelectionStatementSwitch (PI CExpression) (PI CStatement)
  deriving (Show, Eq)

data CElseOptional
  -- empty
  = CElseOptionalEmpty
  -- else CStatement
  | CElseOptional (PI CStatement)
  deriving (Show, Eq)

data CIterationStatement
  -- while ( CExpression ) CStatement
  = CIterationStatementWhile (PI CExpression) (PI CStatement)
  -- do CStatement while ( CExpression ) ;
  | CIterationStatementDoWhile (PI CStatement) (PI CExpression)
  -- for ( CExpressionOptional
  --     ; CExpressionOptional
  --     ; CExpressionOptional
  --     ) CStatement
  | CIterationStatementFor
      (PI CExpressionOptional)
      (PI CExpressionOptional)
      (PI CExpressionOptional)
      (PI CStatement)
  deriving (Show, Eq)

data CJumpStatement
  -- goto CIdentifier ;
  = CJumpStatementGoto (PI CIdentifier)
  -- continue ;
  | CJumpStatementContinue
  -- break ;
  | CJumpStatementBreak
  -- return CExpressionOptional ;
  | CJumpStatementReturn (PI CExpressionOptional)
  deriving (Show, Eq)

-- CAssignmentExpression CExpression'
data CExpression =
  CExpression (PI CAssignmentExpression) (PI CExpression')
  deriving (Show, Eq)

data CExpressionOptional
  = CExpressionOptionalEmpty
  | CExpressionOptional (PI CExpression)
  deriving (Show, Eq)

data CExpression'
  -- empty
  = CExpression'Empty
  -- , CAssignmentExpression CExpression'
  | CExpression' (PI CAssignmentExpression) (PI CExpression')
  deriving (Show, Eq)

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionConditional (PI CConditionalExpression)
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpression
      (PI CUnaryExpression)
      (PI CAssignmentOperator)
      (PI CAssignmentExpression)
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
  CConditionalExpression (PI CLogicalOrExpression) (PI CTernaryOptional)
  deriving (Show, Eq)

data CTernaryOptional
  -- empty
  = CTernaryOptionalEmpty
  -- ? CExpression : CConditionalExpression
  | CTernaryOptional (PI CExpression) (PI CConditionalExpression)
  deriving (Show, Eq)

-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression (PI CConditionalExpression)
  deriving (Show, Eq)

data CConstantExpressionOptional
  = CConstantExpressionOptionalEmpty
  | CConstantExpressionOptional (PI CConstantExpression)
  deriving (Show, Eq)

-- CLogicalAndExpression CLogicalOrExpression'
data CLogicalOrExpression =
  CLogicalOrExpression (PI CLogicalAndExpression) (PI CLogicalOrExpression')
  deriving (Show, Eq)

data CLogicalOrExpression'
  -- empty
  = CLogicalOrExpression'Empty
  -- || CLogicalAndExpression CLogicalOrExpression'
  | CLogicalOrExpression' (PI CLogicalAndExpression) (PI CLogicalOrExpression')
  deriving (Show, Eq)

-- CInclusiveOrExpression CLogicalAndExpression'
data CLogicalAndExpression =
  CLogicalAndExpression (PI CInclusiveOrExpression) (PI CLogicalAndExpression')
  deriving (Show, Eq)

data CLogicalAndExpression'
  -- empty
  = CLogicalAndExpression'Empty
  -- && CInclusiveOrExpression CLogicalAndExpression'
  | CLogicalAndExpression'
      (PI CInclusiveOrExpression)
      (PI CLogicalAndExpression')
  deriving (Show, Eq)

-- CExclusiveOrExpression CInclusiveOrExpression'
data CInclusiveOrExpression =
  CInclusiveOrExpression
    (PI CExclusiveOrExpression)
    (PI CInclusiveOrExpression')
  deriving (Show, Eq)

data CInclusiveOrExpression'
  -- empty
  = CInclusiveOrExpression'Empty
  -- | CExclusiveOrExpression CInclusiveOrExpression'
  | CInclusiveOrExpression'
    (PI CExclusiveOrExpression)
    (PI CInclusiveOrExpression')
  deriving (Show, Eq)

-- CAndExpression CExclusiveOrExpression'
data CExclusiveOrExpression =
  CExclusiveOrExpression (PI CAndExpression) (PI CExclusiveOrExpression')
  deriving (Show, Eq)

data CExclusiveOrExpression'
  -- empty
  = CExclusiveOrExpression'Empty
  -- ^ CAndExpression CExclusiveOrExpression'
  | CExclusiveOrExpression' (PI CAndExpression) (PI CExclusiveOrExpression')
  deriving (Show, Eq)

-- CEqualityExpression CAndExpression'
data CAndExpression =
  CAndExpression (PI CEqualityExpression) (PI CAndExpression')
  deriving (Show, Eq)

data CAndExpression'
  -- empty
  = CAndExpression'Empty
  -- & CEqualityExpression CAndExpression'
  | CAndExpression' (PI CEqualityExpression) (PI CAndExpression')
  deriving (Show, Eq)

-- CRelationalExpression CEqualityExpression'
data CEqualityExpression =
  CEqualityExpression (PI CRelationalExpression) (PI CEqualityExpression')
  deriving (Show, Eq)

data CEqualityExpression'
  -- empty
  = CEqualityExpression'Empty
  -- == CRelationalExpression CEqualityExpression'
  | CEqualityExpression'EQ (PI CRelationalExpression) (PI CEqualityExpression')
  -- != CRelationalExpression CEqualityExpression'
  | CEqualityExpression'NEQ
      (PI CRelationalExpression)
      (PI CEqualityExpression')
  deriving (Show, Eq)

-- CShiftExpression CRelationalExpression'
data CRelationalExpression =
  CRelationalExpression (PI CShiftExpression) (PI CRelationalExpression')
  deriving (Show, Eq)

data CRelationalExpression'
  -- empty
  = CRelationalExpression'Empty
  -- < CShiftExpression CRelationalExpression'
  | CRelationalExpression'LT (PI CShiftExpression) (PI CRelationalExpression')
  -- <= CShiftExpression CRelationalExpression'
  | CRelationalExpression'LTE (PI CShiftExpression) (PI CRelationalExpression')
  -- > CShiftExpression CRelationalExpression'
  | CRelationalExpression'GT (PI CShiftExpression) (PI CRelationalExpression')
  -- >= CShiftExpression CRelationalExpression'
  | CRelationalExpression'GTE (PI CShiftExpression) (PI CRelationalExpression')
  deriving (Show, Eq)

-- CAdditiveExpression CShiftExpression'
data CShiftExpression =
  CShiftExpression (PI CAdditiveExpression) (PI CShiftExpression')
  deriving (Show, Eq)

data CShiftExpression'
  -- empty
  = CShiftExpression'Empty
  -- << CAdditiveExpression CShiftExpression'
  | CShiftExpression'Left (PI CAdditiveExpression) (PI CShiftExpression')
  -- >> CAdditiveExpression CShiftExpression'
  | CShiftExpression'Right (PI CAdditiveExpression) (PI CShiftExpression')
  deriving (Show, Eq)

-- CMultiplicativeExpression CAdditiveExpression'
data CAdditiveExpression =
  CAdditiveExpression (PI CMultiplicativeExpression) (PI CAdditiveExpression')
  deriving (Show, Eq)

data CAdditiveExpression'
  -- empty
  = CAdditiveExpression'Empty
  -- + CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Add
      (PI CMultiplicativeExpression)
      (PI CAdditiveExpression')
  -- - CMultiplicativeExpression CAdditiveExpression'
  | CAdditiveExpression'Sub
      (PI CMultiplicativeExpression)
      (PI CAdditiveExpression')
  deriving (Show, Eq)

-- CCastExpression CMultiplicativeExpression'
data CMultiplicativeExpression =
  CMultiplicativeExpression
    (PI CCastExpression)
    (PI CMultiplicativeExpression')
  deriving (Show, Eq)

-- empty option not documented in the book
-- but its absence would result in infinite recursion
data CMultiplicativeExpression'
  -- empty
  = CMultiplicativeExpression'Empty
  -- * CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mul
      (PI CCastExpression)
      (PI CMultiplicativeExpression')
  -- / CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Div
      (PI CCastExpression)
      (PI CMultiplicativeExpression')
  -- % CCastExpression CMultiplicativeExpression'
  | CMultiplicativeExpression'Mod
      (PI CCastExpression)
      (PI CMultiplicativeExpression')
  deriving (Show, Eq)

data CCastExpression
  -- CUnaryExpression
  = CCastExpressionUnary (PI CUnaryExpression)
  -- ( CTypeName ) CCastExpression
  | CCastExpression (PI CTypeName) (PI CCastExpression)
  deriving (Show, Eq)

data CUnaryExpression
  -- CPostfixExpression
  = CUnaryExpressionPostfix (PI CPostfixExpression)
  -- ++ CUnaryExpression
  | CUnaryExpressionInc (PI CUnaryExpression)
  -- -- CUnaryExpression
  | CUnaryExpressionDec (PI CUnaryExpression)
  -- CUnaryOperator CCastExpression
  | CUnaryExpressionUnaryOp (PI CUnaryOperator) (PI CCastExpression)
  -- sizeof CUnaryExpression
  | CUnaryExpressionSizeof (PI CUnaryExpression)
  -- sizeof ( CTypeName )
  | CUnaryExpressionSizeofType (PI CTypeName)
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
  CPostfixExpression (PI CPrimaryExpression) (PI CPostfixExpression')
  deriving (Show, Eq)

data CPostfixExpression'
  -- empty
  = CPostfixExpression'Empty
  -- [ CExpression ] CPostfixExpression'
  | CPostfixExpression'Bracket (PI CExpression) (PI CPostfixExpression')
  -- ( CArgumentExpressionListOptional ) CPostfixExpression'
  | CPostfixExpression'Paren
      (PI CArgumentExpressionListOptional)
      (PI CPostfixExpression')
  -- . CIdentifier CPostfixExpression'
  | CPostfixExpression'Dot (PI CIdentifier) (PI CPostfixExpression')
  -- -> CIdentifier CPostfixExpression'
  | CPostfixExpression'Arrow (PI CIdentifier) (PI CPostfixExpression')
  -- ++ CPostfixExpression'
  | CPostfixExpression'Inc (PI CPostfixExpression')
  -- -- CPostfixExpression'
  | CPostfixExpression'Dec (PI CPostfixExpression')
  deriving (Show, Eq)

data CPrimaryExpression
  -- CIdentifier
  = CPrimaryExpressionId (PI CIdentifier)
  -- CConstant
  | CPrimaryExpressionConst (PI CConstant)
  -- string literal
  | CPrimaryExpressionString (PI String)
  -- ( CExpression )
  | CPrimaryExpressionParen (PI CExpression)
  deriving (Show, Eq)

-- CAssignmentExpression CArgumentExpressionList'
data CArgumentExpressionList =
  CArgumentExpressionList
    (PI CAssignmentExpression)
    (PI CArgumentExpressionList')
  deriving (Show, Eq)

data CArgumentExpressionListOptional
  = CArgumentExpressionListOptionalEmpty
  | CArgumentExpressionListOptional (PI CArgumentExpressionList)
  deriving (Show, Eq)

data CArgumentExpressionList'
  -- empty
  = CArgumentExpressionList'Empty
  -- , CAssignmentExpression CArgumentExpressionList'
  | CArgumentExpressionList'
      (PI CAssignmentExpression)
      (PI CArgumentExpressionList')
  deriving (Show, Eq)

data CConstant
  -- int literal
  = CConstantInt (PI Int)
  -- char literal
  | CConstantChar (PI Char)
  -- float literal
  | CConstantFloat (PI Double)
  -- CIdentifier
  | CConstantEnum (PI CIdentifier)
  deriving (Show, Eq)

