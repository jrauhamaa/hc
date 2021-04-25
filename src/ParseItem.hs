module ParseItem where

import qualified Data.Map as M

import Lexeme (CLexeme)
import Scanner (Coordinates, ScanItem(..))

data ParseItem a =
  ParseItem
    { parseLoc :: Coordinates
    , scanItems :: [ScanItem CLexeme]
    , parseItem :: a
    , symbolTable :: SymbolTable
    }
  deriving (Eq)

instance Show a => Show (ParseItem a) where
  show (ParseItem l _ item _) =
    mconcat
      [ "{ "
      , show l
      , " "
      , show item
      , " }"
      ]

instance Functor ParseItem where
  fmap fab pia =
    ParseItem
      { parseLoc = parseLoc pia
      , scanItems = scanItems pia
      , parseItem = fab $ parseItem pia
      , symbolTable = initialSymbols
      }

instance Applicative ParseItem where
  pure a = ParseItem (1, 1) [] a initialSymbols
  (ParseItem l s fab sym) <*> (ParseItem _ s' a _) =
    ParseItem
      { parseLoc = l
      , scanItems = s <> s'
      , parseItem = fab $ a
      , symbolTable = sym
      }

type PI = ParseItem

data CDataType
  = TChar
  | TShort          -- int or short int
  | TLong
  | TLongLong
  | TUChar
  | TUShort         -- unsigned int or unsigned short
  | TULong
  | TULongLong
  | TFloat
  | TDouble
  | TLongDouble
  | TPointer CType
  | TArray CType (Maybe Int)
  | TUnion (Maybe String) (M.Map String CType)
  | TStruct (Maybe String) [(CType, Maybe String, Maybe Int)]
  | TFunction CType [CType]
  | TVarArgFunction CType [CType]
  | TEnum (Maybe String) (M.Map String Int)
  | TVoid
  deriving (Show, Eq)

data CType =
  CType
    { storageClass  :: [CStorageClassSpecifier]
    , typeQualifier :: [CTypeQualifier]
    , dataType      :: CDataType
    }
  deriving (Show, Eq)

data SymbolTable =
  SymbolTable
    { typedef :: M.Map String (Coordinates, CType)
    , labels  :: M.Map String Coordinates
    , symbols :: M.Map String (Coordinates, CType)
    , parent  :: Maybe SymbolTable
    }
  deriving (Show, Eq)

initialSymbols :: SymbolTable
initialSymbols =
  SymbolTable
    { typedef = M.empty
    , labels  = M.empty
    , symbols = M.empty
    , parent  = Nothing
    }

----------------
-- PARSEITEMS --
----------------

-- typedef, function or variable name
newtype CIdentifier =
  CIdentifier String
  deriving (Show, Eq)

data CIdentifierOptional
  = CIdentifierOptionalEmpty
  | CIdentifierOptional (PI CIdentifier)
  deriving (Show, Eq)

-- the entire source file
-- CExternalDeclaration CTranslationUnitOptional
data CTranslationUnit =
  CTranslationUnit (PI CExternalDeclaration) (PI CTranslationUnitOptional)
  deriving (Show, Eq)

data CTranslationUnitOptional
  = CTranslationUnitOptionalEmpty
  | CTranslationUnitOptional (PI CTranslationUnit)
  deriving (Show, Eq)

-- declaratoin or function definition
data CExternalDeclaration
  -- CFunctionDefinition
  = CExternalDeclarationFunction (PI CFunctionDefinition)
  -- CDeclaration
  | CExternalDeclaration (PI CDeclaration)
  deriving (Show, Eq)

-- function definition (including function body)
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

-- declaration of variable or function. in case of variable it may be assigned.
-- CDeclarationSpecifiers CInitDeclaratorListOptional ;
data CDeclaration =
  CDeclaration (PI CDeclarationSpecifiers) (PI CInitDeclaratorListOptional)
  deriving (Show, Eq)

-- series of declarations
-- CDeclaration CDeclarationListOptional
data CDeclarationList =
  CDeclarationList (PI CDeclaration) (PI CDeclarationListOptional)
  deriving (Show, Eq)

data CDeclarationListOptional
  = CDeclarationListOptionalEmpty
  | CDeclarationListOptional (PI CDeclarationList)
  deriving (Show, Eq)

-- series of type or storage class specifiers
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

-- information regarding storage & scope
data CStorageClassSpecifier
  = CStorageClassSpecifierAuto      -- auto
  | CStorageClassSpecifierRegister  -- register
  | CStorageClassSpecifierStatic    -- static
  | CStorageClassSpecifierExtern    -- extern
  | CStorageClassSpecifierTypedef   -- typedef
  deriving (Show, Eq)

-- data type
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

-- information related to variable assignment
data CTypeQualifier
  = CTypeQualifierConst     -- const
  | CTypeQualifierVolatile  -- volatile
  deriving (Show, Eq)

-- definition of struct or union
data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifierOptional { CStructDeclarationList }
  = CStructOrUnionSpecifierList
      (PI CStructOrUnion)
      (PI CIdentifierOptional)
      (PI CStructDeclarationList)
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpecifier (PI CStructOrUnion) (PI CIdentifier)
  deriving (Show, Eq)

-- struct or union keyword
data CStructOrUnion
  = CStructOrUnionStruct    -- struct
  | CStructOrUnionUnion     -- union
  deriving (Show, Eq)

-- list of struct declarations (values inside struct)
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

-- series of init declarators
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

-- declarator with possible assignment
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

-- declaration of one or more values inside struct
-- CSpecifierQualifierList CStructDeclaratorList ;
data CStructDeclaration =
  CStructDeclaration (PI CSpecifierQualifierList) (PI CStructDeclaratorList)
  deriving (Show, Eq)

-- series of type specifiers & qualifiers (but not storage classes)
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

-- series of declarators of same type inside struct
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

-- declare value inside struct and optionally specify size in bits
data CStructDeclarator
  -- CDeclarator
  = CStructDeclarator (PI CDeclarator)
  -- CDeclaratorOptional : CConstantExpression
  | CStructDeclaratorField (PI CDeclaratorOptional) (PI CConstantExpression)
  deriving (Show, Eq)

-- specify enum data type
data CEnumSpecifier
  -- enum CIdentifierOptional { CEnumeratorList }
  = CEnumSpecifierList (PI CIdentifierOptional) (PI CEnumeratorList)
  -- enum CIdentifier
  | CEnumSpecifier (PI CIdentifier)
  deriving (Show, Eq)

-- list of possible values for enum type
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

-- a possible value for enum type
data CEnumerator
  -- CIdentifier
  = CEnumerator (PI CIdentifier)
  -- CIdentifier = CConstantExpression
  | CEnumeratorAssign (PI CIdentifier) (PI CConstantExpression)
  deriving (Show, Eq)

-- part of variable declaration between specifier list & possible assignment
-- CPointerOptional CDirectDeclarator
data CDeclarator =
  CDeclarator (PI CPointerOptional) (PI CDirectDeclarator)
  deriving (Show, Eq)

data CDeclaratorOptional
  = CDeclaratorOptionalEmpty
  | CDeclaratorOptional (PI CDeclarator)
  deriving (Show, Eq)

-- declarator without pointers
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

-- a pointer
-- * CTypeQualifierListOptional CPointerOptional
data CPointer =
  CPointer (PI CTypeQualifierListOptional) (PI CPointerOptional)
  deriving (Show, Eq)

data CPointerOptional
  = CPointerOptionalEmpty
  | CPointerOptional (PI CPointer)
  deriving (Show, Eq)

-- series of type qualifiers
-- CTypeQualifier CTypeQualifierListOptional
data CTypeQualifierList =
  CTypeQualifierList (PI CTypeQualifier) (PI CTypeQualifierListOptional)
  deriving (Show, Eq)

data CTypeQualifierListOptional
  = CTypeQualifierListOptionalEmpty
  | CTypeQualifierListOptional (PI CTypeQualifierList)
  deriving (Show, Eq)

-- list of function parameters with or without argument names
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

-- function parameter list without varargs specifier
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

-- series of identifiers
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

-- assign variable
data CInitializer
  -- CAssignmentExpression
  = CInitializerAssignment (PI CAssignmentExpression)
  -- { CInitializerList }
  -- { CInitializerList , }
  | CInitializerInitList (PI CInitializerList)
  deriving (Show, Eq)

-- list of initializers (for struct or array)
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

-- a type without variable name
-- CSpecifierQualifierList CAbstractDeclaratorOptional
data CTypeName =
  CTypeName (PI CSpecifierQualifierList) (PI CAbstractDeclaratorOptional)
  deriving (Show, Eq)

-- declarator without label
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

-- declarator without label and with no pointer
data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator ) CDirectAbstractDeclarator'
  = CDirectAbstractDeclaratorParen
      (PI CAbstractDeclarator)
      (PI CDirectAbstractDeclarator')
  -- [ CConstantExpressionOptional ] CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorIndexed
      (PI CConstantExpressionOptional)
      (PI CDirectAbstractDeclarator')
  -- ( CParameterTypeListOptional ) CDirectAbstractDeclarator'
  | CDirectAbstractDeclaratorParams
      (PI CParameterTypeListOptional)
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

-- name of a type
-- CIdentifier
newtype CTypedefName =
  CTypedefName (PI CIdentifier)
  deriving (Show, Eq)

-- any statement
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

-- statement and label (for switch case and goto)
data CLabeledStatement
  -- CIdentifier : CStatement
  = CLabeledStatementId (PI CIdentifier) (PI CStatement)
  -- case CConstantExpression : CStatement
  | CLabeledStatementCase (PI CConstantExpression) (PI CStatement)
  -- default : CStatement
  | CLabeledStatementDefault (PI CStatement)
  deriving (Show, Eq)

-- an expression
-- CExpressionOptional ;
newtype CExpressionStatement =
  CExpressionStatement (PI CExpressionOptional)
  deriving (Show, Eq)

-- series of statements.
-- appearently all declarations must be done in the beginning of the compound
-- statement. this could be bug in the code or me misunderstanding the spec
-- but perhaps this is something changed in the later versions of C
-- { CDeclarationListOptional CStatementListOptional }
data CCompoundStatement =
  CCompoundStatement (PI CDeclarationListOptional) (PI CStatementListOptional)
  deriving (Show, Eq)

-- series of statements
-- CStatement CStatementListOptional
data CStatementList =
  CStatementList (PI CStatement) (PI CStatementListOptional)
  deriving (Show, Eq)

data CStatementListOptional
  = CStatementListOptionalEmpty
  | CStatementListOptional (PI CStatementList)
  deriving (Show, Eq)

-- if or switch
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

-- while, do while or for
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

-- goto, continue, break and return
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

-- anything that returns a value (I think)
data CExpression'
  -- empty
  = CExpression'Empty
  -- , CAssignmentExpression CExpression'
  | CExpression' (PI CAssignmentExpression) (PI CExpression')
  deriving (Show, Eq)

-- optionally assign a value
data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExpressionConditional (PI CConditionalExpression)
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpression
      (PI CUnaryExpression)
      (PI CAssignmentOperator)
      (PI CAssignmentExpression)
  deriving (Show, Eq)

-- = or an operator combined with = (like +=)
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

-- optional ternary expression
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

-- (appearently) an expression where the value can be determined at compile time
-- CConditionalExpression
newtype CConstantExpression =
  CConstantExpression (PI CConditionalExpression)
  deriving (Show, Eq)

data CConstantExpressionOptional
  = CConstantExpressionOptionalEmpty
  | CConstantExpressionOptional (PI CConstantExpression)
  deriving (Show, Eq)

-- optional ||
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

-- optional &&
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

-- optional bitwise or
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

-- optional bitwise xor
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

-- optional bitwise and
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

-- optional equals or not equals
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

-- optional <, >, >= or <=
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

-- optional bit shift
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

-- optional + or -
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

-- optional *, / or %
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

-- optional cast to type
data CCastExpression
  -- CUnaryExpression
  = CCastExpressionUnary (PI CUnaryExpression)
  -- ( CTypeName ) CCastExpression
  | CCastExpression (PI CTypeName) (PI CCastExpression)
  deriving (Show, Eq)

-- optional unary operation
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

-- optional postfix unary expression
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

-- label, literal or expression inside parens
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

-- list of function arguments
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

-- a literal or enum value
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

