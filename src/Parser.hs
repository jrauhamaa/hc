module Parser where

import Lexeme (CLexeme(..))

type Identifier = String

-- TODO: eliminate left recursion

data CPrimaryExpression
  = CPrimaryExprIdentifier Identifier
  | CPrimaryExprConstant CConstant
  | CPrimaryExprStrLiteral String
  | CPrimaryExprParenthesized CExpression

data CPostfixExpression
  -- CPrimaryExpression
  = CPostfixExprEmpty CPrimaryExpression
  -- CPostfixExpression [ CExpression ]
  | CPostfixExprIndexed CPostfixExpression CExpression
  -- CPostfixExpression ( CArgumentExpressionList (optional) )
  | CPostfixExprArgList CPostfixExpression CArgumentExpressionList
  -- CPostfixExpression . identifier
  | CPostfixExprStructField CPostfixExpression Identifier
  -- CPostfixExpression -> identifier
  | CPostfixExprStructPointer CPostfixExpression Identifier
  -- CPostfixExpression ++
  | CPostfixExprIncrement CPostfixExpression
  -- CPostfixExpression --
  | CPostfixExprDecrement CPostfixExpression

data CArgumentExpressionList
  -- CAssignmentExpression
  = CArgExprListEmpty CAssignmentExpression
  -- CArgumentExpressionList . CAssignmentExpression
  | CArgExprListMulti CArgumentExpressionList CAssignmentExpression

data CUnaryExpression
  -- CPostfixExpression
  = CUnaryExprEmpty CPostfixExpression
  -- ++ CUnaryExpression
  | CUnaryExprIncrement
  -- -- CUnaryExpression
  | CUnaryExprDecrement
  -- CUnaryOperator CCastExpression
  | CUnaryExprCast CUnaryOperator CCastExpression
  -- sizeof CUnaryExpression
  | CUnaryExprSizeof CUnaryExpression
  -- sizeof ( CTypeName )
  | CUnaryExprSizeofType CTypeName

data CUnaryOperator
  = CUnaryOpAddress             -- &
  | CUnaryOpMultiply            -- *
  | CUnaryOpPlus                -- +
  | CUnaryOpMinus               -- -
  | CUnaryOpBitwiseNot          -- ~
  | CUnaryOpNot                 -- !

data CCastExpression
  -- CUnaryExpression
  = CCastExprEmpty CUnaryExpression
  -- ( CTypeName ) CCastExpression
  | CCastExprCast CTypeName CCastExpression

data CMultiplicativeExpression
  -- CCastExpression
  = CMultExprEmpty CCastExpression
  -- CMultiplicativeExpression * CCastExpression
  | CMultExprMultiplication CMultiplicativeExpression CCastExpression
  -- CMultiplicativeExpression / CCastExpression
  | CMultExprDivision CMultiplicativeExpression CCastExpression
  -- CMultiplicativeExpression % CCastExpression
  | CMultExprModulo CMultiplicativeExpression CCastExpression

data CAdditiveExpression
  -- CMultiplicativeExpression
  = CAddExprEmpty CMultiplicativeExpression
  -- CAdditiveExpression + CMultiplicativeExpression
  | CAddExprAddition CAdditiveExpression CMultiplicativeExpression
  -- CAdditiveExpression - CMultiplicativeExpression
  | CAddExprSubtraction CAdditiveExpression CMultiplicativeExpression

data CShiftExpression
  -- CAdditiveExpression
  = CShiftExprEmpty CAdditiveExpression
  -- CShiftExpression << CAdditiveExpression
  | CShiftExprLeft CShiftExpression CAdditiveExpression
  -- CShiftExpression >> CAdditiveExpression
  | CShiftExprRight CShiftExpression CAdditiveExpression

data CRelationalExpression
  -- CShiftExpression
  = CRelationalExprEmpty CShiftExpression
  -- CRelationalExpression < CShiftExpression
  | CRelationalExprLT CRelationalExpression CShiftExpression
  -- CRelationalExpression > CShiftExpression
  | CRelationalExprGT CRelationalExpression CShiftExpression
  -- CRelationalExpression <= CShiftExpression
  | CRelationalExprLTE CRelationalExpression CShiftExpression
  -- CRelationalExpression >= CShiftExpression
  | CRelationalExprGTE CRelationalExpression CShiftExpression

data CEqualityExpression
  -- CRelationalExpression
  = CEqExprEmpty CRelationalExpression
  -- CEqualityExpression == CRelationalExpression
  | CEqExprEquals CEqualityExpression CRelationalExpression
  -- CEqualityExpression != CRelationalExpression
  | CEqExprNotEquals CEqualityExpression CRelationalExpression

data CAndExpression
  -- CEqualityExpression
  = CAndExprEmpty CEqualityExpression
  -- CAndExpression & CEqualityExpression
  | CAndExpr CAndExpression CEqualityExpression

data CExclusiveOrExpression
  -- CAndExpression
  = CXorExprEmpty CAndExpression
  -- CExclusiveOrExpression ^ CAndExpression
  | CXorExpr CExclusiveOrExpression CAndExpression

data CInclusiveOrExpression
  -- CExclusiveOrExpression
  = COrExprEmpty CExclusiveOrExpression
  -- CInclusiveOrExpression | CExclusiveOrExpression
  | COrExpr CInclusiveOrExpression CExclusiveOrExpression

data CLogicalAndExpression
  -- CInclusiveOrExpression
  = CLogicalAndExprEmpty CInclusiveOrExpression
  -- CLogicalAndExpression && CInclusiveOrExpression
  | CLogicalAndExpr CLogicalAndExpression CInclusiveOrExpression

data CLogicalOrExpression
  -- CLogicalAndExpression
  = CLogicalOrExprEmpty CLogicalAndExpression
  -- CLogicalOrExpression || CLogicalAndExpression
  | CLogicalOrExpr CLogicalOrExpression CLogicalAndExpression

data CConditionalExpression
  -- CLogicalOrExpression
  = CConditionalExprEmpty CLogicalOrExpression
  -- CLogicalOrExpression ? CExpression : CConditionalExpression
  | CConditionalExpr CLogicalOrExpression CExpression CConditionalExpression

data CAssignmentExpression
  -- CConditionalExpression
  = CAssignmentExprEmpty CConditionalExpression
  -- CUnaryExpression CAssignmentOperator CAssignmentExpression
  | CAssignmentExpr CUnaryExpression CAssignmentOperator CAssignmentExpression

data CAssignmentOperator
  = CAssignOpAssign             -- =
  | CAssignOpMultiply           -- *=
  | CAssignOpDivide             -- /=
  | CAssignOpModulo             -- %=
  | CAssignOpAdd                -- +=
  | CAssignOpSubtract           -- -=
  | CAssignOpShiftLeft          -- <<=
  | CAssignOpShiftRight         -- >>=
  | CAssignOpAnd                -- &=
  | CAssignOpXor                -- ^=
  | CAssignOpOr                 -- |=

data CExpression
  -- CAssignmentExpression
  = CExprEmpty CAssignmentExpression
  -- CExpression , CAssignmentExpression
  | CExpr CExpression CAssignmentExpression

data CConstantExpression
  -- CConditionalExpression
  = CConstantExprEmpty CConditionalExpression

data CDeclaration
  -- CDeclarationSpecifiers CInitDeclaratorList (optional)
  = CDecl CDeclarationSpecifiers CInitDeclaratorList

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclSpecifiersStorageCLass CStorageClassSpecifier CDeclarationSpecifiers
  -- CTypeSpecifier CDeclarationSpecifiers (optional)
  | CDeclSpecifiersTypeSpecifier CTypeSpecifier CDeclarationSpecifiers
  -- CTypeQualifier CDeclarationSpecifiers (optional)
  | CDeclSpecifiersTypeQualifier CTypeQualifier CDeclarationSpecifiers

data CInitDeclaratorList
  -- CInitDeclarator
  = CInitDeclListSingleton CInitDeclarator
  -- CInitDeclaratorList , CInitDeclarator
  | CInitDeclList CInitDeclaratorList CInitDeclarator

data CInitDeclarator
  -- CDeclarator
  = CInitDeclEmpty CDeclarator
  -- CDeclarator = CInitializer
  | CInitDecl CDeclarator CInitializer

data CStorageClassSpecifier
  = CStorageClsSpecTypedef
  | CStorageClsSpecExtern       -- extern
  | CStorageClsSpecStatic       -- static
  | CStorageClsSpecAuto         -- auto
  | CStorageClsSpecRegister     -- register

data CTypeSpecifier =
  = CTypeSpecVoid               -- void
  | CTypeSpecChar               -- char
  | CTypeSpecShort              -- short
  | CTypeSpecInt                -- int
  | CTypeSpecLong               -- long
  | CTypeSpecFloat              -- float
  | CTypeSpecDouble             -- double
  | CTypeSpecSigned             -- signed
  | CTypeSpecUnsigned           -- unsigned
  | CTypeSpecStructOrUnion CStructOrUnionSpecifier
  | CTypeSpecEnum CEnumSpecifier
  | CTypeSpecTypedef CTypedefName

data CStructOrUnionSpecifier
  -- CStructOrUnion CIdentifier (optional) { CStructDeclarationList }
  = CStructOrUnionSpecList CStructOrUnion CIdentifier CStructDeclarationList
  -- CStructOrUnion CIdentifier
  | CStructOrUnionSpec CStructOrUnion CIdentifier

data CStructOrUnion
  = CStruct                     -- struct
  | CUnion                      -- union

data CStructDeclarationList     -- NOTE: different from CStructDeclaratorList
  -- CStructDeclaration
  = CStructDeclarationListSingleton CStructDeclaration
  -- CStructDeclarationList CStructDeclaration
  | CStructDeclarationList CStructDeclarationList CStructDeclaration

data CStructDeclaration         -- NOTE: different from CStructDeclarator
  = CStructDeclaration CSpecifierQualifierList CStructDeclarationList

data CSpecifierQualifierList
  -- CTypeSpecifier CSpecifierQualifierList (optional)
  = CSpecQualifierListSpec CTypeSpecifier CSpecQualifierListSpec
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecQualifierListSpec CTypeQualifier CSpecQualifierListSpec

data CStructDeclaratorList      -- NOTE: different from CStructDeclarationList
  -- CStructDeclarator
  = CStructDeclaratorListSingleton CStructDeclarator
  -- CStructDeclaratorList CStructDeclarator
  | CStructDeclaratorList CStructDeclaratorList CStructDeclarator

data CStructDeclarator          -- NOTE: different from CStructDeclaration
  -- CDeclarator
  = CStructDeclarator CDeclarator
  -- CDeclarator (optional) : CConstantExpression
  | CStructDeclaratorInit CDeclarator CConstantExpression
