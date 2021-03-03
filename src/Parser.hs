module Parser where

import Lexeme (CLexeme(..))

type Identifier = String

-- TODO: eliminate left recursion

data CIdentifier = CIdentifier String

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
  = CDeclaration CDeclarationSpecifiers CInitDeclaratorList

data CDeclarationSpecifiers
  -- CStorageClassSpecifier CDeclarationSpecifiers (optional)
  = CDeclarationSpecifiersStorageCLass CStorageClassSpecifier CDeclarationSpecifiers
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
  = CStorageClsSpecTypedef
  | CStorageClsSpecExtern       -- extern
  | CStorageClsSpecStatic       -- static
  | CStorageClsSpecAuto         -- auto
  | CStorageClsSpecRegister     -- register

data CTypeSpecifier
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
  = CSpecQualifierListSpecifier CTypeSpecifier CSpecifierQualifierList
  -- CTypeQualifier CSpecifierQualifierList (optional)
  | CSpecQualifierListQualifier CTypeQualifier CSpecifierQualifierList

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

data CEnumSpecifier
  -- enum CIdentifier (optional) { CEnumeratorList }
  = CEnumSpecList CIdentifier CEnumeratorList
  -- enum CIdentifier
  | CEnumSpec CIdentifier

data CEnumeratorList
  -- CEnumerator
  = CEnumListSingleton CEnumerator
  -- CEnumeratorList , CEnumerator
  | CEnumeratorList CEnumerator

data CEnumerator
  -- CEnumerationConstant
  = CEnumerator CEnumerationConstant
  -- CEnumerationConstant = CConstantExpression
  | CEnumeratorAssign CEnumerationConstant CConstantExpression

data CEnumerationConstant = CEnumerationConstant CIdentifier

data CTypeQualifier
  = CTypeQualifierConst         -- const
  | CTypeQualifierVolatile      -- volatile

data CDeclarator
  -- CPointer (optional) CDirectDeclarator
  = CDeclarator CPointer CDirectDeclarator

data CDirectDeclarator
  -- CIdentifier
  = CDirectDeclaratorId CIdentifier
  -- ( CDeclarator )
  | CDirectDeclaratorParenthesized CDeclarator
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
  = CParamTypeList CParameterList
  -- CParameterTypeList , ...
  | CParamTypeListVarargs CParameterList

data CParameterList
  -- CParameterDeclaration
  = CParamListSingleton CParameterDeclaration
  -- CParameterList CParameterDeclaration
  | CParamList CParameterList CParameterDeclaration

data CParameterDeclaration
  -- CParameterList , CParameterDeclaration
  = CParameterDeclaration CParameterList CParameterDeclaration

data CIdentifierList
  -- CIdentifier
  = CIdListSingleton CIdentifier
  -- CIdentifierList , CIdentifier
  | CidList CIdentifierList CIdentifier

data CTypeName
  -- CSpecifierQualifierList CAbstractDeclarator (optional)
  = CTypeName CSpecifierQualifierList CAbstractDeclarator

data CAbstractDeclarator
  -- CPointer
  = CAbstractDeclaratorPointer CPointer
  -- CPointer (optional) CDirectAbstractDeclarator
  | CAbstractDeclaratorDirect CPointer CDirectAbstractDeclarator

data CDirectAbstractDeclarator
  -- ( CAbstractDeclarator )
  = CDirectAbstractDeclaratorParenthesized CAbstractDeclarator
  -- CDirectAbstractDeclarator (optional) [ CConstantExpression (optional) ]
  | CDirectAbstractDeclaratorConstant CDirectAbstractDeclarator CConstantExpression
  -- CDirectAbstractDeclarator (optional) ( CParamTypeList (optional) )
  | CDirectAbstractDeclaratorParamList CDirectAbstractDeclarator CParameterTypeList

data CTypedefName = CTypedefName CIdentifier

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

data CCompoundStatement
  -- { CDeclarationList (optional) CStatementList (optional) }
  = CCompoundStatement CDeclarationList CStatementList

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

data CExpressionStatement
  -- CExpression (optional) ;
  = CExpressionStatement CExpression

data CSelectionStatement
  -- if ( CExpression ) CStatement
  = CSelectionStatementIf CExpression CStatement
  -- if ( CExpression ) CStatement else CStatement
  | CSelectionStatementIfElse CExpression CStatement CStatement
  -- switch ( CExpression ) CStatement
  | CSelectionStatementSwitch CExpression CStatement

data CIterationStatement
  -- while ( CExpression ) CStatement
  = CIterStatementWhile CExpression CStatement
  -- do CStatement while ( CExpression ) ;
  | CIterStatementDoWhile CStatement CExpression
  -- for ( CExpression (optional) ; CExpression (optional) ; CExpression (optional) ) CStatement
  | CIterStatementFor CExpression CExpression CExpression CStatement

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


