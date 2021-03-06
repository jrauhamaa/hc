{-# LANGUAGE LambdaCase #-}

module Parser where

import Lexeme (CLexeme(..))
import ParseElements
import Scanner (Coordinates, ScanElement)

-----------
-- TYPES --
-----------
newtype Parser a =
  Parser
    { runParser :: [ScanElement] -> Either ParseError ( [ScanElement]
                                                      , ParseResult a)
    }

data ParseError =
  ParseError
    { errorCoordinates :: Coordinates
    , errorMsg :: String
    }
  deriving (Eq, Show)

data ParseResult a =
  ParseResult
    { coordinates :: Coordinates
    , result :: a
    }
  deriving (Eq, Show)

instance Functor ParseResult where
  fmap fab (ParseResult c a) = ParseResult c $ fab a

instance Applicative ParseResult where
  pure x = ParseResult (0, 0) x
  (ParseResult _ fab) <*> (ParseResult c2 a) = ParseResult c2 $ fab a

instance Functor Parser where
  fmap fab pa = Parser $ \input -> (fmap . fmap . fmap) fab $ runParser pa input

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, ParseResult (fst $ head input) x)
  p1 <*> p2 =
    Parser $ \input -> do
      (input', pab) <- runParser p1 input
      (input'', pa) <- runParser p2 input'
      return (input'', pab <*> pa)

-----------
-- UTILS --
-----------
-- Try parsers in order. If both fail use error message of the one
-- that reached the furthest
parserUnion' :: Parser a -> Parser a -> Parser a
parserUnion' p1 p2 =
  Parser $ \input ->
    case runParser p1 input of
      r1@(Right _) -> r1
      e1@(Left (ParseError c1 _)) ->
        case runParser p2 input of
          r2@(Right _) -> r2
          e2@(Left (ParseError c2 _)) ->
            if c1 > c2
              then e1
              else e2

parserUnion :: [Parser a] -> Parser a
parserUnion = foldl parserUnion' failingP
  where
    failingP = Parser $ \_ -> Left $ ParseError (0, 0) ""

optionalP :: Parser a -> Parser (Maybe a)
optionalP p = parserUnion [Just <$> p, pure Nothing]

unexpectedEof :: ParseError
unexpectedEof = ParseError (maxBound, maxBound) "Unexpected EOF"

unexpectedLexeme :: Coordinates -> String -> String -> ParseError
unexpectedLexeme c expected encountered =
  ParseError c $
  mconcat ["Expected ", expected, ". Instead encountered ", encountered, "."]

------------------------
-- ELEMENTARY PARSERS --
------------------------
singleP :: CLexeme -> Parser CLexeme
singleP l =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, lexeme):rest) ->
      if lexeme == l
        then Right (rest, ParseResult c lexeme)
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: Parser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LIntLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: Parser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LFloatLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: Parser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LCharLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: Parser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LStringLiteral x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: Parser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((c, LLabel x):rest) -> Right (rest, ParseResult c x)
    ((c, l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l

---------------
-- C PARSERS --
---------------
cConstantP :: Parser CConstant
cConstantP =
  parserUnion
    [ CConstantInteger <$> intLiteralP
    , CConstantFloat <$> floatLiteralP
    , CConstantCharacter <$> charLiteralP
    , CConstantEnumeration <$> cEnumerationConstantP
    ]

cIdentifierP :: Parser CIdentifier
cIdentifierP = CIdentifier <$> labelP

cEnumerationConstantP :: Parser CEnumerationConstant
cEnumerationConstantP = CEnumerationConstant <$> cIdentifierP

cPrimaryExpressionP :: Parser CPrimaryExpression
cPrimaryExpressionP =
  parserUnion
    [ CPrimaryExpressionId <$> cIdentifierP
    , CPrimaryExpressionConst <$> cConstantP
    , CPrimaryExpressionStr <$> stringLiteralP
    , CPrimaryExpressionParen <$>
      (singleP LParenthesisOpen *> cExpressionP <* singleP LParenthesisClose)
    ]

cPostfixExpressionP :: Parser CPostfixExpression
cPostfixExpressionP =
  CPostfixExpression <$> cPrimaryExpressionP <*> cPostfixExpressionP'

cPostfixExpressionP' :: Parser CPostfixExpression'
cPostfixExpressionP' =
  parserUnion
    [ CPostfixExpression'Increment <$>
      (singleP LIncrement *> cPostfixExpressionP')
    , CPostfixExpression'Decrement <$>
      (singleP LDecrement *> cPostfixExpressionP')
    , CPostfixExpression'StructField <$> (singleP LDot *> cIdentifierP) <*>
      cPostfixExpressionP'
    , CPostfixExpression'StructPointer <$> (singleP LArrow *> cIdentifierP) <*>
      cPostfixExpressionP'
    , CPostfixExpression'ArgList <$>
      (singleP LParenthesisOpen *> optionalP cArgumentExpressionListP <*
       singleP LParenthesisClose) <*>
      cPostfixExpressionP'
    , CPostfixExpression'Indexed <$>
      (singleP LBracketOpen *> cExpressionP <* singleP LBracketClose) <*>
      cPostfixExpressionP'
    , pure CPostfixExpression'Empty
    ]

cArgumentExpressionListP :: Parser CArgumentExpressionList
cArgumentExpressionListP =
  CArgumentExpressionList <$> cAssignmentExpressionP <*>
  cArgumentExpressionListP'

cArgumentExpressionListP' :: Parser CArgumentExpressionList'
cArgumentExpressionListP' =
  parserUnion
    [ CArgumentExpressionList' <$> cAssignmentExpressionP <*>
      cArgumentExpressionListP'
    , pure CArgumentExpressionList'Empty
    ]

cUnaryExpressionP :: Parser CUnaryExpression
cUnaryExpressionP =
  parserUnion
    [ CUnaryExpressionSizeof <$> (singleP LSizeof *> cUnaryExpressionP)
    , CUnaryExpressionSizeofType <$>
      (singleP LSizeof *> singleP LParenthesisOpen *> cTypeNameP <*
       singleP LParenthesisClose)
    , CUnaryExpressionIncr <$> (singleP LIncrement *> cUnaryExpressionP)
    , CUnaryExpressionDecr <$> (singleP LDecrement *> cUnaryExpressionP)
    , CUnaryExpressionCast <$> cUnaryOperatorP <*> cCastExpressionP
    , CUnaryExpressionSingleton <$> cPostfixExpressionP
    ]

cUnaryOperatorP :: Parser CUnaryOperator
cUnaryOperatorP =
  parserUnion
    [ const CUnaryOperatorAddress <$> singleP LAmp
    , const CUnaryOperatorMultiply <$> singleP LStar
    , const CUnaryOperatorPlus <$> singleP LPlus
    , const CUnaryOperatorMinus <$> singleP LMinus
    , const CUnaryOperatorBitwiseNot <$> singleP LBitwiseNot
    , const CUnaryOperatorNot <$> singleP LNot
    ]

cCastExpressionP :: Parser CCastExpression
cCastExpressionP =
  parserUnion
    [ CCastExpression <$>
      (singleP LParenthesisOpen *> cTypeNameP <* singleP LParenthesisClose) <*>
      cCastExpressionP
    , CCastExpressionSingleton <$> cUnaryExpressionP
    ]

cMultiplicativeExpressionP :: Parser CMultiplicativeExpression
cMultiplicativeExpressionP =
  CMultiplicativeExpression <$> cCastExpressionP <*> cMultiplicativeExpressionP'

cMultiplicativeExpressionP' :: Parser CMultiplicativeExpression'
cMultiplicativeExpressionP' =
  parserUnion
    [ CMultiplicativeExpression'Mul <$> (singleP LStar *> cCastExpressionP) <*>
      cMultiplicativeExpressionP'
    , CMultiplicativeExpression'Div <$> (singleP LDivision *> cCastExpressionP) <*>
      cMultiplicativeExpressionP'
    , CMultiplicativeExpression'Mod <$> (singleP LModulo *> cCastExpressionP) <*>
      cMultiplicativeExpressionP'
    , pure CMultiplicativeExpression'Empty
    ]

cAdditiveExpressionP :: Parser CAdditiveExpression
cAdditiveExpressionP =
  CAdditiveExpression <$> cMultiplicativeExpressionP <*> cAdditiveExpressionP'

cAdditiveExpressionP' :: Parser CAdditiveExpression'
cAdditiveExpressionP' =
  parserUnion
    [ CAdditiveExpression'Plus <$> (singleP LPlus *> cMultiplicativeExpressionP) <*>
      cAdditiveExpressionP'
    , CAdditiveExpression'Minus <$>
      (singleP LMinus *> cMultiplicativeExpressionP) <*>
      cAdditiveExpressionP'
    , pure CAdditiveExpression'Empty
    ]

cShiftExpressionP :: Parser CShiftExpression
cShiftExpressionP =
  CShiftExpression <$> cAdditiveExpressionP <*> cShiftExpressionP'

cShiftExpressionP' :: Parser CShiftExpression'
cShiftExpressionP' =
  parserUnion
    [ CShiftExpression'Left <$> (singleP LBitShiftLeft *> cAdditiveExpressionP) <*>
      cShiftExpressionP'
    , CShiftExpression'Right <$>
      (singleP LBitShiftRight *> cAdditiveExpressionP) <*>
      cShiftExpressionP'
    , pure CShiftExpression'Empty
    ]

cRelationalExpressionP :: Parser CRelationalExpression
cRelationalExpressionP =
  CRelationalExpression <$> cShiftExpressionP <*> cRelationalExpressionP'

cRelationalExpressionP' :: Parser CRelationalExpression'
cRelationalExpressionP' =
  parserUnion
    [ CRelationalExpression'LT <$> (singleP LLT *> cShiftExpressionP) <*>
      cRelationalExpressionP'
    , CRelationalExpression'LTE <$> (singleP LLTE *> cShiftExpressionP) <*>
      cRelationalExpressionP'
    , CRelationalExpression'GT <$> (singleP LGT *> cShiftExpressionP) <*>
      cRelationalExpressionP'
    , CRelationalExpression'GTE <$> (singleP LGTE *> cShiftExpressionP) <*>
      cRelationalExpressionP'
    , pure CRelationalExpression'Empty
    ]

cEqualityExpressionP :: Parser CEqualityExpression
cEqualityExpressionP =
  CEqualityExpression <$> cRelationalExpressionP <*> cEqualityExpressionP'

cEqualityExpressionP' :: Parser CEqualityExpression'
cEqualityExpressionP' =
  parserUnion
    [ CEqualityExpression'EQ <$> (singleP LEquals *> cRelationalExpressionP) <*>
      cEqualityExpressionP'
    , CEqualityExpression'NEQ <$> (singleP LNotEquals *> cRelationalExpressionP) <*>
      cEqualityExpressionP'
    , pure CEqualityExpression'Empty
    ]

cAndExpressionP :: Parser CAndExpression
cAndExpressionP = CAndExpression <$> cEqualityExpressionP <*> cAndExpressionP'

cAndExpressionP' :: Parser CAndExpression'
cAndExpressionP' =
  parserUnion
    [ CAndExpression' <$> (singleP LAmp *> cEqualityExpressionP) <*>
      cAndExpressionP'
    , pure CAndExpression'Empty
    ]

cExclusiveOrExpressionP :: Parser CExclusiveOrExpression
cExclusiveOrExpressionP =
  CExclusiveOrExpression <$> cAndExpressionP <*> cExclusiveOrExpressionP'

cExclusiveOrExpressionP' :: Parser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parserUnion
    [ CExclusiveOrExpression' <$> (singleP LBitwiseXor *> cAndExpressionP) <*>
      cExclusiveOrExpressionP'
    , pure CExclusiveOrExpression'Empty
    ]

cInclusiveOrExpressionP :: Parser CInclusiveOrExpression
cInclusiveOrExpressionP =
  CInclusiveOrExpression <$> cExclusiveOrExpressionP <*>
  cInclusiveOrExpressionP'

cInclusiveOrExpressionP' :: Parser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parserUnion
    [ CInclusiveOrExpression' <$>
      (singleP LBitwiseOr *> cExclusiveOrExpressionP) <*>
      cInclusiveOrExpressionP'
    , pure CInclusiveOrExpression'Empty
    ]

cLogicalAndExpressionP :: Parser CLogicalAndExpression
cLogicalAndExpressionP =
  CLogicalAndExpression <$> cInclusiveOrExpressionP <*> cLogicalAndExpressionP'

cLogicalAndExpressionP' :: Parser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parserUnion
    [ CLogicalAndExpression' <$> (singleP LAnd *> cInclusiveOrExpressionP) <*>
      cLogicalAndExpressionP'
    , pure CLogicalAndExpression'Empty
    ]

cLogicalOrExpressionP :: Parser CLogicalOrExpression
cLogicalOrExpressionP =
  CLogicalOrExpression <$> cLogicalAndExpressionP <*> cLogicalOrExpressionP'

cLogicalOrExpressionP' :: Parser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parserUnion
    [ CLogicalOrExpression' <$> (singleP LOr *> cLogicalAndExpressionP) <*>
      cLogicalOrExpressionP'
    , pure CLogicalOrExpression'Empty
    ]

cConditionalExpressionP :: Parser CConditionalExpression
cConditionalExpressionP =
  parserUnion
    [ CConditionalExpression <$> cLogicalOrExpressionP <*>
      (singleP LTernary *> cExpressionP <* singleP LColon) <*>
      cConditionalExpressionP
    , CConditionalExpressionSingleton <$> cLogicalOrExpressionP
    ]

cAssignmentExpressionP :: Parser CAssignmentExpression
cAssignmentExpressionP =
  parserUnion
    [ CAssignmentExpression <$> cUnaryExpressionP <*> cAssignmentOperatorP <*>
      cAssignmentExpressionP
    , CAssignmentExpressionSingleton <$> cConditionalExpressionP
    ]

cAssignmentOperatorP :: Parser CAssignmentOperator
cAssignmentOperatorP =
  parserUnion
    [ const CAssignmentOperatorAssign <$> singleP LAssign
    , const CAssignmentOperatorMul <$> singleP LMultiplicationAssign
    , const CAssignmentOperatorDiv <$> singleP LDivisionAssign
    , const CAssignmentOperatorMod <$> singleP LModuloAssign
    , const CAssignmentOperatorAdd <$> singleP LPlusAssign
    , const CAssignmentOperatorSub <$> singleP LMinusAssign
    , const CAssignmentOperatorLShift <$> singleP LBitShiftLeftAssign
    , const CAssignmentOperatorRShfit <$> singleP LBitShiftRightAssign
    , const CAssignmentOperatorAnd <$> singleP LBitwiseAndAssign
    , const CAssignmentOperatorXor <$> singleP LBitwiseXorAssign
    , const CAssignmentOperatorOr <$> singleP LBitwiseOrAssign
    ]

cExpressionP :: Parser CExpression
cExpressionP = CExpression <$> cAssignmentExpressionP <*> cExpressionP'

cExpressionP' :: Parser CExpression'
cExpressionP' =
  CExpression' <$> (singleP LComma *> cAssignmentExpressionP) <*> cExpressionP'

cConstantExpressionP :: Parser CConstantExpression
cConstantExpressionP = CConstantExpression <$> cConditionalExpressionP

cDeclarationP :: Parser CDeclaration
cDeclarationP =
  CDeclaration <$> cDeclarationSpecifiersP <*> optionalP cInitDeclaratorListP

cDeclarationSpecifiersP :: Parser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserUnion
    [ CDeclarationSpecifiersStorageClass <$> cStorageClassSpecifierP <*>
      optionalP cDeclarationSpecifiersP
    , CDeclarationSpecifiersTypeSpecifier <$> cTypeSpecifierP <*>
      optionalP cDeclarationSpecifiersP
    , CDeclarationSpecifiersTypeQualifier <$> cTypeQualifierP <*>
      optionalP cDeclarationSpecifiersP
    ]

cInitDeclaratorListP :: Parser CInitDeclaratorList
cInitDeclaratorListP =
  CInitDeclaratorList <$> cInitDeclaratorP <*> cInitDeclaratorListP'

cInitDeclaratorListP' :: Parser CInitDeclaratorList'
cInitDeclaratorListP' =
  parserUnion
    [ CInitDeclaratorList' <$> (singleP LComma *> cInitDeclaratorP) <*>
      cInitDeclaratorListP'
    , pure CInitDeclaratorList'Empty
    ]

cInitDeclaratorP :: Parser CInitDeclarator
cInitDeclaratorP =
  parserUnion
    [ CInitDeclarator <$> cDeclaratorP <*> (singleP LAssign *> cInitializerP)
    , CInitDeclaratorSingleton <$> cDeclaratorP
    ]

cStorageClassSpecifierP :: Parser CStorageClassSpecifier
cStorageClassSpecifierP =
  parserUnion
    [ const CStorageClassSpecifierTypedef <$> singleP LTypedef
    , const CStorageClassSpecifierExtern <$> singleP LExtern
    , const CStorageClassSpecifierStatic <$> singleP LStatic
    , const CStorageClassSpecifierAuto <$> singleP LAuto
    , const CStorageClassSpecifierRegister <$> singleP LRegister
    ]

cTypeSpecifierP :: Parser CTypeSpecifier
cTypeSpecifierP =
  parserUnion
    [ const CTypeSpecifierVoid <$> singleP LVoid
    , const CTypeSpecifierChar <$> singleP LChar
    , const CTypeSpecifierShort <$> singleP LShort
    , const CTypeSpecifierInt <$> singleP LInt
    , const CTypeSpecifierLong <$> singleP LLong
    , const CTypeSpecifierFloat <$> singleP LFloat
    , const CTypeSpecifierDouble <$> singleP LDouble
    , const CTypeSpecifierSigned <$> singleP LSigned
    , const CTypeSpecifierUnsigned <$> singleP LUnsigned
    , CTypeSpecifierStructOrUnion <$> cStructOrUnionSpecifierP
    , CTypeSpecifierEnum <$> cEnumSpecifierP
    , CTypeSpecifierTypedef <$> cTypedefNameP
    ]

cStructOrUnionSpecifierP :: Parser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserUnion
    [ CStructOrUnionSpecifierList <$> cStructOrUnionP <*> optionalP cIdentifierP <*>
      (singleP LBraceOpen *> cStructDeclarationListP <* singleP LBraceClose)
    , CStructOrUnionSpecifier <$> cStructOrUnionP <*> cIdentifierP
    ]

cStructOrUnionP :: Parser CStructOrUnion
cStructOrUnionP =
  parserUnion
    [const CStruct <$> singleP LStruct, const CUnion <$> singleP LUnion]

cStructDeclarationListP :: Parser CStructDeclarationList
cStructDeclarationListP =
  parserUnion
    [ CStructDeclarationList <$> cStructDeclarationP <*> cStructDeclarationListP
    , CStructDeclarationListSingleton <$> cStructDeclarationP
    ]

cStructDeclarationP :: Parser CStructDeclaration
cStructDeclarationP =
  CStructDeclaration <$> cSpecifierQualifierListP <*>
  (cStructDeclaratorListP <* singleP LSemiColon)

cSpecifierQualifierListP :: Parser CSpecifierQualifierList
cSpecifierQualifierListP =
  parserUnion
    [ CSpecifierQualifierListSpecifier <$> cTypeSpecifierP <*>
      optionalP cSpecifierQualifierListP
    , CSpecifierQualifierListQualifier <$> cTypeQualifierP <*>
      optionalP cSpecifierQualifierListP
    ]

cStructDeclaratorListP :: Parser CStructDeclaratorList
cStructDeclaratorListP =
  CStructDeclaratorList <$> cStructDeclaratorP <*> cStructDeclaratorListP'

cStructDeclaratorListP' :: Parser CStructDeclaratorList'
cStructDeclaratorListP' =
  parserUnion
    [ CStructDeclaratorList' <$> (singleP LComma *> cStructDeclaratorP) <*>
      cStructDeclaratorListP'
    , pure CStructDeclaratorList'Empty
    ]

cStructDeclaratorP :: Parser CStructDeclarator
cStructDeclaratorP =
  parserUnion
    [ CStructDeclaratorInit <$> optionalP cDeclaratorP <*>
      (singleP LColon *> cConstantExpressionP)
    , CStructDeclarator <$> cDeclaratorP
    ]

cEnumSpecifierP :: Parser CEnumSpecifier
cEnumSpecifierP =
  parserUnion
    [ CEnumSpecifierList <$> (singleP LEnum *> optionalP cIdentifierP) <*>
      (singleP LBraceOpen *> cEnumeratorListP <* singleP LBraceClose)
    , CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)
    ]

cEnumeratorListP :: Parser CEnumeratorList
cEnumeratorListP = CEnumeratorList <$> cEnumeratorP <*> cEnumeratorListP'

cEnumeratorListP' :: Parser CEnumeratorList'
cEnumeratorListP' =
  parserUnion
    [ CEnumeratorList' <$> (singleP LComma *> cEnumeratorP) <*>
      cEnumeratorListP'
    , pure CEnumeratorList'Empty
    ]

cEnumeratorP :: Parser CEnumerator
cEnumeratorP =
  parserUnion
    [ CEnumeratorAssign <$> cEnumerationConstantP <*>
      (singleP LAssign *> cConstantExpressionP)
    , CEnumerator <$> cEnumerationConstantP
    ]

cTypeQualifierP :: Parser CTypeQualifier
cTypeQualifierP =
  parserUnion
    [ const CTypeQualifierConst <$> singleP LConst
    , const CTypeQualifierVolatile <$> singleP LVolatile
    ]

cDeclaratorP :: Parser CDeclarator
cDeclaratorP = CDeclarator <$> optionalP cPointerP <*> cDirectDeclaratorP

cDirectDeclaratorP :: Parser CDirectDeclarator
cDirectDeclaratorP =
  parserUnion
    [ CDirectDeclaratorParen <$>
      (singleP LParenthesisOpen *> cDeclaratorP <* singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclaratorId <$> cIdentifierP <*> cDirectDeclaratorP'
    ]

cDirectDeclaratorP' :: Parser CDirectDeclarator'
cDirectDeclaratorP' =
  parserUnion
    [ CDirectDeclarator'Indexed <$>
      (singleP LBracketOpen *> optionalP cConstantExpressionP <*
       singleP LBracketClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclarator'ParamTypeList <$>
      (singleP LParenthesisOpen *> cParameterTypeListP <*
       singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclarator'IdList <$>
      (singleP LParenthesisOpen *> optionalP cIdentifierListP <*
       singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , pure CDirectDeclarator'Empty
    ]

cPointerP :: Parser CPointer
cPointerP =
  parserUnion
    [ CPointerMulti <$> (singleP LStar *> optionalP cTypeQualifierListP) <*>
      cPointerP
    , CPointerSingle <$> (singleP LStar *> optionalP cTypeQualifierListP)
    ]

cTypeQualifierListP :: Parser CTypeQualifierList
cTypeQualifierListP =
  parserUnion
    [ CTypeQualifierList <$> cTypeQualifierP <*> cTypeQualifierListP
    , CTypeQualifierListSingleton <$> cTypeQualifierP
    ]

cParameterTypeListP :: Parser CParameterTypeList
cParameterTypeListP =
  parserUnion
    [ CParameterTypeListVarargs <$>
      (cParameterListP <* singleP LComma <* singleP LVarargs)
    , CParameterTypeList <$> cParameterListP
    ]

cParameterListP :: Parser CParameterList
cParameterListP = CParameterList <$> cParameterDeclarationP <*> cParameterListP'

cParameterListP' :: Parser CParameterList'
cParameterListP' =
  parserUnion
    [ CParameterList' <$> (singleP LComma *> cParameterDeclarationP) <*>
      cParameterListP'
    , pure CParameterList'Empty
    ]

cParameterDeclarationP :: Parser CParameterDeclaration
cParameterDeclarationP =
  parserUnion
    [ CParameterDeclaration <$> cDeclarationSpecifiersP <*> cDeclaratorP
    , CParameterDeclarationAbstract <$> cDeclarationSpecifiersP <*>
      optionalP cAbstractDeclaratorP
    ]

cIdentifierListP :: Parser CIdentifierList
cIdentifierListP = CIdentifierList <$> cIdentifierP <*> cIdentifierListP'

cIdentifierListP' :: Parser CIdentifierList'
cIdentifierListP' =
  parserUnion
    [ CIdentifierList' <$> (singleP LComma *> cIdentifierP) <*>
      cIdentifierListP'
    , pure CIdentifierList'Empty
    ]

cTypeNameP :: Parser CTypeName
cTypeNameP =
  CTypeName <$> cSpecifierQualifierListP <*> optionalP cAbstractDeclaratorP

cAbstractDeclaratorP :: Parser CAbstractDeclarator
cAbstractDeclaratorP =
  parserUnion
    [ CAbstractDeclaratorDirect <$> optionalP cPointerP <*>
      cDirectAbstractDeclaratorP
    , CAbstractDeclaratorPointer <$> cPointerP
    ]

-- TODO: Figure out how to implement this
cDirectAbstractDeclaratorP :: Parser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP = undefined

cTypedefNameP :: Parser CTypedefName
cTypedefNameP = CTypedefName <$> cIdentifierP

cInitializerP :: Parser CInitializer
cInitializerP =
  parserUnion
    [ CInitializerBracketListComma <$>
      (singleP LBraceOpen *> cInitializerListP <* singleP LComma <*
       singleP LBraceClose)
    , CInitializerBracketList <$>
      (singleP LBraceOpen *> cInitializerListP <* singleP LBraceClose)
    , CInitializerAssignment <$> cAssignmentExpressionP
    ]

cInitializerListP :: Parser CInitializerList
cInitializerListP = CInitializerList <$> cInitializerP <*> cInitializerListP'

cInitializerListP' :: Parser CInitializerList'
cInitializerListP' =
  parserUnion
    [ CInitializerList' <$> (singleP LComma *> cInitializerP) <*>
      cInitializerListP'
    , pure CInitializerList'Empty
    ]

cStatementP :: Parser CStatement
cStatementP =
  parserUnion
    [ CStatementLabeled <$> cLabeledStatementP
    , CStatementCompound <$> cCompoundStatementP
    , CStatementExpression <$> cExpressionStatementP
    , CStatementSelection <$> cSelectionStatementP
    , CStatementIteration <$> cIterationStatementP
    , CStatementJump <$> cJumpStatementP
    ]

cLabeledStatementP :: Parser CLabeledStatement
cLabeledStatementP =
  parserUnion
    [ CLabeledStatementDefault <$>
      (singleP LDefault *> singleP LColon *> cStatementP)
    , CLabeledStatementCase <$>
      (singleP LCase *> cConstantExpressionP <* singleP LColon) <*>
      cStatementP
    , CLabeledStatementId <$> cIdentifierP <*> (singleP LColon *> cStatementP)
    ]

cCompoundStatementP :: Parser CCompoundStatement
cCompoundStatementP =
  CCompoundStatement <$> (singleP LBraceOpen *> optionalP cDeclarationListP) <*>
  (optionalP cStatementListP <* singleP LBraceClose)

cDeclarationListP :: Parser CDeclarationList
cDeclarationListP =
  parserUnion
    [ CDeclarationList <$> cDeclarationP <*> cDeclarationListP
    , CDeclarationListSingleton <$> cDeclarationP
    ]

cStatementListP :: Parser CStatementList
cStatementListP =
  parserUnion
    [ CStatementList <$> cStatementP <*> cStatementListP
    , CStatementListSingleton <$> cStatementP
    ]

cExpressionStatementP :: Parser CExpressionStatement
cExpressionStatementP =
  CExpressionStatement <$> (optionalP cExpressionP <* singleP LSemiColon)

cSelectionStatementP :: Parser CSelectionStatement
cSelectionStatementP =
  parserUnion
    [ CSelectionStatementSwitch <$>
      (singleP LSwitch *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose) <*>
      cStatementP
    , CSelectionStatementIfElse <$>
      (singleP LIf *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose) <*>
      cStatementP <*>
      (singleP LElse *> cStatementP)
    , CSelectionStatementIf <$>
      (singleP LIf *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose) <*>
      cStatementP
    ]

cIterationStatementP :: Parser CIterationStatement
cIterationStatementP =
  parserUnion
    [ CIterationStatementWhile <$>
      (singleP LWhile *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose) <*>
      cStatementP
    , CIterationStatementDoWhile <$> (singleP LDo *> cStatementP) <*>
      (singleP LWhile *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose <*
       singleP LSemiColon)
    , CIterationStatementFor <$>
      (singleP LFor *> singleP LParenthesisOpen *> optionalP cExpressionP) <*>
      (singleP LSemiColon *> optionalP cExpressionP) <*>
      (singleP LSemiColon *> optionalP cExpressionP <* singleP LParenthesisClose) <*>
      cStatementP
    ]

cJumpStatementP :: Parser CJumpStatement
cJumpStatementP =
  parserUnion
    [ CJumpStatementGoto <$>
      (singleP LGoto *> cIdentifierP <* singleP LSemiColon)
    , const CJumpStatementContinue <$> (singleP LContinue <* singleP LSemiColon)
    , const CJumpStatementBreak <$> (singleP LBreak <* singleP LSemiColon)
    , CJumpStatementReturn <$>
      (singleP LReturn *> optionalP cExpressionP <* singleP LSemiColon)
    ]

cTranslationUnitP :: Parser CTranslationUnit
cTranslationUnitP =
  parserUnion
    [ CTranslationUnitTranslation <$> cExternalDeclarationP <*>
      cTranslationUnitP
    , CTranslationUnitExternal <$> cExternalDeclarationP
    ]

cExternalDeclarationP :: Parser CExternalDeclaration
cExternalDeclarationP =
  parserUnion
    [ CExternalDeclarationFunction <$> cFunctionDefinitionP
    , CExternalDeclaration <$> cDeclarationP
    ]

cFunctionDefinitionP :: Parser CFunctionDefinition
cFunctionDefinitionP =
  parserUnion
    [ CFunctionDefinitionSpecifiers <$> optionalP cDeclarationSpecifiersP <*>
      cDeclaratorP
    , CFunctionDefinitionList <$> optionalP cDeclarationListP <*>
      cCompoundStatementP
    ]
