{-# LANGUAGE LambdaCase #-}

module Parser where

import Lexeme (CLexeme(..))
import ParseElements
import Scanner (Coordinates, ScanElement(..))

-----------
-- TYPES --
-----------

type Input = [ScanElement CLexeme]

newtype Parser a =
  Parser
    { runParser :: Input -> Either ParseError (Input, a)
    }

type PEParser a = Parser (ParseElement a)

data ParseError =
  ParseError
    { errorCoordinates :: Coordinates
    , errorMsg :: String
    }
  deriving (Eq, Show)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  p1 <*> p2 =
    Parser $ \input -> do
      (input', ab) <- runParser p1 input
      (input'', a) <- runParser p2 input'
      return (input'', ab a)

-- CTranslationUnit is the root element in the grammar
cParser :: PEParser CTranslationUnit
cParser =
  Parser $ \input ->
    case runParser cTranslationUnitP input of
      e@(Left _) -> e
      r@(Right ([], _)) -> r
      r@(Right (notParsed:_, _)) -> r
      --  Left $
      --  ParseError (fst $ notParsed) "Parsing terminated prematurely"

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

-- transform parsers with parseElementP before union
parseElementUnion :: [Parser a] -> Parser (ParseElement a)
parseElementUnion = parserUnion . map parseElementP

getCoords :: [ScanElement CLexeme] -> Coordinates
getCoords [] = (0, 0) -- we should never get here
getCoords elems = scanLoc $ head elems

-- wrap result of parser in ParseElement
parseElementP :: Parser a -> PEParser a
parseElementP p =
  Parser $ \input ->
    runParser (ParseElement (getCoords input) <$> p) input

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
    ((ScanElement c lexeme):rest) ->
      if lexeme == l
        then Right (rest, l)
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: PEParser Int
intLiteralP =
  parseElementP $
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement _ (LIntLiteral x)):rest) -> Right (rest, x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: PEParser Double
floatLiteralP =
  parseElementP $
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement _ (LFloatLiteral x)):rest) -> Right (rest, x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: PEParser Char
charLiteralP =
  parseElementP $
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement _ (LCharLiteral x)):rest) -> Right (rest, x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: PEParser String
stringLiteralP =
  parseElementP $
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement _ (LStringLiteral x)):rest) -> Right (rest, x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: PEParser String
labelP =
  parseElementP $
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement _ (LLabel x)):rest) -> Right (rest, x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l

---------------
-- C PARSERS --
---------------
cConstantP :: PEParser CConstant
cConstantP =
  parseElementUnion
    [ CConstantInteger <$> intLiteralP
    , CConstantFloat <$> floatLiteralP
    , CConstantCharacter <$> charLiteralP
    , CConstantEnumeration <$> cEnumerationConstantP
    ]

cIdentifierP :: PEParser CIdentifier
cIdentifierP = parseElementP $ CIdentifier <$> labelP

cEnumerationConstantP :: PEParser CEnumerationConstant
cEnumerationConstantP = parseElementP $ CEnumerationConstant <$> cIdentifierP

cPrimaryExpressionP :: PEParser CPrimaryExpression
cPrimaryExpressionP =
  parseElementUnion
    [ CPrimaryExpressionId <$> cIdentifierP
    , CPrimaryExpressionConst <$> cConstantP
    , CPrimaryExpressionStr <$> stringLiteralP
    , CPrimaryExpressionParen <$>
      (singleP LParenthesisOpen *> cExpressionP <* singleP LParenthesisClose)
    ]

cPostfixExpressionP :: PEParser CPostfixExpression
cPostfixExpressionP =
  parseElementP $
  CPostfixExpression <$> cPrimaryExpressionP <*> cPostfixExpressionP'

cPostfixExpressionP' :: PEParser CPostfixExpression'
cPostfixExpressionP' =
  parseElementUnion
    [ CPostfixExpression'Increment <$>
      (singleP LIncrement *> cPostfixExpressionP')
    , CPostfixExpression'Decrement <$>
      (singleP LDecrement *> cPostfixExpressionP')
    , CPostfixExpression'StructField <$> (singleP LDot *> cIdentifierP) <*>
      cPostfixExpressionP'
    , CPostfixExpression'StructPointer <$> (singleP LArrow *> cIdentifierP) <*>
      cPostfixExpressionP'
    , CPostfixExpression'ArgList <$>
      (singleP LParenthesisOpen *> cArgumentExpressionListOptionalP <*
       singleP LParenthesisClose) <*>
      cPostfixExpressionP'
    , CPostfixExpression'Indexed <$>
      (singleP LBracketOpen *> cExpressionP <* singleP LBracketClose) <*>
      cPostfixExpressionP'
    , pure CPostfixExpression'Empty
    ]

cArgumentExpressionListP :: PEParser CArgumentExpressionList
cArgumentExpressionListP =
  parseElementP $
  CArgumentExpressionList <$> cAssignmentExpressionP <*>
  cArgumentExpressionListP'

cArgumentExpressionListP' :: PEParser CArgumentExpressionList'
cArgumentExpressionListP' =
  parseElementUnion
    [ CArgumentExpressionList' <$> cAssignmentExpressionP <*>
      cArgumentExpressionListP'
    , pure CArgumentExpressionList'Empty
    ]

cUnaryExpressionP :: PEParser CUnaryExpression
cUnaryExpressionP =
  parseElementUnion
    [ CUnaryExpressionSizeof <$> (singleP LSizeof *> cUnaryExpressionP)
    , CUnaryExpressionSizeofType <$>
      (singleP LSizeof *> singleP LParenthesisOpen *> cTypeNameP <*
       singleP LParenthesisClose)
    , CUnaryExpressionIncr <$> (singleP LIncrement *> cUnaryExpressionP)
    , CUnaryExpressionDecr <$> (singleP LDecrement *> cUnaryExpressionP)
    , CUnaryExpressionCast <$> cUnaryOperatorP <*> cCastExpressionP
    , CUnaryExpressionSingleton <$> cPostfixExpressionP
    ]

cUnaryOperatorP :: PEParser CUnaryOperator
cUnaryOperatorP =
  parseElementUnion
    [ CUnaryOperatorAddress <$ singleP LAmp
    , CUnaryOperatorMultiply <$ singleP LStar
    , CUnaryOperatorPlus <$ singleP LPlus
    , CUnaryOperatorMinus <$ singleP LMinus
    , CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot
    , CUnaryOperatorNot <$ singleP LNot
    ]

cCastExpressionP :: PEParser CCastExpression
cCastExpressionP =
  parseElementUnion
    [ CCastExpression <$>
      (singleP LParenthesisOpen *> cTypeNameP <* singleP LParenthesisClose) <*>
      cCastExpressionP
    , CCastExpressionSingleton <$> cUnaryExpressionP
    ]

cMultiplicativeExpressionP :: PEParser CMultiplicativeExpression
cMultiplicativeExpressionP =
  parseElementP $
  CMultiplicativeExpression <$> cCastExpressionP <*> cMultiplicativeExpressionP'

cMultiplicativeExpressionP' :: PEParser CMultiplicativeExpression'
cMultiplicativeExpressionP' =
  parseElementUnion
    [ CMultiplicativeExpression'Mul <$> (singleP LStar *> cCastExpressionP) <*>
      cMultiplicativeExpressionP'
    , CMultiplicativeExpression'Div <$> (singleP LDivision *> cCastExpressionP) <*>
      cMultiplicativeExpressionP'
    , CMultiplicativeExpression'Mod <$> (singleP LModulo *> cCastExpressionP) <*>
      cMultiplicativeExpressionP'
    , pure CMultiplicativeExpression'Empty
    ]

cAdditiveExpressionP :: PEParser CAdditiveExpression
cAdditiveExpressionP =
  parseElementP $
  CAdditiveExpression <$> cMultiplicativeExpressionP <*> cAdditiveExpressionP'

cAdditiveExpressionP' :: PEParser CAdditiveExpression'
cAdditiveExpressionP' =
  parseElementUnion
    [ CAdditiveExpression'Plus <$> (singleP LPlus *> cMultiplicativeExpressionP) <*>
      cAdditiveExpressionP'
    , CAdditiveExpression'Minus <$>
      (singleP LMinus *> cMultiplicativeExpressionP) <*>
      cAdditiveExpressionP'
    , pure CAdditiveExpression'Empty
    ]

cShiftExpressionP :: PEParser CShiftExpression
cShiftExpressionP =
  parseElementP $
  CShiftExpression <$> cAdditiveExpressionP <*> cShiftExpressionP'

cShiftExpressionP' :: PEParser CShiftExpression'
cShiftExpressionP' =
  parseElementUnion
    [ CShiftExpression'Left <$> (singleP LBitShiftLeft *> cAdditiveExpressionP) <*>
      cShiftExpressionP'
    , CShiftExpression'Right <$>
      (singleP LBitShiftRight *> cAdditiveExpressionP) <*>
      cShiftExpressionP'
    , pure CShiftExpression'Empty
    ]

cRelationalExpressionP :: PEParser CRelationalExpression
cRelationalExpressionP =
  parseElementP $
  CRelationalExpression <$> cShiftExpressionP <*> cRelationalExpressionP'

cRelationalExpressionP' :: PEParser CRelationalExpression'
cRelationalExpressionP' =
  parseElementUnion
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

cEqualityExpressionP :: PEParser CEqualityExpression
cEqualityExpressionP =
  parseElementP $
  CEqualityExpression <$> cRelationalExpressionP <*> cEqualityExpressionP'

cEqualityExpressionP' :: PEParser CEqualityExpression'
cEqualityExpressionP' =
  parseElementUnion
    [ CEqualityExpression'EQ <$> (singleP LEquals *> cRelationalExpressionP) <*>
      cEqualityExpressionP'
    , CEqualityExpression'NEQ <$> (singleP LNotEquals *> cRelationalExpressionP) <*>
      cEqualityExpressionP'
    , pure CEqualityExpression'Empty
    ]

cAndExpressionP :: PEParser CAndExpression
cAndExpressionP =
  parseElementP $ CAndExpression <$> cEqualityExpressionP <*> cAndExpressionP'

cAndExpressionP' :: PEParser CAndExpression'
cAndExpressionP' =
  parseElementUnion
    [ CAndExpression' <$> (singleP LAmp *> cEqualityExpressionP) <*>
      cAndExpressionP'
    , pure CAndExpression'Empty
    ]

cExclusiveOrExpressionP :: PEParser CExclusiveOrExpression
cExclusiveOrExpressionP =
  parseElementP $
  CExclusiveOrExpression <$> cAndExpressionP <*> cExclusiveOrExpressionP'

cExclusiveOrExpressionP' :: PEParser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parseElementUnion
    [ CExclusiveOrExpression' <$> (singleP LBitwiseXor *> cAndExpressionP) <*>
      cExclusiveOrExpressionP'
    , pure CExclusiveOrExpression'Empty
    ]

cInclusiveOrExpressionP :: PEParser CInclusiveOrExpression
cInclusiveOrExpressionP =
  parseElementP $
  CInclusiveOrExpression <$> cExclusiveOrExpressionP <*>
  cInclusiveOrExpressionP'

cInclusiveOrExpressionP' :: PEParser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parseElementUnion
    [ CInclusiveOrExpression' <$>
      (singleP LBitwiseOr *> cExclusiveOrExpressionP) <*>
      cInclusiveOrExpressionP'
    , pure CInclusiveOrExpression'Empty
    ]

cLogicalAndExpressionP :: PEParser CLogicalAndExpression
cLogicalAndExpressionP =
  parseElementP $
  CLogicalAndExpression <$> cInclusiveOrExpressionP <*> cLogicalAndExpressionP'

cLogicalAndExpressionP' :: PEParser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parseElementUnion
    [ CLogicalAndExpression' <$> (singleP LAnd *> cInclusiveOrExpressionP) <*>
      cLogicalAndExpressionP'
    , pure CLogicalAndExpression'Empty
    ]

cLogicalOrExpressionP :: PEParser CLogicalOrExpression
cLogicalOrExpressionP =
  parseElementP $
  CLogicalOrExpression <$> cLogicalAndExpressionP <*> cLogicalOrExpressionP'

cLogicalOrExpressionP' :: PEParser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parseElementUnion
    [ CLogicalOrExpression' <$> (singleP LOr *> cLogicalAndExpressionP) <*>
      cLogicalOrExpressionP'
    , pure CLogicalOrExpression'Empty
    ]

cConditionalExpressionP :: PEParser CConditionalExpression
cConditionalExpressionP =
  parseElementUnion
    [ CConditionalExpression <$> cLogicalOrExpressionP <*>
      (singleP LTernary *> cExpressionP <* singleP LColon) <*>
      cConditionalExpressionP
    , CConditionalExpressionSingleton <$> cLogicalOrExpressionP
    ]

cAssignmentExpressionP :: PEParser CAssignmentExpression
cAssignmentExpressionP =
  parseElementUnion
    [ CAssignmentExpression <$> cUnaryExpressionP <*> cAssignmentOperatorP <*>
      cAssignmentExpressionP
    , CAssignmentExpressionSingleton <$> cConditionalExpressionP
    ]

cAssignmentOperatorP :: PEParser CAssignmentOperator
cAssignmentOperatorP =
  parseElementUnion
    [ CAssignmentOperatorAssign <$ singleP LAssign
    , CAssignmentOperatorMul <$ singleP LMultiplicationAssign
    , CAssignmentOperatorDiv <$ singleP LDivisionAssign
    , CAssignmentOperatorMod <$ singleP LModuloAssign
    , CAssignmentOperatorAdd <$ singleP LPlusAssign
    , CAssignmentOperatorSub <$ singleP LMinusAssign
    , CAssignmentOperatorLShift <$ singleP LBitShiftLeftAssign
    , CAssignmentOperatorRShfit <$ singleP LBitShiftRightAssign
    , CAssignmentOperatorAnd <$ singleP LBitwiseAndAssign
    , CAssignmentOperatorXor <$ singleP LBitwiseXorAssign
    , CAssignmentOperatorOr <$ singleP LBitwiseOrAssign
    ]

cExpressionP :: PEParser CExpression
cExpressionP =
  parseElementP $ CExpression <$> cAssignmentExpressionP <*> cExpressionP'

cExpressionP' :: PEParser CExpression'
cExpressionP' =
  parseElementP $
  CExpression' <$> (singleP LComma *> cAssignmentExpressionP) <*> cExpressionP'

cConstantExpressionP :: PEParser CConstantExpression
cConstantExpressionP =
  parseElementP $ CConstantExpression <$> cConditionalExpressionP

cDeclarationP :: PEParser CDeclaration
cDeclarationP =
  parseElementP $
  CDeclaration <$> cDeclarationSpecifiersP <*> cInitDeclaratorListOptionalP

cDeclarationSpecifiersP :: PEParser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parseElementUnion
    [ CDeclarationSpecifiersStorageClass <$> cStorageClassSpecifierP <*>
      cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeSpecifier <$> cTypeSpecifierP <*>
      cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeQualifier <$> cTypeQualifierP <*>
      cDeclarationSpecifiersOptionalP
    ]

cInitDeclaratorListP :: PEParser CInitDeclaratorList
cInitDeclaratorListP =
  parseElementP $
  CInitDeclaratorList <$> cInitDeclaratorP <*> cInitDeclaratorListP'

cInitDeclaratorListP' :: PEParser CInitDeclaratorList'
cInitDeclaratorListP' =
  parseElementUnion
    [ CInitDeclaratorList' <$> (singleP LComma *> cInitDeclaratorP) <*>
      cInitDeclaratorListP'
    , pure CInitDeclaratorList'Empty
    ]

cInitDeclaratorP :: PEParser CInitDeclarator
cInitDeclaratorP =
  parseElementUnion
    [ CInitDeclarator <$> cDeclaratorP <*> (singleP LAssign *> cInitializerP)
    , CInitDeclaratorSingleton <$> cDeclaratorP
    ]

cStorageClassSpecifierP :: PEParser CStorageClassSpecifier
cStorageClassSpecifierP =
  parseElementUnion
    [ CStorageClassSpecifierTypedef <$ singleP LTypedef
    , CStorageClassSpecifierExtern <$ singleP LExtern
    , CStorageClassSpecifierStatic <$ singleP LStatic
    , CStorageClassSpecifierAuto <$ singleP LAuto
    , CStorageClassSpecifierRegister <$ singleP LRegister
    ]

cTypeSpecifierP :: PEParser CTypeSpecifier
cTypeSpecifierP =
  parseElementUnion
    [ CTypeSpecifierVoid <$ singleP LVoid
    , CTypeSpecifierChar <$ singleP LChar
    , CTypeSpecifierShort <$ singleP LShort
    , CTypeSpecifierInt <$ singleP LInt
    , CTypeSpecifierLong <$ singleP LLong
    , CTypeSpecifierFloat <$ singleP LFloat
    , CTypeSpecifierDouble <$ singleP LDouble
    , CTypeSpecifierSigned <$ singleP LSigned
    , CTypeSpecifierUnsigned <$ singleP LUnsigned
    , CTypeSpecifierStructOrUnion <$> cStructOrUnionSpecifierP
    , CTypeSpecifierEnum <$> cEnumSpecifierP
    , CTypeSpecifierTypedef <$> cTypedefNameP
    ]

cStructOrUnionSpecifierP :: PEParser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parseElementUnion
    [ CStructOrUnionSpecifierList <$> cStructOrUnionP <*> cIdentifierOptionalP <*>
      (singleP LBraceOpen *> cStructDeclarationListP <* singleP LBraceClose)
    , CStructOrUnionSpecifier <$> cStructOrUnionP <*> cIdentifierP
    ]

cStructOrUnionP :: PEParser CStructOrUnion
cStructOrUnionP =
  parseElementUnion
    [CStruct <$ singleP LStruct, CUnion <$ singleP LUnion]

cStructDeclarationListP :: PEParser CStructDeclarationList
cStructDeclarationListP =
  parseElementUnion
    [ CStructDeclarationList <$> cStructDeclarationP <*> cStructDeclarationListP
    , CStructDeclarationListSingleton <$> cStructDeclarationP
    ]

cStructDeclarationP :: PEParser CStructDeclaration
cStructDeclarationP =
  parseElementP $
  CStructDeclaration <$> cSpecifierQualifierListP <*>
  (cStructDeclaratorListP <* singleP LSemiColon)

cSpecifierQualifierListP :: PEParser CSpecifierQualifierList
cSpecifierQualifierListP =
  parseElementUnion
    [ CSpecifierQualifierListSpecifier <$> cTypeSpecifierP <*>
      cSpecifierQualifierListOptionalP
    , CSpecifierQualifierListQualifier <$> cTypeQualifierP <*>
      cSpecifierQualifierListOptionalP
    ]

cStructDeclaratorListP :: PEParser CStructDeclaratorList
cStructDeclaratorListP =
  parseElementP $
  CStructDeclaratorList <$> cStructDeclaratorP <*> cStructDeclaratorListP'

cStructDeclaratorListP' :: PEParser CStructDeclaratorList'
cStructDeclaratorListP' =
  parseElementUnion
    [ CStructDeclaratorList' <$> (singleP LComma *> cStructDeclaratorP) <*>
      cStructDeclaratorListP'
    , pure CStructDeclaratorList'Empty
    ]

cStructDeclaratorP :: PEParser CStructDeclarator
cStructDeclaratorP =
  parseElementUnion
    [ CStructDeclaratorInit <$> cDeclaratorOptionalP <*>
      (singleP LColon *> cConstantExpressionP)
    , CStructDeclarator <$> cDeclaratorP
    ]

cEnumSpecifierP :: PEParser CEnumSpecifier
cEnumSpecifierP =
  parseElementUnion
    [ CEnumSpecifierList <$> (singleP LEnum *> cIdentifierOptionalP) <*>
      (singleP LBraceOpen *> cEnumeratorListP <* singleP LBraceClose)
    , CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)
    ]

cEnumeratorListP :: PEParser CEnumeratorList
cEnumeratorListP =
  parseElementP $ CEnumeratorList <$> cEnumeratorP <*> cEnumeratorListP'

cEnumeratorListP' :: PEParser CEnumeratorList'
cEnumeratorListP' =
  parseElementUnion
    [ CEnumeratorList' <$> (singleP LComma *> cEnumeratorP) <*>
      cEnumeratorListP'
    , pure CEnumeratorList'Empty
    ]

cEnumeratorP :: PEParser CEnumerator
cEnumeratorP =
  parseElementUnion
    [ CEnumeratorAssign <$> cEnumerationConstantP <*>
      (singleP LAssign *> cConstantExpressionP)
    , CEnumerator <$> cEnumerationConstantP
    ]

cTypeQualifierP :: PEParser CTypeQualifier
cTypeQualifierP =
  parseElementUnion
    [ CTypeQualifierConst <$ singleP LConst
    , CTypeQualifierVolatile <$ singleP LVolatile
    ]

cDeclaratorP :: PEParser CDeclarator
cDeclaratorP =
  parseElementP $ CDeclarator <$> cPointerOptionalP <*> cDirectDeclaratorP

cDirectDeclaratorP :: PEParser CDirectDeclarator
cDirectDeclaratorP =
  parseElementUnion
    [ CDirectDeclaratorParen <$>
      (singleP LParenthesisOpen *> cDeclaratorP <* singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclaratorId <$> cIdentifierP <*> cDirectDeclaratorP'
    ]

cDirectDeclaratorP' :: PEParser CDirectDeclarator'
cDirectDeclaratorP' =
  parseElementUnion
    [ CDirectDeclarator'Indexed <$>
      (singleP LBracketOpen *> cConstantExpressionOptionalP <*
       singleP LBracketClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclarator'ParamTypeList <$>
      (singleP LParenthesisOpen *> cParameterTypeListP <*
       singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclarator'IdList <$>
      (singleP LParenthesisOpen *> cIdentifierListOptionalP <*
       singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , pure CDirectDeclarator'Empty
    ]

cPointerP :: PEParser CPointer
cPointerP =
  parseElementUnion
    [ CPointerMulti <$> (singleP LStar *> cTypeQualifierListOptionalP) <*>
      cPointerP
    , CPointerSingle <$> (singleP LStar *> cTypeQualifierListOptionalP)
    ]

cTypeQualifierListP :: PEParser CTypeQualifierList
cTypeQualifierListP =
  parseElementUnion
    [ CTypeQualifierList <$> cTypeQualifierP <*> cTypeQualifierListP
    , CTypeQualifierListSingleton <$> cTypeQualifierP
    ]

cParameterTypeListP :: PEParser CParameterTypeList
cParameterTypeListP =
  parseElementUnion
    [ CParameterTypeListVarargs <$>
      (cParameterListP <* singleP LComma <* singleP LVarargs)
    , CParameterTypeList <$> cParameterListP
    ]

cParameterListP :: PEParser CParameterList
cParameterListP =
  parseElementP $ CParameterList <$> cParameterDeclarationP <*> cParameterListP'

cParameterListP' :: PEParser CParameterList'
cParameterListP' =
  parseElementUnion
    [ CParameterList' <$> (singleP LComma *> cParameterDeclarationP) <*>
      cParameterListP'
    , pure CParameterList'Empty
    ]

cParameterDeclarationP :: PEParser CParameterDeclaration
cParameterDeclarationP =
  parseElementUnion
    [ CParameterDeclaration <$> cDeclarationSpecifiersP <*> cDeclaratorP
    , CParameterDeclarationAbstract <$> cDeclarationSpecifiersP <*>
      cAbstractDeclaratorOptionalP
    ]

cIdentifierListP :: PEParser CIdentifierList
cIdentifierListP =
  parseElementP $ CIdentifierList <$> cIdentifierP <*> cIdentifierListP'

cIdentifierListP' :: PEParser CIdentifierList'
cIdentifierListP' =
  parseElementUnion
    [ CIdentifierList' <$> (singleP LComma *> cIdentifierP) <*>
      cIdentifierListP'
    , pure CIdentifierList'Empty
    ]

cTypeNameP :: PEParser CTypeName
cTypeNameP =
  parseElementP $
  CTypeName <$> cSpecifierQualifierListP <*> cAbstractDeclaratorOptionalP

cAbstractDeclaratorP :: PEParser CAbstractDeclarator
cAbstractDeclaratorP =
  parseElementUnion
    [ CAbstractDeclaratorDirect <$> cPointerOptionalP <*>
      cDirectAbstractDeclaratorP
    , CAbstractDeclaratorPointer <$> cPointerP
    ]

cDirectAbstractDeclaratorP :: PEParser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP =
  parseElementUnion
    [ CDirectAbstractDeclaratorParens <$>
      (singleP LParenthesisOpen *> cAbstractDeclaratorP <*
       singleP LParenthesisClose) <*>
      cDirectAbstractDeclaratorP'
    , CDirectAbstractDeclaratorConst <$>
      (singleP LBracketOpen *> cConstantExpressionOptionalP <*
       singleP LBracketClose) <*>
      cDirectAbstractDeclaratorP'
    , CDirectAbstractDeclaratorParams <$>
      (singleP LParenthesisOpen *> cParameterTypeListOptionalP <*
       singleP LParenthesisClose) <*>
      cDirectAbstractDeclaratorP'
    ]

cDirectAbstractDeclaratorP' :: PEParser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parseElementUnion
    [ CDirectAbstractDeclarator'Const <$>
      (singleP LBracketOpen *> cConstantExpressionOptionalP <*
       singleP LBracketClose)
    , CDirectAbstractDeclarator'Params <$>
      (singleP LParenthesisOpen *> cParameterTypeListOptionalP <*
       singleP LParenthesisClose)
    ]

cTypedefNameP :: PEParser CTypedefName
cTypedefNameP = parseElementP $ CTypedefName <$> cIdentifierP

cInitializerP :: PEParser CInitializer
cInitializerP =
  parseElementUnion
    [ CInitializerBracketListComma <$>
      (singleP LBraceOpen *> cInitializerListP <* singleP LComma <*
       singleP LBraceClose)
    , CInitializerBracketList <$>
      (singleP LBraceOpen *> cInitializerListP <* singleP LBraceClose)
    , CInitializerAssignment <$> cAssignmentExpressionP
    ]

cInitializerListP :: PEParser CInitializerList
cInitializerListP =
  parseElementP $ CInitializerList <$> cInitializerP <*> cInitializerListP'

cInitializerListP' :: PEParser CInitializerList'
cInitializerListP' =
  parseElementUnion
    [ CInitializerList' <$> (singleP LComma *> cInitializerP) <*>
      cInitializerListP'
    , pure CInitializerList'Empty
    ]

cStatementP :: PEParser CStatement
cStatementP =
  parseElementUnion
    [ CStatementLabeled <$> cLabeledStatementP
    , CStatementCompound <$> cCompoundStatementP
    , CStatementExpression <$> cExpressionStatementP
    , CStatementSelection <$> cSelectionStatementP
    , CStatementIteration <$> cIterationStatementP
    , CStatementJump <$> cJumpStatementP
    ]

cLabeledStatementP :: PEParser CLabeledStatement
cLabeledStatementP =
  parseElementUnion
    [ CLabeledStatementDefault <$>
      (singleP LDefault *> singleP LColon *> cStatementP)
    , CLabeledStatementCase <$>
      (singleP LCase *> cConstantExpressionP <* singleP LColon) <*>
      cStatementP
    , CLabeledStatementId <$> cIdentifierP <*> (singleP LColon *> cStatementP)
    ]

cCompoundStatementP :: PEParser CCompoundStatement
cCompoundStatementP =
  parseElementP $
  CCompoundStatement <$> (singleP LBraceOpen *> cDeclarationListOptionalP) <*>
  (cStatementListOptionalP <* singleP LBraceClose)

cDeclarationListP :: PEParser CDeclarationList
cDeclarationListP =
  parseElementUnion
    [ CDeclarationList <$> cDeclarationP <*> cDeclarationListP
    , CDeclarationListSingleton <$> cDeclarationP
    ]

cStatementListP :: PEParser CStatementList
cStatementListP =
  parseElementUnion
    [ CStatementList <$> cStatementP <*> cStatementListP
    , CStatementListSingleton <$> cStatementP
    ]

cExpressionStatementP :: PEParser CExpressionStatement
cExpressionStatementP =
  parseElementP $
  CExpressionStatement <$> (cExpressionOptionalP <* singleP LSemiColon)

cSelectionStatementP :: PEParser CSelectionStatement
cSelectionStatementP =
  parseElementUnion
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

cIterationStatementP :: PEParser CIterationStatement
cIterationStatementP =
  parseElementUnion
    [ CIterationStatementWhile <$>
      (singleP LWhile *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose) <*>
      cStatementP
    , CIterationStatementDoWhile <$> (singleP LDo *> cStatementP) <*>
      (singleP LWhile *> singleP LParenthesisOpen *> cExpressionP <*
       singleP LParenthesisClose <*
       singleP LSemiColon)
    , CIterationStatementFor <$>
      (singleP LFor *> singleP LParenthesisOpen *> cExpressionOptionalP) <*>
      (singleP LSemiColon *> cExpressionOptionalP) <*>
      (singleP LSemiColon *> cExpressionOptionalP <* singleP LParenthesisClose) <*>
      cStatementP
    ]

cJumpStatementP :: PEParser CJumpStatement
cJumpStatementP =
  parseElementUnion
    [ CJumpStatementGoto <$>
      (singleP LGoto *> cIdentifierP <* singleP LSemiColon)
    , CJumpStatementContinue <$ (singleP LContinue <* singleP LSemiColon)
    , CJumpStatementBreak <$ (singleP LBreak <* singleP LSemiColon)
    , CJumpStatementReturn <$>
      (singleP LReturn *> cExpressionOptionalP <* singleP LSemiColon)
    ]

cTranslationUnitP :: PEParser CTranslationUnit
cTranslationUnitP =
  parseElementUnion
    [ CTranslationUnitTranslation <$> cExternalDeclarationP <*>
      cTranslationUnitP
    , CTranslationUnitExternal <$> cExternalDeclarationP
    ]

cExternalDeclarationP :: PEParser CExternalDeclaration
cExternalDeclarationP =
  parseElementUnion
    [ CExternalDeclarationFunction <$> cFunctionDefinitionP
    , CExternalDeclaration <$> cDeclarationP
    ]

cFunctionDefinitionP :: PEParser CFunctionDefinition
cFunctionDefinitionP =
  parseElementUnion
    [ CFunctionDefinitionSpecifiers <$> cDeclarationSpecifiersOptionalP <*>
      cDeclaratorP
    , CFunctionDefinitionList <$> cDeclarationListOptionalP <*>
      cCompoundStatementP
    ]

cDeclarationListOptionalP :: PEParser CDeclarationListOptional
cDeclarationListOptionalP =
  parseElementUnion
    [ CDeclarationListOptional <$> cDeclarationListP
    , pure CDeclarationListOptionalEmpty
    ]

cDeclarationSpecifiersOptionalP :: PEParser CDeclarationSpecifiersOptional
cDeclarationSpecifiersOptionalP =
  parseElementUnion
    [ CDeclarationSpecifiersOptional <$> cDeclarationSpecifiersP
    , pure CDeclarationSpecifiersOptionalEmpty
    ]

cExpressionOptionalP :: PEParser CExpressionOptional
cExpressionOptionalP =
  parseElementUnion
    [CExpressionOptional <$> cExpressionP, pure CExpressionOptionalEmpty]

cStatementListOptionalP :: PEParser CStatementListOptional
cStatementListOptionalP =
  parseElementUnion
    [ CStatementListOptional <$> cStatementListP
    , pure CStatementListOptionalEmpty
    ]

cParameterTypeListOptionalP :: PEParser CParameterTypeListOptional
cParameterTypeListOptionalP =
  parseElementUnion
    [ CParameterTypeListOptional <$> cParameterTypeListP
    , pure CParameterTypeListOptionalEmpty
    ]

cConstantExpressionOptionalP :: PEParser CConstantExpressionOptional
cConstantExpressionOptionalP =
  parseElementUnion
    [ CConstantExpressionOptional <$> cConstantExpressionP
    , pure CConstantExpressionOptionalEmpty
    ]

cPointerOptionalP :: PEParser CPointerOptional
cPointerOptionalP =
  parseElementUnion [CPointerOptional <$> cPointerP, pure CPointerOptionalEmpty]

cAbstractDeclaratorOptionalP :: PEParser CAbstractDeclaratorOptional
cAbstractDeclaratorOptionalP =
  parseElementUnion
    [ CAbstractDeclaratorOptional <$> cAbstractDeclaratorP
    , pure CAbstractDeclaratorOptionalEmpty
    ]

cTypeQualifierListOptionalP :: PEParser CTypeQualifierListOptional
cTypeQualifierListOptionalP =
  parseElementUnion
    [ CTypeQualifierListOptional <$> cTypeQualifierListP
    , pure CTypeQualifierListOptionalEmpty
    ]

cIdentifierListOptionalP :: PEParser CIdentifierListOptional
cIdentifierListOptionalP =
  parseElementUnion
    [ CIdentifierListOptional <$> cIdentifierListP
    , pure CIdentifierListOptionalEmpty
    ]

cIdentifierOptionalP :: PEParser CIdentifierOptional
cIdentifierOptionalP =
  parseElementUnion
    [CIdentifierOptional <$> cIdentifierP, pure CIdentifierOptionalEmpty]

cDeclaratorOptionalP :: PEParser CDeclaratorOptional
cDeclaratorOptionalP =
  parseElementUnion
    [CDeclaratorOptional <$> cDeclaratorP, pure CDeclaratorOptionalEmpty]

cSpecifierQualifierListOptionalP :: PEParser CSpecifierQualifierListOptional
cSpecifierQualifierListOptionalP =
  parseElementUnion
    [ CSpecifierQualifierListOptional <$> cSpecifierQualifierListP
    , pure CSpecifierQualifierListOptionalEmpty
    ]

cInitDeclaratorListOptionalP :: PEParser CInitDeclaratorListOptional
cInitDeclaratorListOptionalP =
  parseElementUnion
    [ CInitDeclaratorListOptional <$> cInitDeclaratorListP
    , pure CInitDeclaratorListOptionalEmpty
    ]

cArgumentExpressionListOptionalP :: PEParser CArgumentExpressionListOptional
cArgumentExpressionListOptionalP =
  parseElementUnion
    [ CArgumentExpressionListOptional <$> cArgumentExpressionListP
    , pure CArgumentExpressionListOptionalEmpty
    ]
