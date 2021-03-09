{-# LANGUAGE LambdaCase #-}

{-
strategy : each parser tries each possible option
           and returns a nonempty list of possible parse trees
           or ParseError
-}
module Parser where

import Data.List
import Data.Either

import Lexeme (CLexeme(..))
import ParseElements
import Scanner (Coordinates, ScanElement(..))

-----------
-- TYPES --
-----------
type Input = [ScanElement CLexeme]

type ParseOutput a = Either ParseError [(Input, ParseElement a)]

newtype Parser a =
  Parser
    { runParser :: Input -> ParseOutput a
    }

data ParseError =
  ParseError
    { errorLoc :: Coordinates
    , errorMsg :: String
    }
  deriving (Eq, Show)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap . fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right [(input, ParseElement (getLoc input) x)]
  p1 <*> p2 =
    Parser $ \input -> do
      rab <- runParser p1 input
      collectOutput $ map
        (\(input', ParseElement _ fab) ->
          runParser (fab <$> p2) input')
        rab

{-

-- CTranslationUnit is the root element in the grammar
cParser :: Parser CTranslationUnit
cParser =
  Parser $ \input ->
    case runParser cTranslationUnitP input of
      e@(Left _) -> e
      r@(Right ([], _)) -> r
      r@(Right (notParsed:_, _)) -> r
      --  Left $
      --  ParseError (fst $ notParsed) "Parsing terminated prematurely"
-}

-----------
-- UTILS --
-----------
getLoc :: Input -> Coordinates
getLoc [] = (0, 0)
getLoc i = scanLoc $ head i

collectOutput :: [ParseOutput a] -> ParseOutput a
collectOutput p =
  if length successful == 0
    then maximumBy (\(Left (ParseError c1 _)) (Left (ParseError c2 _)) -> compare c1 c2) p
    else concat <$> sequenceA successful
  where
    successful = filter isRight p

parserUnion :: [Parser a] -> Parser a
parserUnion parsers =
  Parser $ \input -> collectOutput $ map (\p -> runParser p input) parsers

unexpectedEof :: ParseError
unexpectedEof = ParseError (0, 0) "Unexpected EOF"

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
        then Right [(rest, ParseElement c l)]
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: Parser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LIntLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: Parser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LFloatLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: Parser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LCharLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: Parser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LStringLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: Parser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LLabel x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l

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
      (singleP LParenthesisOpen *> cArgumentExpressionListOptionalP <*
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
    [ CUnaryOperatorAddress <$ singleP LAmp
    , CUnaryOperatorMultiply <$ singleP LStar
    , CUnaryOperatorPlus <$ singleP LPlus
    , CUnaryOperatorMinus <$ singleP LMinus
    , CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot
    , CUnaryOperatorNot <$ singleP LNot
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
cAndExpressionP =
  CAndExpression <$> cEqualityExpressionP <*> cAndExpressionP'

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

cExpressionP :: Parser CExpression
cExpressionP =
  CExpression <$> cAssignmentExpressionP <*> cExpressionP'

cExpressionP' :: Parser CExpression'
cExpressionP' =
  CExpression' <$> (singleP LComma *> cAssignmentExpressionP) <*> cExpressionP'

cConstantExpressionP :: Parser CConstantExpression
cConstantExpressionP =
  CConstantExpression <$> cConditionalExpressionP

cDeclarationP :: Parser CDeclaration
cDeclarationP =
  CDeclaration <$> cDeclarationSpecifiersP <*> cInitDeclaratorListOptionalP

cDeclarationSpecifiersP :: Parser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserUnion
    [ CDeclarationSpecifiersStorageClass <$> cStorageClassSpecifierP <*>
      cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeSpecifier <$> cTypeSpecifierP <*>
      cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeQualifier <$> cTypeQualifierP <*>
      cDeclarationSpecifiersOptionalP
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
    [ CStorageClassSpecifierTypedef <$ singleP LTypedef
    , CStorageClassSpecifierExtern <$ singleP LExtern
    , CStorageClassSpecifierStatic <$ singleP LStatic
    , CStorageClassSpecifierAuto <$ singleP LAuto
    , CStorageClassSpecifierRegister <$ singleP LRegister
    ]

cTypeSpecifierP :: Parser CTypeSpecifier
cTypeSpecifierP =
  parserUnion
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

cStructOrUnionSpecifierP :: Parser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserUnion
    [ CStructOrUnionSpecifierList <$> cStructOrUnionP <*> cIdentifierOptionalP <*>
      (singleP LBraceOpen *> cStructDeclarationListP <* singleP LBraceClose)
    , CStructOrUnionSpecifier <$> cStructOrUnionP <*> cIdentifierP
    ]

cStructOrUnionP :: Parser CStructOrUnion
cStructOrUnionP =
  parserUnion [CStruct <$ singleP LStruct, CUnion <$ singleP LUnion]

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
      cSpecifierQualifierListOptionalP
    , CSpecifierQualifierListQualifier <$> cTypeQualifierP <*>
      cSpecifierQualifierListOptionalP
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
    [ CStructDeclaratorInit <$> cDeclaratorOptionalP <*>
      (singleP LColon *> cConstantExpressionP)
    , CStructDeclarator <$> cDeclaratorP
    ]

cEnumSpecifierP :: Parser CEnumSpecifier
cEnumSpecifierP =
  parserUnion
    [ CEnumSpecifierList <$> (singleP LEnum *> cIdentifierOptionalP) <*>
      (singleP LBraceOpen *> cEnumeratorListP <* singleP LBraceClose)
    , CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)
    ]

cEnumeratorListP :: Parser CEnumeratorList
cEnumeratorListP =
  CEnumeratorList <$> cEnumeratorP <*> cEnumeratorListP'

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
    [ CTypeQualifierConst <$ singleP LConst
    , CTypeQualifierVolatile <$ singleP LVolatile
    ]

cDeclaratorP :: Parser CDeclarator
cDeclaratorP =
  CDeclarator <$> cPointerOptionalP <*> cDirectDeclaratorP

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

cPointerP :: Parser CPointer
cPointerP =
  parserUnion
    [ CPointerMulti <$> (singleP LStar *> cTypeQualifierListOptionalP) <*>
      cPointerP
    , CPointerSingle <$> (singleP LStar *> cTypeQualifierListOptionalP)
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
cParameterListP =
  CParameterList <$> cParameterDeclarationP <*> cParameterListP'

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
      cAbstractDeclaratorOptionalP
    ]

cIdentifierListP :: Parser CIdentifierList
cIdentifierListP =
  CIdentifierList <$> cIdentifierP <*> cIdentifierListP'

cIdentifierListP' :: Parser CIdentifierList'
cIdentifierListP' =
  parserUnion
    [ CIdentifierList' <$> (singleP LComma *> cIdentifierP) <*>
      cIdentifierListP'
    , pure CIdentifierList'Empty
    ]

cTypeNameP :: Parser CTypeName
cTypeNameP =
  CTypeName <$> cSpecifierQualifierListP <*> cAbstractDeclaratorOptionalP

cAbstractDeclaratorP :: Parser CAbstractDeclarator
cAbstractDeclaratorP =
  parserUnion
    [ CAbstractDeclaratorDirect <$> cPointerOptionalP <*>
      cDirectAbstractDeclaratorP
    , CAbstractDeclaratorPointer <$> cPointerP
    ]

cDirectAbstractDeclaratorP :: Parser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP =
  parserUnion
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

cDirectAbstractDeclaratorP' :: Parser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parserUnion
    [ CDirectAbstractDeclarator'Const <$>
      (singleP LBracketOpen *> cConstantExpressionOptionalP <*
       singleP LBracketClose)
    , CDirectAbstractDeclarator'Params <$>
      (singleP LParenthesisOpen *> cParameterTypeListOptionalP <*
       singleP LParenthesisClose)
    ]

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
cInitializerListP =
  CInitializerList <$> cInitializerP <*> cInitializerListP'

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
  CCompoundStatement <$> (singleP LBraceOpen *> cDeclarationListOptionalP) <*>
  (cStatementListOptionalP <* singleP LBraceClose)

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
  CExpressionStatement <$> (cExpressionOptionalP <* singleP LSemiColon)

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
      (singleP LFor *> singleP LParenthesisOpen *> cExpressionOptionalP) <*>
      (singleP LSemiColon *> cExpressionOptionalP) <*>
      (singleP LSemiColon *> cExpressionOptionalP <* singleP LParenthesisClose) <*>
      cStatementP
    ]

cJumpStatementP :: Parser CJumpStatement
cJumpStatementP =
  parserUnion
    [ CJumpStatementGoto <$>
      (singleP LGoto *> cIdentifierP <* singleP LSemiColon)
    , CJumpStatementContinue <$ (singleP LContinue <* singleP LSemiColon)
    , CJumpStatementBreak <$ (singleP LBreak <* singleP LSemiColon)
    , CJumpStatementReturn <$>
      (singleP LReturn *> cExpressionOptionalP <* singleP LSemiColon)
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
    [ CFunctionDefinitionSpecifiers <$> cDeclarationSpecifiersOptionalP <*>
      cDeclaratorP
    , CFunctionDefinitionList <$> cDeclarationListOptionalP <*>
      cCompoundStatementP
    ]

cDeclarationListOptionalP :: Parser CDeclarationListOptional
cDeclarationListOptionalP =
  parserUnion
    [ CDeclarationListOptional <$> cDeclarationListP
    , pure CDeclarationListOptionalEmpty
    ]

cDeclarationSpecifiersOptionalP :: Parser CDeclarationSpecifiersOptional
cDeclarationSpecifiersOptionalP =
  parserUnion
    [ CDeclarationSpecifiersOptional <$> cDeclarationSpecifiersP
    , pure CDeclarationSpecifiersOptionalEmpty
    ]

cExpressionOptionalP :: Parser CExpressionOptional
cExpressionOptionalP =
  parserUnion
    [CExpressionOptional <$> cExpressionP, pure CExpressionOptionalEmpty]

cStatementListOptionalP :: Parser CStatementListOptional
cStatementListOptionalP =
  parserUnion
    [ CStatementListOptional <$> cStatementListP
    , pure CStatementListOptionalEmpty
    ]

cParameterTypeListOptionalP :: Parser CParameterTypeListOptional
cParameterTypeListOptionalP =
  parserUnion
    [ CParameterTypeListOptional <$> cParameterTypeListP
    , pure CParameterTypeListOptionalEmpty
    ]

cConstantExpressionOptionalP :: Parser CConstantExpressionOptional
cConstantExpressionOptionalP =
  parserUnion
    [ CConstantExpressionOptional <$> cConstantExpressionP
    , pure CConstantExpressionOptionalEmpty
    ]

cPointerOptionalP :: Parser CPointerOptional
cPointerOptionalP =
  parserUnion [CPointerOptional <$> cPointerP, pure CPointerOptionalEmpty]

cAbstractDeclaratorOptionalP :: Parser CAbstractDeclaratorOptional
cAbstractDeclaratorOptionalP =
  parserUnion
    [ CAbstractDeclaratorOptional <$> cAbstractDeclaratorP
    , pure CAbstractDeclaratorOptionalEmpty
    ]

cTypeQualifierListOptionalP :: Parser CTypeQualifierListOptional
cTypeQualifierListOptionalP =
  parserUnion
    [ CTypeQualifierListOptional <$> cTypeQualifierListP
    , pure CTypeQualifierListOptionalEmpty
    ]

cIdentifierListOptionalP :: Parser CIdentifierListOptional
cIdentifierListOptionalP =
  parserUnion
    [ CIdentifierListOptional <$> cIdentifierListP
    , pure CIdentifierListOptionalEmpty
    ]

cIdentifierOptionalP :: Parser CIdentifierOptional
cIdentifierOptionalP =
  parserUnion
    [CIdentifierOptional <$> cIdentifierP, pure CIdentifierOptionalEmpty]

cDeclaratorOptionalP :: Parser CDeclaratorOptional
cDeclaratorOptionalP =
  parserUnion
    [CDeclaratorOptional <$> cDeclaratorP, pure CDeclaratorOptionalEmpty]

cSpecifierQualifierListOptionalP :: Parser CSpecifierQualifierListOptional
cSpecifierQualifierListOptionalP =
  parserUnion
    [ CSpecifierQualifierListOptional <$> cSpecifierQualifierListP
    , pure CSpecifierQualifierListOptionalEmpty
    ]

cInitDeclaratorListOptionalP :: Parser CInitDeclaratorListOptional
cInitDeclaratorListOptionalP =
  parserUnion
    [ CInitDeclaratorListOptional <$> cInitDeclaratorListP
    , pure CInitDeclaratorListOptionalEmpty
    ]

cArgumentExpressionListOptionalP :: Parser CArgumentExpressionListOptional
cArgumentExpressionListOptionalP =
  parserUnion
    [ CArgumentExpressionListOptional <$> cArgumentExpressionListP
    , pure CArgumentExpressionListOptionalEmpty
    ]

