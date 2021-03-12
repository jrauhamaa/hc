{-# LANGUAGE LambdaCase #-}

{-
strategy : each parser tries each possible option
           and returns a nonempty list of possible parse trees
           or ParseError
-}
module Parser where

import Data.Either
import Data.List

import Lexeme (CLexeme(..))
import ParseElements
import Scanner (ScanElement(..))

-----------
-- TYPES --
-----------
type Input = [ScanElement CLexeme]

type ParseOutput a = Either ParseError [(Input, a)]

newtype Parser a =
  Parser
    { runParser :: Input -> ParseOutput a
    }

type PEParser a = Parser (ParseElement a)

newtype ParseError =
  ParseError String
  deriving (Eq, Show)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right [(input, x)]
  p1 <*> p2 =
    Parser $ \input -> do
      rab <- runParser p1 input
      collectOutput $
        map
          (\(input', fab) -> (fmap . fmap) fab <$> runParser p2 input')
          rab

parseCCode :: Input -> Either ParseError (ParseElement CTranslationUnit)
parseCCode input = do
  results <- runParser cTranslationUnitP input
  let x = minimumBy (\a b -> compare (length $ fst a) (length $ fst b)) results
  case x of
    ([], _) -> Right $ snd x
    _ -> Left $ ParseError "Unable to parse entire file"

-----------
-- UTILS --
-----------
collectOutput :: [ParseOutput a] -> ParseOutput a
collectOutput p =
  if null successful
    then Left $ ParseError "error"
    else concat <$> sequenceA successful
  where
    successful = filter isRight p

parserUnion :: [Parser a] -> PEParser a
parserUnion parsers =
  parserToPEParser
    (Parser $ \input -> collectOutput $ map (`runParser` input) parsers)

parserToPEParser :: Parser a -> PEParser a
parserToPEParser p =
  Parser $ \case
    [] -> (fmap . fmap) (ParseElement (0, 0)) <$> runParser p []
    a@((ScanElement c _):_) ->
      (fmap . fmap) (ParseElement c) <$> runParser p a

unexpectedEof :: ParseError
unexpectedEof = ParseError "Unexpected EOF"

unexpectedLexeme :: String -> String -> ParseError
unexpectedLexeme expected encountered =
  ParseError $
  mconcat ["Expected ", expected, ". Instead encountered ", encountered, "."]

------------------------
-- ELEMENTARY PARSERS --
------------------------
singleP :: CLexeme -> Parser CLexeme
singleP l =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement _ lexeme):rest) ->
      if lexeme == l
        then Right [(rest, l)]
        else Left $ unexpectedLexeme (show l) (show lexeme)

intLiteralP :: PEParser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LIntLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement _ l):_) -> Left $ unexpectedLexeme "LIntLiteral" $ show l

floatLiteralP :: PEParser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LFloatLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement _ l):_) -> Left $ unexpectedLexeme "LFloatLiteral" $ show l

charLiteralP :: PEParser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LCharLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement _ l):_) -> Left $ unexpectedLexeme "LCharLiteral" $ show l

stringLiteralP :: PEParser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LStringLiteral x)):rest) ->
      Right [(rest, ParseElement c x)]
    ((ScanElement _ l):_) -> Left $ unexpectedLexeme "LStringLiteral" $ show l

labelP :: PEParser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LLabel x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement _ l):_) -> Left $ unexpectedLexeme "LLabel" $ show l

---------------
-- C PARSERS --
---------------
cConstantP :: PEParser CConstant
cConstantP =
  parserUnion
    [ CConstantInteger <$> intLiteralP
    , CConstantFloat <$> floatLiteralP
    , CConstantCharacter <$> charLiteralP
    , CConstantEnumeration <$> cEnumerationConstantP
    ]

cIdentifierP :: PEParser CIdentifier
cIdentifierP = parserToPEParser (CIdentifier <$> labelP)

cEnumerationConstantP :: PEParser CEnumerationConstant
cEnumerationConstantP = parserToPEParser (CEnumerationConstant <$> cIdentifierP)

cPrimaryExpressionP :: PEParser CPrimaryExpression
cPrimaryExpressionP =
  parserUnion
    [ CPrimaryExpressionId <$> cIdentifierP
    , CPrimaryExpressionConst <$> cConstantP
    , CPrimaryExpressionStr <$> stringLiteralP
    , CPrimaryExpressionParen <$>
      (singleP LParenthesisOpen *> cExpressionP <* singleP LParenthesisClose)
    ]

cPostfixExpressionP :: PEParser CPostfixExpression
cPostfixExpressionP =
  parserToPEParser
    (CPostfixExpression <$> cPrimaryExpressionP <*> cPostfixExpressionP')

cPostfixExpressionP' :: PEParser CPostfixExpression'
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

cArgumentExpressionListP :: PEParser CArgumentExpressionList
cArgumentExpressionListP =
  parserToPEParser
    (CArgumentExpressionList <$> cAssignmentExpressionP <*>
     cArgumentExpressionListP')

cArgumentExpressionListP' :: PEParser CArgumentExpressionList'
cArgumentExpressionListP' =
  parserUnion
    [ CArgumentExpressionList' <$> cAssignmentExpressionP <*>
      cArgumentExpressionListP'
    , pure CArgumentExpressionList'Empty
    ]

cUnaryExpressionP :: PEParser CUnaryExpression
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

cUnaryOperatorP :: PEParser CUnaryOperator
cUnaryOperatorP =
  parserUnion
    [ CUnaryOperatorAddress <$ singleP LAmp
    , CUnaryOperatorMultiply <$ singleP LStar
    , CUnaryOperatorPlus <$ singleP LPlus
    , CUnaryOperatorMinus <$ singleP LMinus
    , CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot
    , CUnaryOperatorNot <$ singleP LNot
    ]

cCastExpressionP :: PEParser CCastExpression
cCastExpressionP =
  parserUnion
    [ CCastExpression <$>
      (singleP LParenthesisOpen *> cTypeNameP <* singleP LParenthesisClose) <*>
      cCastExpressionP
    , CCastExpressionSingleton <$> cUnaryExpressionP
    ]

cMultiplicativeExpressionP :: PEParser CMultiplicativeExpression
cMultiplicativeExpressionP =
  parserToPEParser
    (CMultiplicativeExpression <$> cCastExpressionP <*>
     cMultiplicativeExpressionP')

cMultiplicativeExpressionP' :: PEParser CMultiplicativeExpression'
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

cAdditiveExpressionP :: PEParser CAdditiveExpression
cAdditiveExpressionP =
  parserToPEParser
    (CAdditiveExpression <$> cMultiplicativeExpressionP <*>
     cAdditiveExpressionP')

cAdditiveExpressionP' :: PEParser CAdditiveExpression'
cAdditiveExpressionP' =
  parserUnion
    [ CAdditiveExpression'Plus <$> (singleP LPlus *> cMultiplicativeExpressionP) <*>
      cAdditiveExpressionP'
    , CAdditiveExpression'Minus <$>
      (singleP LMinus *> cMultiplicativeExpressionP) <*>
      cAdditiveExpressionP'
    , pure CAdditiveExpression'Empty
    ]

cShiftExpressionP :: PEParser CShiftExpression
cShiftExpressionP =
  parserToPEParser
    (CShiftExpression <$> cAdditiveExpressionP <*> cShiftExpressionP')

cShiftExpressionP' :: PEParser CShiftExpression'
cShiftExpressionP' =
  parserUnion
    [ CShiftExpression'Left <$> (singleP LBitShiftLeft *> cAdditiveExpressionP) <*>
      cShiftExpressionP'
    , CShiftExpression'Right <$>
      (singleP LBitShiftRight *> cAdditiveExpressionP) <*>
      cShiftExpressionP'
    , pure CShiftExpression'Empty
    ]

cRelationalExpressionP :: PEParser CRelationalExpression
cRelationalExpressionP =
  parserToPEParser
    (CRelationalExpression <$> cShiftExpressionP <*> cRelationalExpressionP')

cRelationalExpressionP' :: PEParser CRelationalExpression'
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

cEqualityExpressionP :: PEParser CEqualityExpression
cEqualityExpressionP =
  parserToPEParser
    (CEqualityExpression <$> cRelationalExpressionP <*> cEqualityExpressionP')

cEqualityExpressionP' :: PEParser CEqualityExpression'
cEqualityExpressionP' =
  parserUnion
    [ CEqualityExpression'EQ <$> (singleP LEquals *> cRelationalExpressionP) <*>
      cEqualityExpressionP'
    , CEqualityExpression'NEQ <$> (singleP LNotEquals *> cRelationalExpressionP) <*>
      cEqualityExpressionP'
    , pure CEqualityExpression'Empty
    ]

cAndExpressionP :: PEParser CAndExpression
cAndExpressionP =
  parserToPEParser
    (CAndExpression <$> cEqualityExpressionP <*> cAndExpressionP')

cAndExpressionP' :: PEParser CAndExpression'
cAndExpressionP' =
  parserUnion
    [ CAndExpression' <$> (singleP LAmp *> cEqualityExpressionP) <*>
      cAndExpressionP'
    , pure CAndExpression'Empty
    ]

cExclusiveOrExpressionP :: PEParser CExclusiveOrExpression
cExclusiveOrExpressionP =
  parserToPEParser
    (CExclusiveOrExpression <$> cAndExpressionP <*> cExclusiveOrExpressionP')

cExclusiveOrExpressionP' :: PEParser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parserUnion
    [ CExclusiveOrExpression' <$> (singleP LBitwiseXor *> cAndExpressionP) <*>
      cExclusiveOrExpressionP'
    , pure CExclusiveOrExpression'Empty
    ]

cInclusiveOrExpressionP :: PEParser CInclusiveOrExpression
cInclusiveOrExpressionP =
  parserToPEParser
    (CInclusiveOrExpression <$> cExclusiveOrExpressionP <*>
     cInclusiveOrExpressionP')

cInclusiveOrExpressionP' :: PEParser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parserUnion
    [ CInclusiveOrExpression' <$>
      (singleP LBitwiseOr *> cExclusiveOrExpressionP) <*>
      cInclusiveOrExpressionP'
    , pure CInclusiveOrExpression'Empty
    ]

cLogicalAndExpressionP :: PEParser CLogicalAndExpression
cLogicalAndExpressionP =
  parserToPEParser
    (CLogicalAndExpression <$> cInclusiveOrExpressionP <*>
     cLogicalAndExpressionP')

cLogicalAndExpressionP' :: PEParser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parserUnion
    [ CLogicalAndExpression' <$> (singleP LAnd *> cInclusiveOrExpressionP) <*>
      cLogicalAndExpressionP'
    , pure CLogicalAndExpression'Empty
    ]

cLogicalOrExpressionP :: PEParser CLogicalOrExpression
cLogicalOrExpressionP =
  parserToPEParser
    (CLogicalOrExpression <$> cLogicalAndExpressionP <*> cLogicalOrExpressionP')

cLogicalOrExpressionP' :: PEParser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parserUnion
    [ CLogicalOrExpression' <$> (singleP LOr *> cLogicalAndExpressionP) <*>
      cLogicalOrExpressionP'
    , pure CLogicalOrExpression'Empty
    ]

cConditionalExpressionP :: PEParser CConditionalExpression
cConditionalExpressionP =
  parserUnion
    [ CConditionalExpression <$> cLogicalOrExpressionP <*>
      (singleP LTernary *> cExpressionP <* singleP LColon) <*>
      cConditionalExpressionP
    , CConditionalExpressionSingleton <$> cLogicalOrExpressionP
    ]

cAssignmentExpressionP :: PEParser CAssignmentExpression
cAssignmentExpressionP =
  parserUnion
    [ CAssignmentExpression <$> cUnaryExpressionP <*> cAssignmentOperatorP <*>
      cAssignmentExpressionP
    , CAssignmentExpressionSingleton <$> cConditionalExpressionP
    ]

cAssignmentOperatorP :: PEParser CAssignmentOperator
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

cExpressionP :: PEParser CExpression
cExpressionP =
  parserToPEParser (CExpression <$> cAssignmentExpressionP <*> cExpressionP')

cExpressionP' :: PEParser CExpression'
cExpressionP' =
  parserUnion
    [ CExpression' <$> (singleP LComma *> cAssignmentExpressionP) <*>
      cExpressionP'
    , pure CExpression'Empty
    ]

cConstantExpressionP :: PEParser CConstantExpression
cConstantExpressionP =
  parserToPEParser (CConstantExpression <$> cConditionalExpressionP)

cDeclarationP :: PEParser CDeclaration
cDeclarationP =
  parserToPEParser
    (CDeclaration <$> cDeclarationSpecifiersP <*> cInitDeclaratorListOptionalP)

cDeclarationSpecifiersP :: PEParser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserUnion
    [ CDeclarationSpecifiersStorageClass <$> cStorageClassSpecifierP <*>
      cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeSpecifier <$> cTypeSpecifierP <*>
      cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeQualifier <$> cTypeQualifierP <*>
      cDeclarationSpecifiersOptionalP
    ]

cInitDeclaratorListP :: PEParser CInitDeclaratorList
cInitDeclaratorListP =
  parserToPEParser
    (CInitDeclaratorList <$> cInitDeclaratorP <*> cInitDeclaratorListP')

cInitDeclaratorListP' :: PEParser CInitDeclaratorList'
cInitDeclaratorListP' =
  parserUnion
    [ CInitDeclaratorList' <$> (singleP LComma *> cInitDeclaratorP) <*>
      cInitDeclaratorListP'
    , pure CInitDeclaratorList'Empty
    ]

cInitDeclaratorP :: PEParser CInitDeclarator
cInitDeclaratorP =
  parserUnion
    [ CInitDeclarator <$> cDeclaratorP <*> (singleP LAssign *> cInitializerP)
    , CInitDeclaratorSingleton <$> cDeclaratorP
    ]

cStorageClassSpecifierP :: PEParser CStorageClassSpecifier
cStorageClassSpecifierP =
  parserUnion
    [ CStorageClassSpecifierTypedef <$ singleP LTypedef
    , CStorageClassSpecifierExtern <$ singleP LExtern
    , CStorageClassSpecifierStatic <$ singleP LStatic
    , CStorageClassSpecifierAuto <$ singleP LAuto
    , CStorageClassSpecifierRegister <$ singleP LRegister
    ]

cTypeSpecifierP :: PEParser CTypeSpecifier
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

cStructOrUnionSpecifierP :: PEParser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserUnion
    [ CStructOrUnionSpecifierList <$> cStructOrUnionP <*> cIdentifierOptionalP <*>
      (singleP LBraceOpen *> cStructDeclarationListP <* singleP LBraceClose)
    , CStructOrUnionSpecifier <$> cStructOrUnionP <*> cIdentifierP
    ]

cStructOrUnionP :: PEParser CStructOrUnion
cStructOrUnionP =
  parserUnion [CStruct <$ singleP LStruct, CUnion <$ singleP LUnion]

cStructDeclarationListP :: PEParser CStructDeclarationList
cStructDeclarationListP =
  parserUnion
    [ CStructDeclarationList <$> cStructDeclarationP <*> cStructDeclarationListP
    , CStructDeclarationListSingleton <$> cStructDeclarationP
    ]

cStructDeclarationP :: PEParser CStructDeclaration
cStructDeclarationP =
  parserToPEParser
    (CStructDeclaration <$> cSpecifierQualifierListP <*>
     (cStructDeclaratorListP <* singleP LSemiColon))

cSpecifierQualifierListP :: PEParser CSpecifierQualifierList
cSpecifierQualifierListP =
  parserUnion
    [ CSpecifierQualifierListSpecifier <$> cTypeSpecifierP <*>
      cSpecifierQualifierListOptionalP
    , CSpecifierQualifierListQualifier <$> cTypeQualifierP <*>
      cSpecifierQualifierListOptionalP
    ]

cStructDeclaratorListP :: PEParser CStructDeclaratorList
cStructDeclaratorListP =
  parserToPEParser
    (CStructDeclaratorList <$> cStructDeclaratorP <*> cStructDeclaratorListP')

cStructDeclaratorListP' :: PEParser CStructDeclaratorList'
cStructDeclaratorListP' =
  parserUnion
    [ CStructDeclaratorList' <$> (singleP LComma *> cStructDeclaratorP) <*>
      cStructDeclaratorListP'
    , pure CStructDeclaratorList'Empty
    ]

cStructDeclaratorP :: PEParser CStructDeclarator
cStructDeclaratorP =
  parserUnion
    [ CStructDeclaratorInit <$> cDeclaratorOptionalP <*>
      (singleP LColon *> cConstantExpressionP)
    , CStructDeclarator <$> cDeclaratorP
    ]

cEnumSpecifierP :: PEParser CEnumSpecifier
cEnumSpecifierP =
  parserUnion
    [ CEnumSpecifierList <$> (singleP LEnum *> cIdentifierOptionalP) <*>
      (singleP LBraceOpen *> cEnumeratorListP <* singleP LBraceClose)
    , CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)
    ]

cEnumeratorListP :: PEParser CEnumeratorList
cEnumeratorListP =
  parserToPEParser (CEnumeratorList <$> cEnumeratorP <*> cEnumeratorListP')

cEnumeratorListP' :: PEParser CEnumeratorList'
cEnumeratorListP' =
  parserUnion
    [ CEnumeratorList' <$> (singleP LComma *> cEnumeratorP) <*>
      cEnumeratorListP'
    , pure CEnumeratorList'Empty
    ]

cEnumeratorP :: PEParser CEnumerator
cEnumeratorP =
  parserUnion
    [ CEnumeratorAssign <$> cEnumerationConstantP <*>
      (singleP LAssign *> cConstantExpressionP)
    , CEnumerator <$> cEnumerationConstantP
    ]

cTypeQualifierP :: PEParser CTypeQualifier
cTypeQualifierP =
  parserUnion
    [ CTypeQualifierConst <$ singleP LConst
    , CTypeQualifierVolatile <$ singleP LVolatile
    ]

cDeclaratorP :: PEParser CDeclarator
cDeclaratorP =
  parserToPEParser (CDeclarator <$> cPointerOptionalP <*> cDirectDeclaratorP)

cDirectDeclaratorP :: PEParser CDirectDeclarator
cDirectDeclaratorP =
  parserUnion
    [ CDirectDeclaratorParen <$>
      (singleP LParenthesisOpen *> cDeclaratorP <* singleP LParenthesisClose) <*>
      cDirectDeclaratorP'
    , CDirectDeclaratorId <$> cIdentifierP <*> cDirectDeclaratorP'
    ]

cDirectDeclaratorP' :: PEParser CDirectDeclarator'
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

cPointerP :: PEParser CPointer
cPointerP =
  parserUnion
    [ CPointerMulti <$> (singleP LStar *> cTypeQualifierListOptionalP) <*>
      cPointerP
    , CPointerSingle <$> (singleP LStar *> cTypeQualifierListOptionalP)
    ]

cTypeQualifierListP :: PEParser CTypeQualifierList
cTypeQualifierListP =
  parserUnion
    [ CTypeQualifierList <$> cTypeQualifierP <*> cTypeQualifierListP
    , CTypeQualifierListSingleton <$> cTypeQualifierP
    ]

cParameterTypeListP :: PEParser CParameterTypeList
cParameterTypeListP =
  parserUnion
    [ CParameterTypeListVarargs <$>
      (cParameterListP <* singleP LComma <* singleP LVarargs)
    , CParameterTypeList <$> cParameterListP
    ]

cParameterListP :: PEParser CParameterList
cParameterListP =
  parserToPEParser
    (CParameterList <$> cParameterDeclarationP <*> cParameterListP')

cParameterListP' :: PEParser CParameterList'
cParameterListP' =
  parserUnion
    [ CParameterList' <$> (singleP LComma *> cParameterDeclarationP) <*>
      cParameterListP'
    , pure CParameterList'Empty
    ]

cParameterDeclarationP :: PEParser CParameterDeclaration
cParameterDeclarationP =
  parserUnion
    [ CParameterDeclaration <$> cDeclarationSpecifiersP <*> cDeclaratorP
    , CParameterDeclarationAbstract <$> cDeclarationSpecifiersP <*>
      cAbstractDeclaratorOptionalP
    ]

cIdentifierListP :: PEParser CIdentifierList
cIdentifierListP =
  parserToPEParser (CIdentifierList <$> cIdentifierP <*> cIdentifierListP')

cIdentifierListP' :: PEParser CIdentifierList'
cIdentifierListP' =
  parserUnion
    [ CIdentifierList' <$> (singleP LComma *> cIdentifierP) <*>
      cIdentifierListP'
    , pure CIdentifierList'Empty
    ]

cTypeNameP :: PEParser CTypeName
cTypeNameP =
  parserToPEParser
    (CTypeName <$> cSpecifierQualifierListP <*> cAbstractDeclaratorOptionalP)

cAbstractDeclaratorP :: PEParser CAbstractDeclarator
cAbstractDeclaratorP =
  parserUnion
    [ CAbstractDeclaratorDirect <$> cPointerOptionalP <*>
      cDirectAbstractDeclaratorP
    , CAbstractDeclaratorPointer <$> cPointerP
    ]

cDirectAbstractDeclaratorP :: PEParser CDirectAbstractDeclarator
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

cDirectAbstractDeclaratorP' :: PEParser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parserUnion
    [ CDirectAbstractDeclarator'Const <$>
      (singleP LBracketOpen *> cConstantExpressionOptionalP <*
       singleP LBracketClose)
    , CDirectAbstractDeclarator'Params <$>
      (singleP LParenthesisOpen *> cParameterTypeListOptionalP <*
       singleP LParenthesisClose)
    ]

cTypedefNameP :: PEParser CTypedefName
cTypedefNameP = parserToPEParser (CTypedefName <$> cIdentifierP)

cInitializerP :: PEParser CInitializer
cInitializerP =
  parserUnion
    [ CInitializerBracketListComma <$>
      (singleP LBraceOpen *> cInitializerListP <* singleP LComma <*
       singleP LBraceClose)
    , CInitializerBracketList <$>
      (singleP LBraceOpen *> cInitializerListP <* singleP LBraceClose)
    , CInitializerAssignment <$> cAssignmentExpressionP
    ]

cInitializerListP :: PEParser CInitializerList
cInitializerListP =
  parserToPEParser (CInitializerList <$> cInitializerP <*> cInitializerListP')

cInitializerListP' :: PEParser CInitializerList'
cInitializerListP' =
  parserUnion
    [ CInitializerList' <$> (singleP LComma *> cInitializerP) <*>
      cInitializerListP'
    , pure CInitializerList'Empty
    ]

cStatementP :: PEParser CStatement
cStatementP =
  parserUnion
    [ CStatementLabeled <$> cLabeledStatementP
    , CStatementCompound <$> cCompoundStatementP
    , CStatementExpression <$> cExpressionStatementP
    , CStatementSelection <$> cSelectionStatementP
    , CStatementIteration <$> cIterationStatementP
    , CStatementJump <$> cJumpStatementP
    ]

cLabeledStatementP :: PEParser CLabeledStatement
cLabeledStatementP =
  parserUnion
    [ CLabeledStatementDefault <$>
      (singleP LDefault *> singleP LColon *> cStatementP)
    , CLabeledStatementCase <$>
      (singleP LCase *> cConstantExpressionP <* singleP LColon) <*>
      cStatementP
    , CLabeledStatementId <$> cIdentifierP <*> (singleP LColon *> cStatementP)
    ]

cCompoundStatementP :: PEParser CCompoundStatement
cCompoundStatementP =
  parserToPEParser
    (CCompoundStatement <$> (singleP LBraceOpen *> cDeclarationListOptionalP) <*>
     (cStatementListOptionalP <* singleP LBraceClose))

cDeclarationListP :: PEParser CDeclarationList
cDeclarationListP =
  parserUnion
    [ CDeclarationList <$> cDeclarationP <*> cDeclarationListP
    , CDeclarationListSingleton <$> cDeclarationP
    ]

cStatementListP :: PEParser CStatementList
cStatementListP =
  parserUnion
    [ CStatementList <$> cStatementP <*> cStatementListP
    , CStatementListSingleton <$> cStatementP
    ]

cExpressionStatementP :: PEParser CExpressionStatement
cExpressionStatementP =
  parserToPEParser
    (CExpressionStatement <$> (cExpressionOptionalP <* singleP LSemiColon))

cSelectionStatementP :: PEParser CSelectionStatement
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

cIterationStatementP :: PEParser CIterationStatement
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

cJumpStatementP :: PEParser CJumpStatement
cJumpStatementP =
  parserUnion
    [ CJumpStatementGoto <$>
      (singleP LGoto *> cIdentifierP <* singleP LSemiColon)
    , CJumpStatementContinue <$ (singleP LContinue <* singleP LSemiColon)
    , CJumpStatementBreak <$ (singleP LBreak <* singleP LSemiColon)
    , CJumpStatementReturn <$>
      (singleP LReturn *> cExpressionOptionalP <* singleP LSemiColon)
    ]

cTranslationUnitP :: PEParser CTranslationUnit
cTranslationUnitP =
  parserUnion
    [ CTranslationUnitTranslation <$> cExternalDeclarationP <*>
      cTranslationUnitP
    , CTranslationUnitExternal <$> cExternalDeclarationP
    ]

cExternalDeclarationP :: PEParser CExternalDeclaration
cExternalDeclarationP =
  parserUnion
    [ CExternalDeclarationFunction <$> cFunctionDefinitionP
    , CExternalDeclaration <$> cDeclarationP
    ]

cFunctionDefinitionP :: PEParser CFunctionDefinition
cFunctionDefinitionP =
  parserUnion
    [ CFunctionDefinitionSpecifiers <$> cDeclarationSpecifiersOptionalP <*>
      cDeclaratorP
    , CFunctionDefinitionList <$> cDeclarationListOptionalP <*>
      cCompoundStatementP
    ]

cDeclarationListOptionalP :: PEParser CDeclarationListOptional
cDeclarationListOptionalP =
  parserUnion
    [ CDeclarationListOptional <$> cDeclarationListP
    , pure CDeclarationListOptionalEmpty
    ]

cDeclarationSpecifiersOptionalP :: PEParser CDeclarationSpecifiersOptional
cDeclarationSpecifiersOptionalP =
  parserUnion
    [ CDeclarationSpecifiersOptional <$> cDeclarationSpecifiersP
    , pure CDeclarationSpecifiersOptionalEmpty
    ]

cExpressionOptionalP :: PEParser CExpressionOptional
cExpressionOptionalP =
  parserUnion
    [CExpressionOptional <$> cExpressionP, pure CExpressionOptionalEmpty]

cStatementListOptionalP :: PEParser CStatementListOptional
cStatementListOptionalP =
  parserUnion
    [ CStatementListOptional <$> cStatementListP
    , pure CStatementListOptionalEmpty
    ]

cParameterTypeListOptionalP :: PEParser CParameterTypeListOptional
cParameterTypeListOptionalP =
  parserUnion
    [ CParameterTypeListOptional <$> cParameterTypeListP
    , pure CParameterTypeListOptionalEmpty
    ]

cConstantExpressionOptionalP :: PEParser CConstantExpressionOptional
cConstantExpressionOptionalP =
  parserUnion
    [ CConstantExpressionOptional <$> cConstantExpressionP
    , pure CConstantExpressionOptionalEmpty
    ]

cPointerOptionalP :: PEParser CPointerOptional
cPointerOptionalP =
  parserUnion [CPointerOptional <$> cPointerP, pure CPointerOptionalEmpty]

cAbstractDeclaratorOptionalP :: PEParser CAbstractDeclaratorOptional
cAbstractDeclaratorOptionalP =
  parserUnion
    [ CAbstractDeclaratorOptional <$> cAbstractDeclaratorP
    , pure CAbstractDeclaratorOptionalEmpty
    ]

cTypeQualifierListOptionalP :: PEParser CTypeQualifierListOptional
cTypeQualifierListOptionalP =
  parserUnion
    [ CTypeQualifierListOptional <$> cTypeQualifierListP
    , pure CTypeQualifierListOptionalEmpty
    ]

cIdentifierListOptionalP :: PEParser CIdentifierListOptional
cIdentifierListOptionalP =
  parserUnion
    [ CIdentifierListOptional <$> cIdentifierListP
    , pure CIdentifierListOptionalEmpty
    ]

cIdentifierOptionalP :: PEParser CIdentifierOptional
cIdentifierOptionalP =
  parserUnion
    [CIdentifierOptional <$> cIdentifierP, pure CIdentifierOptionalEmpty]

cDeclaratorOptionalP :: PEParser CDeclaratorOptional
cDeclaratorOptionalP =
  parserUnion
    [CDeclaratorOptional <$> cDeclaratorP, pure CDeclaratorOptionalEmpty]

cSpecifierQualifierListOptionalP :: PEParser CSpecifierQualifierListOptional
cSpecifierQualifierListOptionalP =
  parserUnion
    [ CSpecifierQualifierListOptional <$> cSpecifierQualifierListP
    , pure CSpecifierQualifierListOptionalEmpty
    ]

cInitDeclaratorListOptionalP :: PEParser CInitDeclaratorListOptional
cInitDeclaratorListOptionalP =
  parserUnion
    [ CInitDeclaratorListOptional <$> cInitDeclaratorListP
    , pure CInitDeclaratorListOptionalEmpty
    ]

cArgumentExpressionListOptionalP :: PEParser CArgumentExpressionListOptional
cArgumentExpressionListOptionalP =
  parserUnion
    [ CArgumentExpressionListOptional <$> cArgumentExpressionListP
    , pure CArgumentExpressionListOptionalEmpty
    ]
