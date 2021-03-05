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
    , CPostfixExpression'StructField <$>
      (singleP LDot *> cIdentifierP) <*> cPostfixExpressionP'
    , CPostfixExpression'StructPointer <$>
      (singleP LArrow *> cIdentifierP) <*> cPostfixExpressionP'
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
    , CUnaryExpressionSingleton <$> cPostfixExpressionP -- TODO: infinite recursion?
    ]

cUnaryOperatorP :: Parser CUnaryOperator
cUnaryOperatorP = undefined

cCastExpressionP :: Parser CCastExpression
cCastExpressionP = undefined

cMultiplicativeExpressionP :: Parser CMultiplicativeExpression
cMultiplicativeExpressionP = undefined

cMultiplicativeExpressionP' :: Parser CMultiplicativeExpression'
cMultiplicativeExpressionP' = undefined

cAdditiveExpressionP :: Parser CAdditiveExpression
cAdditiveExpressionP = undefined

cAdditiveExpressionP' :: Parser CAdditiveExpression'
cAdditiveExpressionP' = undefined

cShiftExpressionP :: Parser CShiftExpression
cShiftExpressionP = undefined

cShiftExpressionP' :: Parser CShiftExpression'
cShiftExpressionP' = undefined

cRelationalExpressionP :: Parser CRelationalExpression
cRelationalExpressionP = undefined

cRelationalExpressionP' :: Parser CRelationalExpression'
cRelationalExpressionP' = undefined

cEqualityExpressionP :: Parser CEqualityExpression
cEqualityExpressionP = undefined

cEqualityExpressionP' :: Parser CEqualityExpression'
cEqualityExpressionP' = undefined

cAndExpressionP :: Parser CAndExpression
cAndExpressionP = undefined

cAndExpressionP' :: Parser CAndExpression'
cAndExpressionP' = undefined

cExclusiveOrExpressionP :: Parser CExclusiveOrExpression
cExclusiveOrExpressionP = undefined

cExclusiveOrExpressionP' :: Parser CExclusiveOrExpression'
cExclusiveOrExpressionP' = undefined

cInclusiveOrExpressionP :: Parser CInclusiveOrExpression
cInclusiveOrExpressionP = undefined

cInclusiveOrExpressionP' :: Parser CInclusiveOrExpression'
cInclusiveOrExpressionP' = undefined

cLogicalAndExpressionP :: Parser CLogicalAndExpression
cLogicalAndExpressionP = undefined

cLogicalAndExpressionP' :: Parser CLogicalAndExpression'
cLogicalAndExpressionP' = undefined

cLogicalOrExpressionP :: Parser CLogicalOrExpression
cLogicalOrExpressionP = undefined

cLogicalOrExpressionP' :: Parser CLogicalOrExpression'
cLogicalOrExpressionP' = undefined

cConditionalExpressionP :: Parser CConditionalExpression
cConditionalExpressionP = undefined

cAssignmentExpressionP :: Parser CAssignmentExpression
cAssignmentExpressionP = undefined

cAssignmentOperatorP :: Parser CAssignmentOperator
cAssignmentOperatorP = undefined

cExpressionP :: Parser CExpression
cExpressionP = undefined

cConstantExpressionP :: Parser CConstantExpression
cConstantExpressionP = undefined

cDeclarationP :: Parser CDeclaration
cDeclarationP = undefined

cDeclarationSpecifiersP :: Parser CDeclarationSpecifiers
cDeclarationSpecifiersP = undefined

cInitDeclaratorListP :: Parser CInitDeclaratorList
cInitDeclaratorListP = undefined

cInitDeclaratorListP' :: Parser CInitDeclaratorList'
cInitDeclaratorListP' = undefined

cInitDeclaratorP :: Parser CInitDeclarator
cInitDeclaratorP = undefined

cStorageClassSpecifierP :: Parser CStorageClassSpecifier
cStorageClassSpecifierP = undefined

cTypeSpecifierP :: Parser CTypeSpecifier
cTypeSpecifierP = undefined

cStructOrUnionSpecifierP :: Parser CStructOrUnionSpecifier
cStructOrUnionSpecifierP = undefined

cStructOrUnionP :: Parser CStructOrUnion
cStructOrUnionP = undefined

cStructDeclarationListP :: Parser CStructDeclarationList
cStructDeclarationListP = undefined

cStructDeclarationP :: Parser CStructDeclaration
cStructDeclarationP = undefined

cSpecifierQualifierListP :: Parser CSpecifierQualifierList
cSpecifierQualifierListP = undefined

cStructDeclaratorListP :: Parser CStructDeclaratorList
cStructDeclaratorListP = undefined

cStructDeclaratorListP' :: Parser CStructDeclaratorList'
cStructDeclaratorListP' = undefined

cStructDeclaratorP :: Parser CStructDeclarator
cStructDeclaratorP = undefined

cEnumSpecifierP :: Parser CEnumSpecifier
cEnumSpecifierP = undefined

cEnumeratorListP :: Parser CEnumeratorList
cEnumeratorListP = undefined

cEnumeratorListP' :: Parser CEnumeratorList'
cEnumeratorListP' = undefined

cEnumerator :: Parser CEnumerator
cEnumerator = undefined

cTypeQualifierP :: Parser CTypeQualifier
cTypeQualifierP = undefined

cDeclaratorP :: Parser CDeclarator
cDeclaratorP = undefined

cDirectDeclaratorP :: Parser CDirectDeclarator
cDirectDeclaratorP = undefined

cDirectDeclaratorP' :: Parser CDirectDeclarator'
cDirectDeclaratorP' = undefined

cPointerP :: Parser CPointer
cPointerP = undefined

cTypeQualifierListP :: Parser CTypeQualifierList
cTypeQualifierListP = undefined

cParameterTypeListP :: Parser CParameterTypeList
cParameterTypeListP = undefined

cParameterListP :: Parser CParameterList
cParameterListP = undefined

cParameterListP' :: Parser CParameterList
cParameterListP' = undefined

cParameterDeclarationP :: Parser CParameterDeclaration
cParameterDeclarationP = undefined

cIdentifierListP :: Parser CIdentifierList
cIdentifierListP = undefined

cIdentifierListP' :: Parser CIdentifierList'
cIdentifierListP' = undefined

cTypeNameP :: Parser CTypeName
cTypeNameP = undefined

cAbstractDeclaratorP :: Parser CAbstractDeclarator
cAbstractDeclaratorP = undefined

cDirectAbstractDeclaratorP :: Parser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP = undefined

cTypedefNameP :: Parser CTypedefName
cTypedefNameP = undefined

cInitializerP :: Parser CInitializer
cInitializerP = undefined

cInitializerListP :: Parser CInitializerList
cInitializerListP = undefined

cInitializerListP' :: Parser CInitializerList'
cInitializerListP' = undefined

cStatementP :: Parser CStatement
cStatementP = undefined

cLabeledStatementP :: Parser CLabeledStatement
cLabeledStatementP = undefined

cCompoundStatementP :: Parser CCompoundStatement
cCompoundStatementP = undefined

cDeclarationList :: Parser CDeclarationList
cDeclarationList = undefined

cStatementListP :: Parser CStatementList
cStatementListP = undefined

cExpressionStatementP :: Parser CExpressionStatement
cExpressionStatementP = undefined

cSelectionStatementP :: Parser CSelectionStatement
cSelectionStatementP = undefined

cIterationStatementP :: Parser CIterationStatement
cIterationStatementP = undefined

cJumpStatementP :: Parser CJumpStatement
cJumpStatementP = undefined

cTranslationUnitP :: Parser CTranslationUnit
cTranslationUnitP = undefined

cExternalDeclarationP :: Parser CExternalDeclaration
cExternalDeclarationP = undefined

cFunctionDefinitionP :: Parser CFunctionDefinition
cFunctionDefinitionP = undefined

