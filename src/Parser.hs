{-# LANGUAGE LambdaCase #-}

{-
strategy : each parser tries each possible option
           and returns a nonempty list of possible parse trees
           or ParseError
-}
module Parser where

import Control.Applicative

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
  ParseError Coordinates String
  deriving (Eq, Show)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  p1 <*> p2 =
    Parser $ \input -> do
      (input', fab) <- runParser p1 input
      (input'', a) <- runParser p2 input'
      return (input'', fab a)

instance Alternative Parser where
  empty = Parser $ \_ -> Left $ ParseError (0, 0) "Error"
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      r@(Right _) -> r
      e1@(Left (ParseError c1 _)) ->
        case runParser p2 input of
          r@(Right _) -> r
          e2@(Left (ParseError c2 _)) ->
            if c1 >= c2 then e1 else e2



parseCCode :: Input -> Either ParseError (ParseElement CTranslationUnit)
parseCCode input = do
  (_, result) <- runParser cParser input
  return result

cParser :: PEParser CTranslationUnit
cParser =
  cTranslationUnitP <* singleP LEndMarker

-----------
-- UTILS --
-----------

parserToPEParser :: Parser a -> PEParser a
parserToPEParser p =
  Parser $ \case
    [] -> Left unexpectedEof
    input@((ScanElement c _):_) -> do
      (input', a) <- runParser p input
      return (input', ParseElement c a)

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
        then Right (rest, l)
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: PEParser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LIntLiteral x)):rest) -> Right (rest, ParseElement c x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: PEParser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LFloatLiteral x)):rest) -> Right (rest, ParseElement c x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: PEParser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LCharLiteral x)):rest) -> Right (rest, ParseElement c x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: PEParser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LStringLiteral x)):rest) -> Right (rest, ParseElement c x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: PEParser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LLabel x)):rest) -> Right (rest, ParseElement c x)
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l

optionalParser :: PEParser a -> (ParseElement a -> b) -> b -> PEParser b
optionalParser p nonEmpty blank =
  parserToPEParser $ nonEmpty <$> p <|> pure blank

parenthesisP :: Parser a -> Parser a
parenthesisP p = singleP LParenthesisOpen *> p <* singleP LParenthesisClose

bracketP :: Parser a -> Parser a
bracketP p = singleP LBracketOpen *> p <* singleP LBracketClose

braceP :: Parser a -> Parser a
braceP p = singleP LBraceOpen *> p <* singleP LBraceClose

---------------
-- C PARSERS --
---------------

cIdentifierP :: PEParser CIdentifier
cIdentifierP =
  parserToPEParser $ CIdentifier <$> labelP

cIdentifierOptionalP :: PEParser CIdentifierOptional
cIdentifierOptionalP =
  optionalParser cIdentifierP CIdentifierOptional CIdentifierOptionalEmpty

cTranslationUnitP :: PEParser CTranslationUnit
cTranslationUnitP =
  parserToPEParser $
  liftA2 CTranslationUnit cExternalDeclarationP cTranslationUnitOptionalP

cTranslationUnitOptionalP :: PEParser CTranslationUnitOptional
cTranslationUnitOptionalP =
  optionalParser
    cTranslationUnitP
    CTranslationUnitOptional
    CTranslationUnitOptionalEmpty

cExternalDeclarationP :: PEParser CExternalDeclaration
cExternalDeclarationP =
  parserToPEParser $
  CExternalDeclarationFunction <$> cFunctionDefinitionP <|>
  CExternalDeclaration <$> cDeclarationP

cFunctionDefinitionP :: PEParser CFunctionDefinition
cFunctionDefinitionP =
  parserToPEParser $
  CFunctionDefinition <$>
  cFunctionSpecifiersP <*>
  cDeclaratorP <*>
  cDeclarationListOptionalP <*>
  cCompoundStatementP

-- interpret LLabel before open parenthesis as function name
-- rather than typedef name
cFunctionSpecifiersP :: PEParser CDeclarationSpecifiersOptional
cFunctionSpecifiersP =
  parserToPEParser $
  Parser $ \case
    input@((ScanElement _ (LLabel _)):(ScanElement _ LParenthesisOpen):_) ->
      Right (input, CDeclarationSpecifiersOptionalEmpty)
    input ->
      case runParser specifiersP input of
        Left _ -> Right (input, CDeclarationSpecifiersOptionalEmpty)
        r -> fmap CDeclarationSpecifiersOptional <$> r
    where
      specifiersP =
        parserToPEParser $ storageClassP <|> typeSpecifierP <|> typeQualifierP
      storageClassP =
        liftA2
          CDeclarationSpecifiersStorageClass
          cStorageClassSpecifierP
          cFunctionSpecifiersP
      typeSpecifierP =
        liftA2
          CDeclarationSpecifiersTypeSpecifier
          cTypeSpecifierP
          cFunctionSpecifiersP
      typeQualifierP =
        liftA2
          CDeclarationSpecifiersTypeQualifier
          cTypeQualifierP
          cFunctionSpecifiersP

cDeclarationP :: PEParser CDeclaration
cDeclarationP =
  parserToPEParser $
  CDeclaration <$>
  cDeclarationSpecifiersP <*>
  cInitDeclaratorListOptionalP <*
  singleP LSemiColon

cDeclarationListP :: PEParser CDeclarationList
cDeclarationListP =
  parserToPEParser $
  liftA2 CDeclarationList cDeclarationP cDeclarationListOptionalP

cDeclarationListOptionalP :: PEParser CDeclarationListOptional
cDeclarationListOptionalP =
  optionalParser
    cDeclarationListP
    CDeclarationListOptional
    CDeclarationListOptionalEmpty

cDeclarationSpecifiersP :: PEParser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserToPEParser $
  liftA2
    CDeclarationSpecifiersStorageClass
    cStorageClassSpecifierP
    cDeclarationSpecifiersOptionalP <|>
  liftA2
    CDeclarationSpecifiersTypeSpecifier
    cTypeSpecifierP
    cDeclarationSpecifiersOptionalP <|>
  liftA2
    CDeclarationSpecifiersTypeQualifier
    cTypeQualifierP
    cDeclarationSpecifiersOptionalP

cDeclarationSpecifiersOptionalP :: PEParser CDeclarationSpecifiersOptional
cDeclarationSpecifiersOptionalP =
  optionalParser
    cDeclarationSpecifiersP
    CDeclarationSpecifiersOptional
    CDeclarationSpecifiersOptionalEmpty

cStorageClassSpecifierP :: PEParser CStorageClassSpecifier
cStorageClassSpecifierP =
  parserToPEParser $
  CStorageClassSpecifierAuto <$ singleP LAuto <|>
  CStorageClassSpecifierRegister <$ singleP LRegister <|>
  CStorageClassSpecifierStatic <$ singleP LStatic <|>
  CStorageClassSpecifierExtern <$ singleP LExtern <|>
  CStorageClassSpecifierTypedef <$ singleP LTypedef

cTypeSpecifierP :: PEParser CTypeSpecifier
cTypeSpecifierP =
  parserToPEParser $
  CTypeSpecifierVoid <$ singleP LVoid <|>
  CTypeSpecifierChar <$ singleP LChar <|>
  CTypeSpecifierShort <$ singleP LShort <|>
  CTypeSpecifierInt <$ singleP LInt <|>
  CTypeSpecifierLong <$ singleP LLong <|>
  CTypeSpecifierFloat <$ singleP LFloat <|>
  CTypeSpecifierDouble <$ singleP LDouble <|>
  CTypeSpecifierSigned <$ singleP LSigned <|>
  CTypeSpecifierUnsigned <$ singleP LUnsigned <|>
  CTypeSpecifierStructOrUnion <$> cStructOrUnionSpecifierP <|>
  CTypeSpecifierEnum <$> cEnumSpecifierP <|>
  CTypeSpecifierTypedef <$> cTypedefNameP

cTypeQualifierP :: PEParser CTypeQualifier
cTypeQualifierP =
  parserToPEParser $
  CTypeQualifierConst <$ singleP LConst <|>
  CTypeQualifierVolatile <$ singleP LVolatile

cStructOrUnionSpecifierP :: PEParser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserToPEParser $
  liftA3
    CStructOrUnionSpecifierList
    cStructOrUnionP
    cIdentifierOptionalP
    (braceP cStructDeclarationListP) <|>
  liftA2
    CStructOrUnionSpecifier
    cStructOrUnionP
    cIdentifierP

cStructOrUnionP :: PEParser CStructOrUnion
cStructOrUnionP =
  parserToPEParser $
  CStructOrUnionStruct <$ singleP LStruct <|>
  CStructOrUnionUnion <$ singleP LUnion

cStructDeclarationListP :: PEParser CStructDeclarationList
cStructDeclarationListP =
  parserToPEParser $
  liftA2
    CStructDeclarationList
    cStructDeclarationP
    cStructDeclarationListOptionalP

cStructDeclarationListOptionalP :: PEParser CStructDeclarationListOptional
cStructDeclarationListOptionalP =
  optionalParser
    cStructDeclarationListP
    CStructDeclarationListOptional
    CStructDeclarationListOptionalEmpty

cInitDeclaratorListP :: PEParser CInitDeclaratorList
cInitDeclaratorListP =
  parserToPEParser $
  liftA2
    CInitDeclaratorList
    cInitDeclaratorP
    cInitDeclaratorListP'

cInitDeclaratorListOptionalP :: PEParser CInitDeclaratorListOptional
cInitDeclaratorListOptionalP =
  optionalParser
    cInitDeclaratorListP
    CInitDeclaratorListOptional
    CInitDeclaratorListOptionalEmpty

cInitDeclaratorListP' :: PEParser CInitDeclaratorList'
cInitDeclaratorListP' =
  parserToPEParser $
  liftA2
    CInitDeclaratorList'
    (singleP LComma *> cInitDeclaratorP)
    cInitDeclaratorListP' <|>
  pure
    CInitDeclaratorList'Empty

cInitDeclaratorP :: PEParser CInitDeclarator
cInitDeclaratorP =
  parserToPEParser $
  liftA2
    CInitDeclarator
    cDeclaratorP
    cAssignInitializerOptionalP

cAssignInitializerOptionalP :: PEParser CAssignInitializerOptional
cAssignInitializerOptionalP =
  parserToPEParser $
  CAssignInitializerOptional <$> (singleP LAssign *> cInitializerP) <|>
  pure CAssignInitializerOptionalEmpty

cStructDeclarationP :: PEParser CStructDeclaration
cStructDeclarationP =
  parserToPEParser $
  liftA2
    CStructDeclaration
    cSpecifierQualifierListP
    cStructDeclaratorListP <* singleP LSemiColon

cSpecifierQualifierListP :: PEParser CSpecifierQualifierList
cSpecifierQualifierListP =
  parserToPEParser $
  liftA2
    CSpecifierQualifierListSpecifier
    cTypeSpecifierP
    cSpecifierQualifierListOptionalP <|>
  liftA2
    CSpecifierQualifierListQualifier
    cTypeQualifierP
    cSpecifierQualifierListOptionalP

cSpecifierQualifierListOptionalP :: PEParser CSpecifierQualifierListOptional
cSpecifierQualifierListOptionalP =
  optionalParser
    cSpecifierQualifierListP
    CSpecifierQualifierListOptional
    CSpecifierQualifierListOptionalEmpty

cStructDeclaratorListP :: PEParser CStructDeclaratorList
cStructDeclaratorListP =
  parserToPEParser $
  liftA2
    CStructDeclaratorList
    cStructDeclaratorP
    cStructDeclaratorListP'

cStructDeclaratorListP' :: PEParser CStructDeclaratorList'
cStructDeclaratorListP' =
  parserToPEParser $
  liftA2
    CStructDeclaratorList'
    (singleP LComma *> cStructDeclaratorP)
    cStructDeclaratorListP' <|>
  pure
    CStructDeclaratorList'Empty

cStructDeclaratorP :: PEParser CStructDeclarator
cStructDeclaratorP =
  parserToPEParser $
  liftA2
    CStructDeclaratorField
    cDeclaratorOptionalP
    (singleP LColon *> cConstantExpressionP) <|>
  CStructDeclarator <$> cDeclaratorP

cEnumSpecifierP :: PEParser CEnumSpecifier
cEnumSpecifierP =
  parserToPEParser $
  liftA2
    CEnumSpecifierList
    (singleP LEnum *> cIdentifierOptionalP)
    (braceP cEnumeratorListP) <|>
  CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)

cEnumeratorListP :: PEParser CEnumeratorList
cEnumeratorListP =
  parserToPEParser $ liftA2 CEnumeratorList cEnumeratorP cEnumeratorListP'

cEnumeratorListP' :: PEParser CEnumeratorList'
cEnumeratorListP' =
  parserToPEParser $
  liftA2
    CEnumeratorList'
    (singleP LComma *> cEnumeratorP)
    cEnumeratorListP' <|>
  pure
    CEnumeratorList'Empty

cEnumeratorP :: PEParser CEnumerator
cEnumeratorP =
  parserToPEParser $
  liftA2
    CEnumeratorAssign
    cIdentifierP
    (singleP LAssign *> cConstantExpressionP) <|>
  liftA
    CEnumerator
    cIdentifierP

cDeclaratorP :: PEParser CDeclarator
cDeclaratorP =
  parserToPEParser $ liftA2 CDeclarator cPointerOptionalP cDirectDeclaratorP

cDeclaratorOptionalP :: PEParser CDeclaratorOptional
cDeclaratorOptionalP =
  optionalParser
    cDeclaratorP
    CDeclaratorOptional
    CDeclaratorOptionalEmpty

cDirectDeclaratorP :: PEParser CDirectDeclarator
cDirectDeclaratorP =
  parserToPEParser $
  liftA2
    CDirectDeclaratorId
    cIdentifierP
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclaratorParen
    (parenthesisP cDeclaratorP)
    cDirectDeclaratorP'

cDirectDeclaratorP' :: PEParser CDirectDeclarator'
cDirectDeclaratorP' =
  parserToPEParser $
  liftA2
    CDirectDeclarator'ConstExpr
    (bracketP cConstantExpressionOptionalP)
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclarator'ParamTypeList
    (parenthesisP cParameterTypeListP)
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclarator'IdList
    (parenthesisP cIdentifierListOptionalP)
    cDirectDeclaratorP' <|>
  pure
    CDirectDeclarator'Empty

cPointerP :: PEParser CPointer
cPointerP =
  parserToPEParser $
  liftA2
    CPointer
    (singleP LStar *> cTypeQualifierListOptionalP)
    cPointerOptionalP

cPointerOptionalP :: PEParser CPointerOptional
cPointerOptionalP =
  optionalParser cPointerP CPointerOptional CPointerOptionalEmpty

cTypeQualifierListP :: PEParser CTypeQualifierList
cTypeQualifierListP =
  parserToPEParser $
  liftA2
    CTypeQualifierList
    cTypeQualifierP
    cTypeQualifierListOptionalP

cTypeQualifierListOptionalP :: PEParser CTypeQualifierListOptional
cTypeQualifierListOptionalP =
  optionalParser
    cTypeQualifierListP
    CTypeQualifierListOptional
    CTypeQualifierListOptionalEmpty

cParameterTypeListP :: PEParser CParameterTypeList
cParameterTypeListP =
  parserToPEParser $
  liftA2
    CParameterTypeList
    cParameterListP
    cVarArgsOptionalP

cParameterTypeListOptionalP :: PEParser CParameterTypeListOptional
cParameterTypeListOptionalP =
  optionalParser
    cParameterTypeListP
    CParameterTypeListOptional
    CParameterTypeListOptionalEmpty

cVarArgsOptionalP :: PEParser CVarArgsOptional
cVarArgsOptionalP =
  parserToPEParser $
  CVarArgsOptional <$ singleP LComma <* singleP LVarargs <|>
  pure CVarArgsOptionalEmpty

cParameterListP :: PEParser CParameterList
cParameterListP =
  parserToPEParser $
  liftA2
    CParameterList
    cParameterDeclarationP
    cParameterListP'

cParameterListP' :: PEParser CParameterList'
cParameterListP' =
  parserToPEParser $
  liftA2
    CParameterList'
    (singleP LComma *> cParameterDeclarationP)
    cParameterListP' <|>
  pure
    CParameterList'Empty

cParameterDeclarationP :: PEParser CParameterDeclaration
cParameterDeclarationP =
  parserToPEParser $
  liftA2
    CParameterDeclaration
    cDeclarationSpecifiersP
    cParameterDeclarationP'

cParameterDeclarationP' :: PEParser CParameterDeclaration'
cParameterDeclarationP' =
  parserToPEParser $
  CParameterDeclaration' <$> cDeclaratorP <|>
  CParameterDeclaration'Abstract <$> cAbstractDeclaratorOptionalP

cIdentifierListP :: PEParser CIdentifierList
cIdentifierListP =
  parserToPEParser $ liftA2 CIdentifierList cIdentifierP cIdentifierListP'

cIdentifierListOptionalP :: PEParser CIdentifierListOptional
cIdentifierListOptionalP =
  optionalParser
    cIdentifierListP
    CIdentifierListOptional
    CIdentifierListOptionalEmpty

cIdentifierListP' :: PEParser CIdentifierList'
cIdentifierListP' =
  parserToPEParser $
  liftA2
    CIdentifierList'
    (singleP LComma *> cIdentifierP)
    cIdentifierListP' <|>
  pure
    CIdentifierList'Empty

cInitializerP :: PEParser CInitializer
cInitializerP =
  parserToPEParser $
  CInitializerAssignment <$> cAssignmentExpressionP <|>
  CInitializerInitList <$>
  bracketP (cInitializerListP <* (singleP LComma <|> pure LComma))

cInitializerListP :: PEParser CInitializerList
cInitializerListP =
  parserToPEParser $
  liftA2
    CInitializerList
    cInitializerP
    cInitializerListP'

cInitializerListP' :: PEParser CInitializerList'
cInitializerListP' =
  parserToPEParser $
  liftA2
    CInitializerList'
    (singleP LComma *> cInitializerP)
    cInitializerListP' <|>
  pure
    CInitializerList'Empty

cTypeNameP :: PEParser CTypeName
cTypeNameP =
  parserToPEParser $
  liftA2
    CTypeName
    cSpecifierQualifierListP
    cAbstractDeclaratorOptionalP

cAbstractDeclaratorP :: PEParser CAbstractDeclarator
cAbstractDeclaratorP =
  parserToPEParser $
  liftA2
    CAbstractDeclaratorDirect
    cPointerOptionalP
    cDirectAbstractDeclaratorP <|>
  CAbstractDeclaratorPointer <$> cPointerP

cAbstractDeclaratorOptionalP :: PEParser CAbstractDeclaratorOptional
cAbstractDeclaratorOptionalP =
  optionalParser
    cAbstractDeclaratorP
    CAbstractDeclaratorOptional
    CAbstractDeclaratorOptionalEmpty

cDirectAbstractDeclaratorP :: PEParser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP =
  parserToPEParser $
  liftA2
    CDirectAbstractDeclarator
    (parenthesisP cAbstractDeclaratorP)
    cDirectAbstractDeclaratorP'

cDirectAbstractDeclaratorP' :: PEParser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parserToPEParser $
  liftA2
    CDirectAbstractDeclarator'Const
    (bracketP cConstantExpressionOptionalP)
    cDirectAbstractDeclaratorP' <|>
  liftA2
    CDirectAbstractDeclarator'Params
    (parenthesisP cParameterTypeListOptionalP)
    cDirectAbstractDeclaratorP' <|>
  pure
    CDirectAbstractDeclarator'Empty

cTypedefNameP :: PEParser CTypedefName
cTypedefNameP =
  parserToPEParser $ CTypedefName <$> cIdentifierP

cStatementP :: PEParser CStatement
cStatementP =
  parserToPEParser $
  CStatementLabeled <$> cLabeledStatementP <|>
  CStatementExpression <$> cExpressionStatementP <|>
  CStatementCompound <$> cCompoundStatementP <|>
  CStatementSelection <$> cSelectionStatementP <|>
  CStatementIteration <$> cIterationStatementP <|>
  CStatementJump <$> cJumpStatementP

cLabeledStatementP :: PEParser CLabeledStatement
cLabeledStatementP =
  parserToPEParser $
  liftA2
    CLabeledStatementId
    cIdentifierP
    (singleP LColon *> cStatementP) <|>
  liftA2
    CLabeledStatementCase
    (singleP LCase *> cConstantExpressionP)
    (singleP LColon *> cStatementP) <|>
  liftA
    CLabeledStatementDefault
    (singleP LDefault *> singleP LColon *> cStatementP)

cExpressionStatementP :: PEParser CExpressionStatement
cExpressionStatementP =
  parserToPEParser $
  CExpressionStatement <$>
  cExpressionOptionalP <*
  singleP LSemiColon

cCompoundStatementP :: PEParser CCompoundStatement
cCompoundStatementP =
  braceP $
  parserToPEParser $
  liftA2
    CCompoundStatement
    cDeclarationListOptionalP
    cStatementListOptionalP

cStatementListP :: PEParser CStatementList
cStatementListP =
  parserToPEParser $
  liftA2
    CStatementList
    cStatementP
    cStatementListOptionalP

cStatementListOptionalP :: PEParser CStatementListOptional
cStatementListOptionalP =
  optionalParser
    cStatementListP
    CStatementListOptional
    CStatementListOptionalEmpty

cSelectionStatementP :: PEParser CSelectionStatement
cSelectionStatementP =
  parserToPEParser $
  liftA3
    CSelectionStatementIf
    (singleP LIf *> parenthesisP cExpressionP)
    cStatementP
    cElseOptionalP <|>
  liftA2
    CSelectionStatementSwitch
    (singleP LSwitch *> parenthesisP cExpressionP)
    cStatementP

cElseOptionalP :: PEParser CElseOptional
cElseOptionalP =
  parserToPEParser $
  CElseOptional <$> (singleP LElse *> cStatementP) <|>
  pure CElseOptionalEmpty

cIterationStatementP :: PEParser CIterationStatement
cIterationStatementP =
  parserToPEParser $
  liftA2
    CIterationStatementWhile
    (singleP LWhile *> parenthesisP cExpressionP)
    cStatementP <|>
  liftA2
    CIterationStatementDoWhile
    (singleP LDo *> cStatementP)
    (singleP LWhile *> parenthesisP cExpressionP <* singleP LSemiColon) <|>
  CIterationStatementFor <$>
  (singleP LFor *> singleP LParenthesisOpen *> cExpressionOptionalP) <*>
  (singleP LSemiColon *> cExpressionOptionalP) <*>
  (singleP LSemiColon *> cExpressionOptionalP <* singleP LParenthesisClose) <*>
  cStatementP

cJumpStatementP :: PEParser CJumpStatement
cJumpStatementP =
  parserToPEParser $
  CJumpStatementGoto <$> (singleP LGoto *> cIdentifierP <* singleP LSemiColon) <|>
  CJumpStatementContinue <$ singleP LContinue <* singleP LSemiColon <|>
  CJumpStatementBreak <$ singleP LBreak <* singleP LSemiColon <|>
  CJumpStatementReturn <$> (singleP LReturn *> cExpressionOptionalP <* singleP LSemiColon)

cExpressionP :: PEParser CExpression
cExpressionP =
  parserToPEParser $
  liftA2
    CExpression
    cAssignmentExpressionP
    cExpressionP'

cExpressionOptionalP :: PEParser CExpressionOptional
cExpressionOptionalP =
  optionalParser cExpressionP CExpressionOptional CExpressionOptionalEmpty

cExpressionP' :: PEParser CExpression'
cExpressionP' =
  parserToPEParser $
  liftA2
    CExpression'
    (singleP LComma *> cAssignmentExpressionP)
    cExpressionP' <|>
  pure
    CExpression'Empty

cAssignmentExpressionP :: PEParser CAssignmentExpression
cAssignmentExpressionP =
  parserToPEParser $
  liftA3
    CAssignmentExpression
    cUnaryExpressionP
    cAssignmentOperatorP
    cAssignmentExpressionP <|>
  liftA
    CAssignmentExpressionConditional
    cConditionalExpressionP

cAssignmentOperatorP :: PEParser CAssignmentOperator
cAssignmentOperatorP =
  parserToPEParser $
  CAssignmentOperatorAssign <$ singleP LAssign <|>
  CAssignmentOperatorMul <$ singleP LMultiplicationAssign <|>
  CAssignmentOperatorDiv <$ singleP LDivisionAssign <|>
  CAssignmentOperatorMod <$ singleP LModuloAssign <|>
  CAssignmentOperatorAdd <$ singleP LPlusAssign <|>
  CAssignmentOperatorSub <$ singleP LMinusAssign <|>
  CAssignmentOperatorLShift <$ singleP LBitShiftLeftAssign <|>
  CAssignmentOperatorRShfit <$ singleP LBitShiftRightAssign <|>
  CAssignmentOperatorAnd <$ singleP LBitwiseAndAssign <|>
  CAssignmentOperatorXor <$ singleP LBitwiseXorAssign <|>
  CAssignmentOperatorOr <$ singleP LBitwiseOrAssign

cConditionalExpressionP :: PEParser CConditionalExpression
cConditionalExpressionP =
  parserToPEParser $
  liftA2
    CConditionalExpression
    cLogicalOrExpressionP
    cTernaryOptionalP

cTernaryOptionalP :: PEParser CTernaryOptional
cTernaryOptionalP =
  parserToPEParser $
  liftA2
    CTernaryOptional
    (singleP LTernary *> cExpressionP)
    (singleP LColon *> cConditionalExpressionP) <|>
  pure
    CTernaryOptionalEmpty

cConstantExpressionP :: PEParser CConstantExpression
cConstantExpressionP =
  parserToPEParser $ CConstantExpression <$> cConditionalExpressionP

cConstantExpressionOptionalP :: PEParser CConstantExpressionOptional
cConstantExpressionOptionalP =
  optionalParser
    cConstantExpressionP
    CConstantExpressionOptional
    CConstantExpressionOptionalEmpty

cLogicalOrExpressionP :: PEParser CLogicalOrExpression
cLogicalOrExpressionP =
  parserToPEParser $
  liftA2
    CLogicalOrExpression
    cLogicalAndExpressionP
    cLogicalOrExpressionP'

cLogicalOrExpressionP' :: PEParser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parserToPEParser $
  liftA2
    CLogicalOrExpression'
    (singleP LOr *> cLogicalAndExpressionP)
    cLogicalOrExpressionP' <|>
  pure
    CLogicalOrExpression'Empty

cLogicalAndExpressionP :: PEParser CLogicalAndExpression
cLogicalAndExpressionP =
  parserToPEParser $
  liftA2
    CLogicalAndExpression
    cInclusiveOrExpressionP
    cLogicalAndExpressionP'

cLogicalAndExpressionP' :: PEParser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parserToPEParser $
  liftA2
    CLogicalAndExpression'
    (singleP LAnd *> cInclusiveOrExpressionP)
    cLogicalAndExpressionP' <|>
  pure
    CLogicalAndExpression'Empty

cInclusiveOrExpressionP :: PEParser CInclusiveOrExpression
cInclusiveOrExpressionP =
  parserToPEParser $
  liftA2
    CInclusiveOrExpression
    cExclusiveOrExpressionP
    cInclusiveOrExpressionP'

cInclusiveOrExpressionP' :: PEParser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parserToPEParser $
  liftA2
    CInclusiveOrExpression'
    (singleP LBitwiseOr *> cExclusiveOrExpressionP)
    cInclusiveOrExpressionP' <|>
  pure
    CInclusiveOrExpression'Empty

cExclusiveOrExpressionP :: PEParser CExclusiveOrExpression
cExclusiveOrExpressionP =
  parserToPEParser $
  liftA2
    CExclusiveOrExpression
    cAndExpressionP
    cExclusiveOrExpressionP'

cExclusiveOrExpressionP' :: PEParser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parserToPEParser $
  liftA2
    CExclusiveOrExpression'
    (singleP LBitwiseXor *> cAndExpressionP)
    cExclusiveOrExpressionP' <|>
  pure
    CExclusiveOrExpression'Empty

cAndExpressionP :: PEParser CAndExpression
cAndExpressionP =
  parserToPEParser $
  liftA2
    CAndExpression
    cEqualityExpressionP
    cAndExpressionP'

cAndExpressionP' :: PEParser CAndExpression'
cAndExpressionP' =
  parserToPEParser $
  liftA2
    CAndExpression'
    (singleP LAmp *> cEqualityExpressionP)
    cAndExpressionP' <|>
  pure
    CAndExpression'Empty

cEqualityExpressionP :: PEParser CEqualityExpression
cEqualityExpressionP =
  parserToPEParser $
  liftA2
    CEqualityExpression
    cRelationalExpressionP
    cEqualityExpressionP'

cEqualityExpressionP' :: PEParser CEqualityExpression'
cEqualityExpressionP' =
  parserToPEParser $
  liftA2
    CEqualityExpression'EQ
    (singleP LEquals *> cRelationalExpressionP)
    cEqualityExpressionP' <|>
  liftA2
    CEqualityExpression'NEQ
    (singleP LNotEquals *> cRelationalExpressionP)
    cEqualityExpressionP' <|>
  pure
    CEqualityExpression'Empty

cRelationalExpressionP :: PEParser CRelationalExpression
cRelationalExpressionP =
  parserToPEParser $
  liftA2
    CRelationalExpression
    cShiftExpressionP
    cRelationalExpressionP'

cRelationalExpressionP' :: PEParser CRelationalExpression'
cRelationalExpressionP' =
  parserToPEParser $
  liftA2
    CRelationalExpression'LT
    (singleP LLT *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  liftA2
    CRelationalExpression'LTE
    (singleP LLTE *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  liftA2
    CRelationalExpression'GT
    (singleP LGT *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  liftA2
    CRelationalExpression'GT
    (singleP LGTE *> cShiftExpressionP)
    cRelationalExpressionP' <|>
  pure
    CRelationalExpression'Empty

cShiftExpressionP :: PEParser CShiftExpression
cShiftExpressionP =
  parserToPEParser $
  liftA2
    CShiftExpression
    cAdditiveExpressionP
    cShiftExpressionP'

cShiftExpressionP' :: PEParser CShiftExpression'
cShiftExpressionP' =
  parserToPEParser $
  liftA2
    CShiftExpression'Left
    (singleP LBitShiftLeft *> cAdditiveExpressionP)
    cShiftExpressionP' <|>
  liftA2
    CShiftExpression'Right
    (singleP LBitShiftRight *> cAdditiveExpressionP)
    cShiftExpressionP' <|>
  pure
    CShiftExpression'Empty

cAdditiveExpressionP :: PEParser CAdditiveExpression
cAdditiveExpressionP =
  parserToPEParser $
  liftA2
    CAdditiveExpression
    cMultiplicativeExpressionP
    cAdditiveExpressionP'

cAdditiveExpressionP' :: PEParser CAdditiveExpression'
cAdditiveExpressionP' =
  parserToPEParser $
  liftA2
    CAdditiveExpression'Add
    (singleP LPlus *> cMultiplicativeExpressionP)
    cAdditiveExpressionP' <|>
  liftA2
    CAdditiveExpression'Sub
    (singleP LMinus *> cMultiplicativeExpressionP)
    cAdditiveExpressionP' <|>
  pure
    CAdditiveExpression'Empty

cMultiplicativeExpressionP :: PEParser CMultiplicativeExpression
cMultiplicativeExpressionP =
  parserToPEParser $
  liftA2
    CMultiplicativeExpression
    cCastExpressionP
    cMultiplicativeExpressionP'

cMultiplicativeExpressionP' :: PEParser CMultiplicativeExpression'
cMultiplicativeExpressionP' =
  parserToPEParser $
  liftA2
    CMultiplicativeExpression'Mul
    (singleP LStar *> cCastExpressionP)
    cMultiplicativeExpressionP' <|>
  liftA2
    CMultiplicativeExpression'Div
    (singleP LDivision *> cCastExpressionP)
    cMultiplicativeExpressionP' <|>
  liftA2
    CMultiplicativeExpression'Mod
    (singleP LModulo *> cCastExpressionP)
    cMultiplicativeExpressionP' <|>
  pure
    CMultiplicativeExpression'Empty

cCastExpressionP :: PEParser CCastExpression
cCastExpressionP =
  parserToPEParser $
  liftA2
    CCastExpression
    (parenthesisP cTypeNameP)
    cCastExpressionP <|>
  liftA
    CCastExpressionUnary
    cUnaryExpressionP

cUnaryExpressionP :: PEParser CUnaryExpression
cUnaryExpressionP =
  parserToPEParser $
  CUnaryExpressionInc <$> (singleP LIncrement *> cUnaryExpressionP) <|>
  CUnaryExpressionDec <$> (singleP LDecrement *> cUnaryExpressionP) <|>
  CUnaryExpressionSizeofType <$> (singleP LSizeof *> parenthesisP cTypeNameP) <|>
  CUnaryExpressionSizeof <$> (singleP LSizeof *> cUnaryExpressionP) <|>
  CUnaryExpressionUnaryOp <$> cUnaryOperatorP <*> cCastExpressionP <|>
  CUnaryExpressionPostfix <$> cPostfixExpressionP

cUnaryOperatorP :: PEParser CUnaryOperator
cUnaryOperatorP =
  parserToPEParser $
  CUnaryOperatorAnd <$ singleP LAmp <|>
  CUnaryOperatorMul <$ singleP LStar <|>
  CUnaryOperatorAdd <$ singleP LPlus <|>
  CUnaryOperatorSub <$ singleP LMinus <|>
  CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot <|>
  CUnaryOperatorNot <$ singleP LNot

cPostfixExpressionP :: PEParser CPostfixExpression
cPostfixExpressionP =
  parserToPEParser $
  liftA2
    CPostfixExpression
    cPrimaryExpressionP
    cPostfixExpressionP'

cPostfixExpressionP' :: PEParser CPostfixExpression'
cPostfixExpressionP' =
  parserToPEParser $
  liftA2
    CPostfixExpression'Bracket
    (bracketP cExpressionP)
    cPostfixExpressionP' <|>
  liftA2
    CPostfixExpression'Paren
    (parenthesisP cArgumentExpressionListOptionalP)
    cPostfixExpressionP' <|>
  liftA2
    CPostfixExpression'Dot
    (singleP LDot *> cIdentifierP)
    cPostfixExpressionP' <|>
  liftA2
    CPostfixExpression'Arrow
    (singleP LArrow *> cIdentifierP)
    cPostfixExpressionP' <|>
  liftA
    CPostfixExpression'Inc
    (singleP LIncrement *> cPostfixExpressionP') <|>
  liftA
    CPostfixExpression'Dec
    (singleP LDecrement *> cPostfixExpressionP') <|>
  pure
    CPostfixExpression'Empty

cPrimaryExpressionP :: PEParser CPrimaryExpression
cPrimaryExpressionP =
  parserToPEParser $
  CPrimaryExpressionId <$> cIdentifierP <|>
  CPrimaryExpressionConst <$> cConstantP <|>
  CPrimaryExpressionString <$> stringLiteralP <|>
  CPrimaryExpressionParen <$> parenthesisP cExpressionP

cArgumentExpressionListP :: PEParser CArgumentExpressionList
cArgumentExpressionListP =
  parserToPEParser $
  liftA2
    CArgumentExpressionList
    cAssignmentExpressionP
    cArgumentExpressionListP'

cArgumentExpressionListOptionalP :: PEParser CArgumentExpressionListOptional
cArgumentExpressionListOptionalP =
  optionalParser
    cArgumentExpressionListP
    CArgumentExpressionListOptional
    CArgumentExpressionListOptionalEmpty

cArgumentExpressionListP' :: PEParser CArgumentExpressionList'
cArgumentExpressionListP' =
  parserToPEParser $
  liftA2
    CArgumentExpressionList'
    (singleP LComma *> cAssignmentExpressionP)
    cArgumentExpressionListP'

cConstantP :: PEParser CConstant
cConstantP =
  parserToPEParser $
  CConstantInt <$> intLiteralP <|>
  CConstantChar <$> charLiteralP <|>
  CConstantFloat <$> floatLiteralP <|>
  CConstantEnum <$> cIdentifierP

