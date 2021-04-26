{-# LANGUAGE LambdaCase #-}

{-
A recursive descent parser.
CTranslationUnit becomes the root element of the AST.
Expects a string of ScanElements terminted by LEndMarker
-}
module Parser where

import Control.Applicative

import Lexeme (CLexeme(..))
import ParseItem
import Scanner (Coordinates, ScanItem(..))

-----------
-- TYPES --
-----------
type Input = [ScanItem CLexeme]

newtype Parser a =
  Parser
    { runParser :: Input -> Either ParseError (Input, Input, a)
    }

type PIParser a = Parser (ParseItem a)

data ParseError =
  ParseError Coordinates String
  deriving (Eq, Show)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right ([], input, x)
  p1 <*> p2 =
    Parser $ \input -> do
      (parsed, notParsed, fab) <- runParser p1 input
      (parsed', notParsed', a) <- runParser p2 notParsed
      return (parsed <> parsed', notParsed', fab a)

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

parseCCode :: Input -> Either ParseError (ParseItem CTranslationUnit)
parseCCode input = do
  (_, _, result) <- runParser cParser input
  return result

cParser :: PIParser CTranslationUnit
cParser =
  cTranslationUnitP <* singleP LEndMarker

-----------
-- UTILS --
-----------

parserToPIParser :: Parser a -> PIParser a
parserToPIParser p =
  Parser $ \case
    [] -> Left unexpectedEof
    input@((ScanItem c _ _):_) -> do
      (parsed, notParsed, a) <- runParser p input
      return (parsed, notParsed, ParseItem c a initialSymbols)

unexpectedEof :: ParseError
unexpectedEof = ParseError (0, 0) "Unexpected EOF"

unexpectedLexeme :: Coordinates -> String -> String -> ParseError
unexpectedLexeme c expected encountered =
  ParseError c $
  mconcat ["Expected ", expected, ". Instead encountered ", encountered, "."]

------------------------
-- ELEMENTARY PARSERS --
------------------------

failingP :: Parser a
failingP =
  Parser $ \_ -> Left $ ParseError (0, 0) "Error"

singleP :: CLexeme -> Parser CLexeme
singleP l =
  Parser $ \case
    [] -> Left unexpectedEof
    (scanned@(ScanItem c _ lexeme):rest) ->
      if lexeme == l
        then Right ([scanned], rest, lexeme)
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: PIParser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    (scanned@(ScanItem c _ (LIntLiteral x)):rest) ->
      Right ([scanned], rest, ParseItem c x initialSymbols)
    ((ScanItem c _ l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: PIParser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    (scanned@(ScanItem c _ (LFloatLiteral x)):rest) ->
      Right ([scanned], rest, ParseItem c x initialSymbols)
    ((ScanItem c _ l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: PIParser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    (scanned@(ScanItem c _ (LCharLiteral x)):rest) ->
      Right ([scanned], rest, ParseItem c x initialSymbols)
    ((ScanItem c _ l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: PIParser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    (scanned@(ScanItem c _ (LStringLiteral x)):rest) ->
      Right ([scanned], rest, ParseItem c x initialSymbols)
    ((ScanItem c _ l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: Parser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    (scanned@(ScanItem _ _ (LLabel x)):rest) ->
      Right ([scanned], rest, x)
    ((ScanItem c _ l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l

optionalParser :: PIParser a -> (ParseItem a -> b) -> b -> PIParser b
optionalParser p nonEmpty blank =
  parserToPIParser $ nonEmpty <$> p <|> pure blank

parenthesisP :: Parser a -> Parser a
parenthesisP p = singleP LParenthesisOpen *> p <* singleP LParenthesisClose

bracketP :: Parser a -> Parser a
bracketP p = singleP LBracketOpen *> p <* singleP LBracketClose

braceP :: Parser a -> Parser a
braceP p = singleP LBraceOpen *> p <* singleP LBraceClose

afterP :: PIParser a -> PIParser b -> PIParser a
afterP before after =
  Parser $ \input ->
    let parsers = map splitParser [length input, length input - 1 .. 0]
        p = foldl (<|>) failingP parsers
     in runParser p input
  where
    splitParser n =
      Parser $ \input ->
        let (begin, end) = splitAt n input
        in
          case runParser before $ begin <> [ScanItem (0, 0) "" LEndMarker] of
            Right (_, [ScanItem _ _ LEndMarker], e) ->
              case runParser after end of
                Right _ -> Right (begin, end, e)
                _ -> Left $ ParseError (0, 0) "Error"
            _ -> Left $ ParseError (0, 0) "Error"

---------------
-- C PARSERS --
---------------

cIdentifierP :: PIParser CIdentifier
cIdentifierP =
  parserToPIParser $ CIdentifier <$> labelP

cIdentifierOptionalP :: PIParser CIdentifierOptional
cIdentifierOptionalP =
  optionalParser cIdentifierP CIdentifierOptional CIdentifierOptionalEmpty

cTranslationUnitP :: PIParser CTranslationUnit
cTranslationUnitP =
  parserToPIParser $
  liftA2 CTranslationUnit cExternalDeclarationP cTranslationUnitOptionalP

cTranslationUnitOptionalP :: PIParser CTranslationUnitOptional
cTranslationUnitOptionalP =
  optionalParser
    cTranslationUnitP
    CTranslationUnitOptional
    CTranslationUnitOptionalEmpty

cExternalDeclarationP :: PIParser CExternalDeclaration
cExternalDeclarationP =
  parserToPIParser $
  CExternalDeclarationFunction <$> cFunctionDefinitionP <|>
  CExternalDeclaration <$> cDeclarationP

cFunctionDefinitionP :: PIParser CFunctionDefinition
cFunctionDefinitionP =
  parserToPIParser $
  CFunctionDefinition <$>
  cFunctionSpecifiersP <*>
  cDeclaratorP <*>
  cDeclarationListOptionalP <*>
  cCompoundStatementP

-- interpret LLabel before open parenthesis as function name
-- rather than typedef name
cFunctionSpecifiersP :: PIParser CDeclarationSpecifiersOptional
cFunctionSpecifiersP =
  parserToPIParser $
  Parser $ \case
    input@((ScanItem _ _ (LLabel _)):(ScanItem _ _ LParenthesisOpen):_) ->
      Right ([], input, CDeclarationSpecifiersOptionalEmpty)
    input ->
      case runParser specifiersP input of
        Left _ -> Right ([], input, CDeclarationSpecifiersOptionalEmpty)
        r -> fmap CDeclarationSpecifiersOptional <$> r
    where
      specifiersP =
        parserToPIParser $ storageClassP <|> typeSpecifierP <|> typeQualifierP
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

cDeclarationP :: PIParser CDeclaration
cDeclarationP =
  parserToPIParser $
  liftA2
    CDeclaration
    (afterP cDeclarationSpecifiersP after)
    after
  where after = cInitDeclaratorListOptionalP <* singleP LSemiColon

cDeclarationListP :: PIParser CDeclarationList
cDeclarationListP =
  parserToPIParser $
  liftA2 CDeclarationList cDeclarationP cDeclarationListOptionalP

cDeclarationListOptionalP :: PIParser CDeclarationListOptional
cDeclarationListOptionalP =
  optionalParser
    cDeclarationListP
    CDeclarationListOptional
    CDeclarationListOptionalEmpty

cDeclarationSpecifiersP :: PIParser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserToPIParser $
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

cDeclarationSpecifiersOptionalP :: PIParser CDeclarationSpecifiersOptional
cDeclarationSpecifiersOptionalP =
  optionalParser
    cDeclarationSpecifiersP
    CDeclarationSpecifiersOptional
    CDeclarationSpecifiersOptionalEmpty

cStorageClassSpecifierP :: PIParser CStorageClassSpecifier
cStorageClassSpecifierP =
  parserToPIParser $
  CStorageClassSpecifierAuto <$ singleP LAuto <|>
  CStorageClassSpecifierRegister <$ singleP LRegister <|>
  CStorageClassSpecifierStatic <$ singleP LStatic <|>
  CStorageClassSpecifierExtern <$ singleP LExtern <|>
  CStorageClassSpecifierTypedef <$ singleP LTypedef

cTypeSpecifierP :: PIParser CTypeSpecifier
cTypeSpecifierP =
  parserToPIParser $
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

cTypeQualifierP :: PIParser CTypeQualifier
cTypeQualifierP =
  parserToPIParser $
  CTypeQualifierConst <$ singleP LConst <|>
  CTypeQualifierVolatile <$ singleP LVolatile

cStructOrUnionSpecifierP :: PIParser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserToPIParser $
  liftA3
    CStructOrUnionSpecifierList
    cStructOrUnionP
    cIdentifierOptionalP
    (braceP cStructDeclarationListP) <|>
  liftA2
    CStructOrUnionSpecifier
    cStructOrUnionP
    cIdentifierP

cStructOrUnionP :: PIParser CStructOrUnion
cStructOrUnionP =
  parserToPIParser $
  CStructOrUnionStruct <$ singleP LStruct <|>
  CStructOrUnionUnion <$ singleP LUnion

cStructDeclarationListP :: PIParser CStructDeclarationList
cStructDeclarationListP =
  parserToPIParser $
  liftA2
    CStructDeclarationList
    cStructDeclarationP
    cStructDeclarationListOptionalP

cStructDeclarationListOptionalP :: PIParser CStructDeclarationListOptional
cStructDeclarationListOptionalP =
  optionalParser
    cStructDeclarationListP
    CStructDeclarationListOptional
    CStructDeclarationListOptionalEmpty

cInitDeclaratorListP :: PIParser CInitDeclaratorList
cInitDeclaratorListP =
  parserToPIParser $
  liftA2
    CInitDeclaratorList
    cInitDeclaratorP
    cInitDeclaratorListP'

cInitDeclaratorListOptionalP :: PIParser CInitDeclaratorListOptional
cInitDeclaratorListOptionalP =
  optionalParser
    cInitDeclaratorListP
    CInitDeclaratorListOptional
    CInitDeclaratorListOptionalEmpty

cInitDeclaratorListP' :: PIParser CInitDeclaratorList'
cInitDeclaratorListP' =
  parserToPIParser $
  liftA2
    CInitDeclaratorList'
    (singleP LComma *> cInitDeclaratorP)
    cInitDeclaratorListP' <|>
  pure
    CInitDeclaratorList'Empty

cInitDeclaratorP :: PIParser CInitDeclarator
cInitDeclaratorP =
  parserToPIParser $
  liftA2
    CInitDeclarator
    cDeclaratorP
    cAssignInitializerOptionalP

cAssignInitializerOptionalP :: PIParser CAssignInitializerOptional
cAssignInitializerOptionalP =
  parserToPIParser $
  CAssignInitializerOptional <$> (singleP LAssign *> cInitializerP) <|>
  pure CAssignInitializerOptionalEmpty

cStructDeclarationP :: PIParser CStructDeclaration
cStructDeclarationP =
  parserToPIParser $
  liftA2
    CStructDeclaration
    (afterP cSpecifierQualifierListP after)
    after
  where after = cStructDeclaratorListP <* singleP LSemiColon

cSpecifierQualifierListP :: PIParser CSpecifierQualifierList
cSpecifierQualifierListP =
  parserToPIParser $
  liftA2
    CSpecifierQualifierListSpecifier
    cTypeSpecifierP
    cSpecifierQualifierListOptionalP <|>
  liftA2
    CSpecifierQualifierListQualifier
    cTypeQualifierP
    cSpecifierQualifierListOptionalP

cSpecifierQualifierListOptionalP :: PIParser CSpecifierQualifierListOptional
cSpecifierQualifierListOptionalP =
  optionalParser
    cSpecifierQualifierListP
    CSpecifierQualifierListOptional
    CSpecifierQualifierListOptionalEmpty

cStructDeclaratorListP :: PIParser CStructDeclaratorList
cStructDeclaratorListP =
  parserToPIParser $
  liftA2
    CStructDeclaratorList
    cStructDeclaratorP
    cStructDeclaratorListP'

cStructDeclaratorListP' :: PIParser CStructDeclaratorList'
cStructDeclaratorListP' =
  parserToPIParser $
  liftA2
    CStructDeclaratorList'
    (singleP LComma *> cStructDeclaratorP)
    cStructDeclaratorListP' <|>
  pure
    CStructDeclaratorList'Empty

cStructDeclaratorP :: PIParser CStructDeclarator
cStructDeclaratorP =
  parserToPIParser $
  liftA2
    CStructDeclaratorField
    cDeclaratorOptionalP
    (singleP LColon *> cConstantExpressionP) <|>
  CStructDeclarator <$> cDeclaratorP

cEnumSpecifierP :: PIParser CEnumSpecifier
cEnumSpecifierP =
  parserToPIParser $
  liftA2
    CEnumSpecifierList
    (singleP LEnum *> cIdentifierOptionalP)
    (braceP cEnumeratorListP) <|>
  CEnumSpecifier <$> (singleP LEnum *> cIdentifierP)

cEnumeratorListP :: PIParser CEnumeratorList
cEnumeratorListP =
  parserToPIParser $ liftA2 CEnumeratorList cEnumeratorP cEnumeratorListP'

cEnumeratorListP' :: PIParser CEnumeratorList'
cEnumeratorListP' =
  parserToPIParser $
  liftA2
    CEnumeratorList'
    (singleP LComma *> cEnumeratorP)
    cEnumeratorListP' <|>
  pure
    CEnumeratorList'Empty

cEnumeratorP :: PIParser CEnumerator
cEnumeratorP =
  parserToPIParser $
  liftA2
    CEnumeratorAssign
    cIdentifierP
    (singleP LAssign *> cConstantExpressionP) <|>
  liftA
    CEnumerator
    cIdentifierP

cDeclaratorP :: PIParser CDeclarator
cDeclaratorP =
  parserToPIParser $ liftA2 CDeclarator cPointerOptionalP cDirectDeclaratorP

cDeclaratorOptionalP :: PIParser CDeclaratorOptional
cDeclaratorOptionalP =
  optionalParser
    cDeclaratorP
    CDeclaratorOptional
    CDeclaratorOptionalEmpty

cDirectDeclaratorP :: PIParser CDirectDeclarator
cDirectDeclaratorP =
  parserToPIParser $
  liftA2
    CDirectDeclaratorId
    cIdentifierP
    cDirectDeclaratorP' <|>
  liftA2
    CDirectDeclaratorParen
    (parenthesisP cDeclaratorP)
    cDirectDeclaratorP'

cDirectDeclaratorP' :: PIParser CDirectDeclarator'
cDirectDeclaratorP' =
  parserToPIParser $
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

cPointerP :: PIParser CPointer
cPointerP =
  parserToPIParser $
  liftA2
    CPointer
    (singleP LStar *> cTypeQualifierListOptionalP)
    cPointerOptionalP

cPointerOptionalP :: PIParser CPointerOptional
cPointerOptionalP =
  optionalParser cPointerP CPointerOptional CPointerOptionalEmpty

cTypeQualifierListP :: PIParser CTypeQualifierList
cTypeQualifierListP =
  parserToPIParser $
  liftA2
    CTypeQualifierList
    cTypeQualifierP
    cTypeQualifierListOptionalP

cTypeQualifierListOptionalP :: PIParser CTypeQualifierListOptional
cTypeQualifierListOptionalP =
  optionalParser
    cTypeQualifierListP
    CTypeQualifierListOptional
    CTypeQualifierListOptionalEmpty

cParameterTypeListP :: PIParser CParameterTypeList
cParameterTypeListP =
  parserToPIParser $
  liftA2
    CParameterTypeList
    cParameterListP
    cVarArgsOptionalP

cParameterTypeListOptionalP :: PIParser CParameterTypeListOptional
cParameterTypeListOptionalP =
  optionalParser
    cParameterTypeListP
    CParameterTypeListOptional
    CParameterTypeListOptionalEmpty

cVarArgsOptionalP :: PIParser CVarArgsOptional
cVarArgsOptionalP =
  parserToPIParser $
  CVarArgsOptional <$ singleP LComma <* singleP LVarargs <|>
  pure CVarArgsOptionalEmpty

cParameterListP :: PIParser CParameterList
cParameterListP =
  parserToPIParser $
  liftA2
    CParameterList
    cParameterDeclarationP
    cParameterListP'

cParameterListP' :: PIParser CParameterList'
cParameterListP' =
  parserToPIParser $
  liftA2
    CParameterList'
    (singleP LComma *> cParameterDeclarationP)
    cParameterListP' <|>
  pure
    CParameterList'Empty

cParameterDeclarationP :: PIParser CParameterDeclaration
cParameterDeclarationP =
  parserToPIParser $
  liftA2
    CParameterDeclaration
    cDeclarationSpecifiersP
    cParameterDeclarationP'

cParameterDeclarationP' :: PIParser CParameterDeclaration'
cParameterDeclarationP' =
  parserToPIParser $
  CParameterDeclaration' <$> cDeclaratorP <|>
  CParameterDeclaration'Abstract <$> cAbstractDeclaratorOptionalP

cIdentifierListP :: PIParser CIdentifierList
cIdentifierListP =
  parserToPIParser $ liftA2 CIdentifierList cIdentifierP cIdentifierListP'

cIdentifierListOptionalP :: PIParser CIdentifierListOptional
cIdentifierListOptionalP =
  optionalParser
    cIdentifierListP
    CIdentifierListOptional
    CIdentifierListOptionalEmpty

cIdentifierListP' :: PIParser CIdentifierList'
cIdentifierListP' =
  parserToPIParser $
  liftA2
    CIdentifierList'
    (singleP LComma *> cIdentifierP)
    cIdentifierListP' <|>
  pure
    CIdentifierList'Empty

cInitializerP :: PIParser CInitializer
cInitializerP =
  parserToPIParser $
  CInitializerAssignment <$> cAssignmentExpressionP <|>
  CInitializerInitList <$>
  bracketP (cInitializerListP <* (singleP LComma <|> pure LComma))

cInitializerListP :: PIParser CInitializerList
cInitializerListP =
  parserToPIParser $
  liftA2
    CInitializerList
    cInitializerP
    cInitializerListP'

cInitializerListP' :: PIParser CInitializerList'
cInitializerListP' =
  parserToPIParser $
  liftA2
    CInitializerList'
    (singleP LComma *> cInitializerP)
    cInitializerListP' <|>
  pure
    CInitializerList'Empty

cTypeNameP :: PIParser CTypeName
cTypeNameP =
  parserToPIParser $
  liftA2
    CTypeName
    cSpecifierQualifierListP
    cAbstractDeclaratorOptionalP

cAbstractDeclaratorP :: PIParser CAbstractDeclarator
cAbstractDeclaratorP =
  parserToPIParser $
  liftA2
    CAbstractDeclaratorDirect
    cPointerOptionalP
    cDirectAbstractDeclaratorP <|>
  CAbstractDeclaratorPointer <$> cPointerP

cAbstractDeclaratorOptionalP :: PIParser CAbstractDeclaratorOptional
cAbstractDeclaratorOptionalP =
  optionalParser
    cAbstractDeclaratorP
    CAbstractDeclaratorOptional
    CAbstractDeclaratorOptionalEmpty

cDirectAbstractDeclaratorP :: PIParser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP =
  parserToPIParser $
  liftA2
    CDirectAbstractDeclaratorParen
    (parenthesisP cAbstractDeclaratorP)
    cDirectAbstractDeclaratorP' <|>
  liftA2
    CDirectAbstractDeclaratorIndexed
    (braceP cConstantExpressionOptionalP)
    cDirectAbstractDeclaratorP' <|>
  liftA2
    CDirectAbstractDeclaratorParams
    (braceP cParameterTypeListOptionalP)
    cDirectAbstractDeclaratorP'

cDirectAbstractDeclaratorP' :: PIParser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parserToPIParser $
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

cTypedefNameP :: PIParser CTypedefName
cTypedefNameP =
  parserToPIParser $ CTypedefName <$> cIdentifierP

cStatementP :: PIParser CStatement
cStatementP =
  parserToPIParser $
  CStatementLabeled <$> cLabeledStatementP <|>
  CStatementExpression <$> cExpressionStatementP <|>
  CStatementCompound <$> cCompoundStatementP <|>
  CStatementSelection <$> cSelectionStatementP <|>
  CStatementIteration <$> cIterationStatementP <|>
  CStatementJump <$> cJumpStatementP

cLabeledStatementP :: PIParser CLabeledStatement
cLabeledStatementP =
  parserToPIParser $
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

cExpressionStatementP :: PIParser CExpressionStatement
cExpressionStatementP =
  parserToPIParser $
  liftA
    CExpressionStatement
    (cExpressionOptionalP <* singleP LSemiColon)

cCompoundStatementP :: PIParser CCompoundStatement
cCompoundStatementP =
  braceP $
  parserToPIParser $
  liftA2
    CCompoundStatement
    cDeclarationListOptionalP
    cStatementListOptionalP

cStatementListP :: PIParser CStatementList
cStatementListP =
  parserToPIParser $
  liftA2
    CStatementList
    cStatementP
    cStatementListOptionalP

cStatementListOptionalP :: PIParser CStatementListOptional
cStatementListOptionalP =
  optionalParser
    cStatementListP
    CStatementListOptional
    CStatementListOptionalEmpty

cSelectionStatementP :: PIParser CSelectionStatement
cSelectionStatementP =
  parserToPIParser $
  liftA3
    CSelectionStatementIf
    (singleP LIf *> parenthesisP cExpressionP)
    cStatementP
    cElseOptionalP <|>
  liftA2
    CSelectionStatementSwitch
    (singleP LSwitch *> parenthesisP cExpressionP)
    cStatementP

cElseOptionalP :: PIParser CElseOptional
cElseOptionalP =
  parserToPIParser $
  CElseOptional <$> (singleP LElse *> cStatementP) <|>
  pure CElseOptionalEmpty

cIterationStatementP :: PIParser CIterationStatement
cIterationStatementP =
  parserToPIParser $
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

cJumpStatementP :: PIParser CJumpStatement
cJumpStatementP =
  parserToPIParser $
  CJumpStatementGoto <$> (singleP LGoto *> cIdentifierP <* singleP LSemiColon) <|>
  CJumpStatementContinue <$ singleP LContinue <* singleP LSemiColon <|>
  CJumpStatementBreak <$ singleP LBreak <* singleP LSemiColon <|>
  CJumpStatementReturn <$> (singleP LReturn *> cExpressionOptionalP <* singleP LSemiColon)

cExpressionP :: PIParser CExpression
cExpressionP =
  parserToPIParser $
  liftA2
    CExpression
    cAssignmentExpressionP
    cExpressionP'

cExpressionOptionalP :: PIParser CExpressionOptional
cExpressionOptionalP =
  optionalParser cExpressionP CExpressionOptional CExpressionOptionalEmpty

cExpressionP' :: PIParser CExpression'
cExpressionP' =
  parserToPIParser $
  liftA2
    CExpression'
    (singleP LComma *> cAssignmentExpressionP)
    cExpressionP' <|>
  pure
    CExpression'Empty

cAssignmentExpressionP :: PIParser CAssignmentExpression
cAssignmentExpressionP =
  parserToPIParser $
  liftA3
    CAssignmentExpression
    cUnaryExpressionP
    cAssignmentOperatorP
    cAssignmentExpressionP <|>
  liftA
    CAssignmentExpressionConditional
    cConditionalExpressionP

cAssignmentOperatorP :: PIParser CAssignmentOperator
cAssignmentOperatorP =
  parserToPIParser $
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

cConditionalExpressionP :: PIParser CConditionalExpression
cConditionalExpressionP =
  parserToPIParser $
  liftA2
    CConditionalExpression
    cLogicalOrExpressionP
    cTernaryOptionalP

cTernaryOptionalP :: PIParser CTernaryOptional
cTernaryOptionalP =
  parserToPIParser $
  liftA2
    CTernaryOptional
    (singleP LTernary *> cExpressionP)
    (singleP LColon *> cConditionalExpressionP) <|>
  pure
    CTernaryOptionalEmpty

cConstantExpressionP :: PIParser CConstantExpression
cConstantExpressionP =
  parserToPIParser $ CConstantExpression <$> cConditionalExpressionP

cConstantExpressionOptionalP :: PIParser CConstantExpressionOptional
cConstantExpressionOptionalP =
  optionalParser
    cConstantExpressionP
    CConstantExpressionOptional
    CConstantExpressionOptionalEmpty

cLogicalOrExpressionP :: PIParser CLogicalOrExpression
cLogicalOrExpressionP =
  parserToPIParser $
  liftA2
    CLogicalOrExpression
    cLogicalAndExpressionP
    cLogicalOrExpressionP'

cLogicalOrExpressionP' :: PIParser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parserToPIParser $
  liftA2
    CLogicalOrExpression'
    (singleP LOr *> cLogicalAndExpressionP)
    cLogicalOrExpressionP' <|>
  pure
    CLogicalOrExpression'Empty

cLogicalAndExpressionP :: PIParser CLogicalAndExpression
cLogicalAndExpressionP =
  parserToPIParser $
  liftA2
    CLogicalAndExpression
    cInclusiveOrExpressionP
    cLogicalAndExpressionP'

cLogicalAndExpressionP' :: PIParser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parserToPIParser $
  liftA2
    CLogicalAndExpression'
    (singleP LAnd *> cInclusiveOrExpressionP)
    cLogicalAndExpressionP' <|>
  pure
    CLogicalAndExpression'Empty

cInclusiveOrExpressionP :: PIParser CInclusiveOrExpression
cInclusiveOrExpressionP =
  parserToPIParser $
  liftA2
    CInclusiveOrExpression
    cExclusiveOrExpressionP
    cInclusiveOrExpressionP'

cInclusiveOrExpressionP' :: PIParser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parserToPIParser $
  liftA2
    CInclusiveOrExpression'
    (singleP LBitwiseOr *> cExclusiveOrExpressionP)
    cInclusiveOrExpressionP' <|>
  pure
    CInclusiveOrExpression'Empty

cExclusiveOrExpressionP :: PIParser CExclusiveOrExpression
cExclusiveOrExpressionP =
  parserToPIParser $
  liftA2
    CExclusiveOrExpression
    cAndExpressionP
    cExclusiveOrExpressionP'

cExclusiveOrExpressionP' :: PIParser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parserToPIParser $
  liftA2
    CExclusiveOrExpression'
    (singleP LBitwiseXor *> cAndExpressionP)
    cExclusiveOrExpressionP' <|>
  pure
    CExclusiveOrExpression'Empty

cAndExpressionP :: PIParser CAndExpression
cAndExpressionP =
  parserToPIParser $
  liftA2
    CAndExpression
    cEqualityExpressionP
    cAndExpressionP'

cAndExpressionP' :: PIParser CAndExpression'
cAndExpressionP' =
  parserToPIParser $
  liftA2
    CAndExpression'
    (singleP LAmp *> cEqualityExpressionP)
    cAndExpressionP' <|>
  pure
    CAndExpression'Empty

cEqualityExpressionP :: PIParser CEqualityExpression
cEqualityExpressionP =
  parserToPIParser $
  liftA2
    CEqualityExpression
    cRelationalExpressionP
    cEqualityExpressionP'

cEqualityExpressionP' :: PIParser CEqualityExpression'
cEqualityExpressionP' =
  parserToPIParser $
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

cRelationalExpressionP :: PIParser CRelationalExpression
cRelationalExpressionP =
  parserToPIParser $
  liftA2
    CRelationalExpression
    cShiftExpressionP
    cRelationalExpressionP'

cRelationalExpressionP' :: PIParser CRelationalExpression'
cRelationalExpressionP' =
  parserToPIParser $
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

cShiftExpressionP :: PIParser CShiftExpression
cShiftExpressionP =
  parserToPIParser $
  liftA2
    CShiftExpression
    cAdditiveExpressionP
    cShiftExpressionP'

cShiftExpressionP' :: PIParser CShiftExpression'
cShiftExpressionP' =
  parserToPIParser $
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

cAdditiveExpressionP :: PIParser CAdditiveExpression
cAdditiveExpressionP =
  parserToPIParser $
  liftA2
    CAdditiveExpression
    cMultiplicativeExpressionP
    cAdditiveExpressionP'

cAdditiveExpressionP' :: PIParser CAdditiveExpression'
cAdditiveExpressionP' =
  parserToPIParser $
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

cMultiplicativeExpressionP :: PIParser CMultiplicativeExpression
cMultiplicativeExpressionP =
  parserToPIParser $
  liftA2
    CMultiplicativeExpression
    cCastExpressionP
    cMultiplicativeExpressionP'

cMultiplicativeExpressionP' :: PIParser CMultiplicativeExpression'
cMultiplicativeExpressionP' =
  parserToPIParser $
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

cCastExpressionP :: PIParser CCastExpression
cCastExpressionP =
  parserToPIParser $
  liftA2
    CCastExpression
    (parenthesisP cTypeNameP)
    cCastExpressionP <|>
  liftA
    CCastExpressionUnary
    cUnaryExpressionP

cUnaryExpressionP :: PIParser CUnaryExpression
cUnaryExpressionP =
  parserToPIParser $
  CUnaryExpressionInc <$> (singleP LIncrement *> cUnaryExpressionP) <|>
  CUnaryExpressionDec <$> (singleP LDecrement *> cUnaryExpressionP) <|>
  CUnaryExpressionSizeofType <$> (singleP LSizeof *> parenthesisP cTypeNameP) <|>
  CUnaryExpressionSizeof <$> (singleP LSizeof *> cUnaryExpressionP) <|>
  CUnaryExpressionUnaryOp <$> cUnaryOperatorP <*> cCastExpressionP <|>
  CUnaryExpressionPostfix <$> cPostfixExpressionP

cUnaryOperatorP :: PIParser CUnaryOperator
cUnaryOperatorP =
  parserToPIParser $
  CUnaryOperatorAnd <$ singleP LAmp <|>
  CUnaryOperatorMul <$ singleP LStar <|>
  CUnaryOperatorAdd <$ singleP LPlus <|>
  CUnaryOperatorSub <$ singleP LMinus <|>
  CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot <|>
  CUnaryOperatorNot <$ singleP LNot

cPostfixExpressionP :: PIParser CPostfixExpression
cPostfixExpressionP =
  parserToPIParser $
  liftA2
    CPostfixExpression
    cPrimaryExpressionP
    cPostfixExpressionP'

cPostfixExpressionP' :: PIParser CPostfixExpression'
cPostfixExpressionP' =
  parserToPIParser $
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

cPrimaryExpressionP :: PIParser CPrimaryExpression
cPrimaryExpressionP =
  parserToPIParser $
  CPrimaryExpressionId <$> cIdentifierP <|>
  CPrimaryExpressionConst <$> cConstantP <|>
  CPrimaryExpressionString <$> stringLiteralP <|>
  CPrimaryExpressionParen <$> parenthesisP cExpressionP

cArgumentExpressionListP :: PIParser CArgumentExpressionList
cArgumentExpressionListP =
  parserToPIParser $
  liftA2
    CArgumentExpressionList
    cAssignmentExpressionP
    cArgumentExpressionListP'

cArgumentExpressionListOptionalP :: PIParser CArgumentExpressionListOptional
cArgumentExpressionListOptionalP =
  optionalParser
    cArgumentExpressionListP
    CArgumentExpressionListOptional
    CArgumentExpressionListOptionalEmpty

cArgumentExpressionListP' :: PIParser CArgumentExpressionList'
cArgumentExpressionListP' =
  parserToPIParser $
  liftA2
    CArgumentExpressionList'
    (singleP LComma *> cAssignmentExpressionP)
    cArgumentExpressionListP' <|>
  pure
    CArgumentExpressionList'Empty

cConstantP :: PIParser CConstant
cConstantP =
  parserToPIParser $
  CConstantInt <$> intLiteralP <|>
  CConstantChar <$> charLiteralP <|>
  CConstantFloat <$> floatLiteralP <|>
  CConstantEnum <$> cIdentifierP

