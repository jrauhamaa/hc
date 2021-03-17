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
import Scanner (Coordinates, ScanElement(..))

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

data ParseError =
  ParseError Coordinates String
  deriving (Eq, Show)

instance Functor Parser where
  fmap fab pa = Parser $ (fmap . fmap . fmap) fab . runParser pa

instance Applicative Parser where
  pure x = Parser $ \input -> Right [(input, x)]
  p1 <*> p2 =
    Parser $ \input -> do
      rab <- runParser p1 input
      collectOutput $
        map (\(input', fab) -> (fmap . fmap) fab <$> runParser p2 input') rab

parseCCode :: Input -> Either ParseError (ParseElement CTranslationUnit)
parseCCode input = do
  results <- runParser cParser input
  return $ snd $ head results

cParser :: PEParser CTranslationUnit
cParser =
  cTranslationUnitP <* singleP LEndMarker

-----------
-- UTILS --
-----------
collectOutput :: [ParseOutput a] -> ParseOutput a
collectOutput p =
  if null successful
    then minimumBy compareLoc p
    else concat <$> sequenceA successful
  where
    successful = filter isRight p
    compareLoc (Left (ParseError c1 _)) (Left (ParseError c2 _)) =
      compare c1 c2
    compareLoc _ _ = EQ -- this shoudl never happen

parserUnion :: [Parser a] -> PEParser a
parserUnion parsers =
  parserToPEParser
    (Parser $ \input -> collectOutput $ map (`runParser` input) parsers)

parserToPEParser :: Parser a -> PEParser a
parserToPEParser p =
  Parser $ \case
    [] -> (fmap . fmap) (ParseElement (0, 0)) <$> runParser p []
    a@((ScanElement c _):_) -> (fmap . fmap) (ParseElement c) <$> runParser p a

unexpectedEof :: ParseError
unexpectedEof = ParseError (0, 0) "Unexpected EOF"

unexpectedLexeme :: Coordinates -> String -> String -> ParseError
unexpectedLexeme loc expected encountered =
  ParseError loc $
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
        then Right [(rest, l)]
        else Left $ unexpectedLexeme c (show l) (show lexeme)

intLiteralP :: PEParser Int
intLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LIntLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LIntLiteral" $ show l

floatLiteralP :: PEParser Double
floatLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LFloatLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LFloatLiteral" $ show l

charLiteralP :: PEParser Char
charLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LCharLiteral x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LCharLiteral" $ show l

stringLiteralP :: PEParser String
stringLiteralP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LStringLiteral x)):rest) ->
      Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LStringLiteral" $ show l

labelP :: PEParser String
labelP =
  Parser $ \case
    [] -> Left unexpectedEof
    ((ScanElement c (LLabel x)):rest) -> Right [(rest, ParseElement c x)]
    ((ScanElement c l):_) -> Left $ unexpectedLexeme c "LLabel" $ show l

optionalP :: PEParser a -> (ParseElement a -> b) -> b -> PEParser b
optionalP nonEmptyP nonEmpty empty =
  parserUnion
    [ nonEmpty <$> nonEmptyP
    , pure empty
    ]

parenthesisP :: PEParser a -> PEParser a
parenthesisP p = singleP LParenthesisOpen *> p <* singleP LParenthesisClose

bracketP :: PEParser a -> PEParser a
bracketP p = singleP LBracketOpen *> p <* singleP LBracketClose

braceP :: PEParser a -> PEParser a
braceP p = singleP LBraceOpen *> p <* singleP LBraceClose

---------------
-- C PARSERS --
---------------

cIdentifierP :: PEParser CIdentifier
cIdentifierP = parserToPEParser (CIdentifier <$> labelP)

cIdentifierOptionalP :: PEParser CIdentifierOptional
cIdentifierOptionalP =
  optionalP cIdentifierP CIdentifierOptional CIdentifierOptionalEmpty

cTranslationUnitP :: PEParser CTranslationUnit
cTranslationUnitP =
  parserToPEParser
    (CTranslationUnit <$> cExternalDeclarationP <*> cTranslationUnitOptionalP)

cTranslationUnitOptionalP :: PEParser CTranslationUnitOptional
cTranslationUnitOptionalP =
  optionalP
    cTranslationUnitP
    CTranslationUnitOptional
    CTranslationUnitOptionalEmpty

cExternalDeclarationP :: PEParser CExternalDeclaration
cExternalDeclarationP =
  parserUnion
    [ CExternalDeclarationFunction <$> cFunctionDefinitionP
    , CExternalDeclaration <$> cDeclarationP
    ]

cFunctionDefinitionP :: PEParser CFunctionDefinition
cFunctionDefinitionP =
  parserToPEParser
    (CFunctionDefinition <$>
     cDeclarationSpecifiersOptionalP <*>
     cDeclaratorP <*>
     cDeclarationListOptionalP <*>
     cCompoundStatementP)

cDeclarationP :: PEParser CDeclaration
cDeclarationP =
  parserToPEParser
    (CDeclaration <$>
     cDeclarationSpecifiersP <*>
     cInitDeclaratorListOptionalP <*
     singleP LSemiColon)

cDeclarationListP :: PEParser CDeclarationList
cDeclarationListP =
  parserToPEParser
    (CDeclarationList <$> cDeclarationP <*> cDeclarationListOptionalP)

cDeclarationListOptionalP :: PEParser CDeclarationListOptional
cDeclarationListOptionalP =
  optionalP
    cDeclarationListP
    CDeclarationListOptional
    CDeclarationListOptionalEmpty

cDeclarationSpecifiersP :: PEParser CDeclarationSpecifiers
cDeclarationSpecifiersP =
  parserUnion
    [ CDeclarationSpecifiersStorageClass <$>
        cStorageClassSpecifierP <*>
        cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeSpecifier <$>
        cTypeSpecifierP <*>
        cDeclarationSpecifiersOptionalP
    , CDeclarationSpecifiersTypeQualifier <$>
        cTypeQualifierP <*>
        cDeclarationSpecifiersOptionalP
    ]

cDeclarationSpecifiersOptionalP :: PEParser CDeclarationSpecifiersOptional
cDeclarationSpecifiersOptionalP =
  optionalP
    cDeclarationSpecifiersP
    CDeclarationSpecifiersOptional
    CDeclarationSpecifiersOptionalEmpty

cStorageClassSpecifierP :: PEParser CStorageClassSpecifier
cStorageClassSpecifierP =
  parserUnion
    [ CStorageClassSpecifierAuto <$ singleP LAuto
    , CStorageClassSpecifierRegister <$ singleP LRegister
    , CStorageClassSpecifierStatic <$ singleP LStatic
    , CStorageClassSpecifierExtern <$ singleP LExtern
    , CStorageClassSpecifierTypedef <$ singleP LTypedef
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

cTypeQualifierP :: PEParser CTypeQualifier
cTypeQualifierP =
  parserUnion
    [ CTypeQualifierConst <$ singleP LConst
    , CTypeQualifierVolatile <$ singleP LVolatile
    ]

cStructOrUnionSpecifierP :: PEParser CStructOrUnionSpecifier
cStructOrUnionSpecifierP =
  parserUnion
    [ CStructOrUnionSpecifierList <$>
        cStructOrUnionP <*>
        cIdentifierOptionalP <*>
        braceP cStructDeclarationListP
    , CStructOrUnionSpecifier <$>
        cStructOrUnionP <*>
        cIdentifierP
    ]

cStructOrUnionP :: PEParser CStructOrUnion
cStructOrUnionP =
  parserUnion
    [ CStructOrUnionStruct <$ singleP LStruct
    , CStructOrUnionUnion <$ singleP LUnion
    ]

cStructDeclarationListP :: PEParser CStructDeclarationList
cStructDeclarationListP =
  parserToPEParser
    (CStructDeclarationList <$>
     (singleP LStruct *> cDeclarationP) <*>
     cStructDeclarationListOptionalP)

cStructDeclarationListOptionalP :: PEParser CStructDeclarationListOptional
cStructDeclarationListOptionalP =
  optionalP
    cStructDeclarationListP
    CStructDeclarationListOptional
    CStructDeclarationListOptionalEmpty

cInitDeclaratorListP :: PEParser CInitDeclaratorList
cInitDeclaratorListP =
  parserToPEParser
    (CInitDeclaratorList <$> cInitDeclaratorP <*> cInitDeclaratorListP')

cInitDeclaratorListOptionalP :: PEParser CInitDeclaratorListOptional
cInitDeclaratorListOptionalP =
  optionalP
    cInitDeclaratorListP
    CInitDeclaratorListOptional
    CInitDeclaratorListOptionalEmpty

cInitDeclaratorListP' :: PEParser CInitDeclaratorList'
cInitDeclaratorListP' =
  parserUnion
    [ CInitDeclaratorList' <$>
        (singleP LComma *> cInitDeclaratorP) <*>
        cInitDeclaratorListP'
    , pure
        CInitDeclaratorList'Empty
    ]

cInitDeclaratorP :: PEParser CInitDeclarator
cInitDeclaratorP =
  parserToPEParser
    (CInitDeclarator <$> cDeclaratorP <*> cAssignInitializerOptionalP)

cAssignInitializerOptionalP :: PEParser CAssignInitializerOptional
cAssignInitializerOptionalP =
  parserUnion
    [ CAssignInitializerOptional <$> (singleP LAssign *> cInitializerP)
    , pure CAssignInitializerOptionalEmpty
    ]

cStructDeclarationP :: PEParser CStructDeclaration
cStructDeclarationP =
  parserToPEParser
    (CStructDeclaration <$>
     cSpecifierQualifierListP <*>
     cStructDeclaratorListP <*
     singleP LSemiColon)

cSpecifierQualifierListP :: PEParser CSpecifierQualifierList
cSpecifierQualifierListP =
  parserUnion
    [ CSpecifierQualifierListSpecifier <$>
        cTypeSpecifierP <*>
        cSpecifierQualifierListOptionalP
    , CSpecifierQualifierListQualifier <$>
        cTypeQualifierP <*>
        cSpecifierQualifierListOptionalP
    ]

cSpecifierQualifierListOptionalP :: PEParser CSpecifierQualifierListOptional
cSpecifierQualifierListOptionalP =
  optionalP
    cSpecifierQualifierListP
    CSpecifierQualifierListOptional
    CSpecifierQualifierListOptionalEmpty

cStructDeclaratorListP :: PEParser CStructDeclaratorList
cStructDeclaratorListP =
  parserToPEParser
    (CStructDeclaratorList <$>
     cStructDeclaratorP <*>
     cStructDeclaratorListP')

cStructDeclaratorListP' :: PEParser CStructDeclaratorList'
cStructDeclaratorListP' =
  parserUnion
    [ CStructDeclaratorList' <$>
        (singleP LComma *> cStructDeclaratorP) <*>
        cStructDeclaratorListP'
    , pure
        CStructDeclaratorList'Empty
    ]

cStructDeclaratorP :: PEParser CStructDeclarator
cStructDeclaratorP =
  parserUnion
    [ CStructDeclarator <$>
        cDeclaratorP
    , CStructDeclaratorField <$>
        cDeclaratorOptionalP <*>
        (singleP LColon *> cConstantExpressionP)
    ]

cEnumSpecifierP :: PEParser CEnumSpecifier
cEnumSpecifierP =
  parserUnion
    [ CEnumSpecifier <$>
        (singleP LEnum *> cIdentifierP)
    , CEnumSpecifierList <$>
        (singleP LEnum *> cIdentifierOptionalP) <*>
        braceP cEnumeratorListP
    ]

cEnumeratorListP :: PEParser CEnumeratorList
cEnumeratorListP =
  parserToPEParser (CEnumeratorList <$> cEnumeratorP <*> cEnumeratorListP')

cEnumeratorListP' :: PEParser CEnumeratorList'
cEnumeratorListP' =
  parserUnion
    [ CEnumeratorList' <$>
        (singleP LComma *> cEnumeratorP) <*>
        cEnumeratorListP'
    , pure
        CEnumeratorList'Empty
    ]

cEnumeratorP :: PEParser CEnumerator
cEnumeratorP =
  parserUnion
    [ CEnumerator <$>
        cIdentifierP
    , CEnumeratorAssign <$>
        cIdentifierP <*>
        (singleP LAssign *> cConstantExpressionP)
    ]

cDeclaratorP :: PEParser CDeclarator
cDeclaratorP =
  parserToPEParser (CDeclarator <$> cPointerOptionalP <*> cDirectDeclaratorP)

cDeclaratorOptionalP :: PEParser CDeclaratorOptional
cDeclaratorOptionalP =
  optionalP cDeclaratorP CDeclaratorOptional CDeclaratorOptionalEmpty

cDirectDeclaratorP :: PEParser CDirectDeclarator
cDirectDeclaratorP =
  parserUnion
    [ CDirectDeclaratorId <$>
        cIdentifierP <*>
        cDirectDeclaratorP'
    , CDirectDeclaratorParen <$>
        parenthesisP cDeclaratorP <*>
        cDirectDeclaratorP'
    ]

cDirectDeclaratorP' :: PEParser CDirectDeclarator'
cDirectDeclaratorP' =
  parserUnion
    [ CDirectDeclarator'ConstExpr <$>
        bracketP cConstantExpressionOptionalP <*>
        cDirectDeclaratorP'
    , CDirectDeclarator'ParamTypeList <$>
        parenthesisP cParameterTypeListP <*>
        cDirectDeclaratorP'
    , CDirectDeclarator'IdList <$>
        parenthesisP cIdentifierListOptionalP <*>
        cDirectDeclaratorP'
    , pure
        CDirectDeclarator'Empty
    ]

cPointerP :: PEParser CPointer
cPointerP =
  parserToPEParser
    (CPointer <$>
     (singleP LStar *> cTypeQualifierListOptionalP) <*>
     cPointerOptionalP)

cPointerOptionalP :: PEParser CPointerOptional
cPointerOptionalP =
  optionalP cPointerP CPointerOptional CPointerOptionalEmpty

cTypeQualifierListP :: PEParser CTypeQualifierList
cTypeQualifierListP =
  parserToPEParser
    (CTypeQualifierList <$> cTypeQualifierP <*> cTypeQualifierListOptionalP)

cTypeQualifierListOptionalP :: PEParser CTypeQualifierListOptional
cTypeQualifierListOptionalP =
  optionalP
    cTypeQualifierListP
    CTypeQualifierListOptional
    CTypeQualifierListOptionalEmpty

cParameterTypeListP :: PEParser CParameterTypeList
cParameterTypeListP =
  parserToPEParser
    (CParameterTypeList <$> cParameterListP <*> cVarArgsOptionalP)

cParameterTypeListOptionalP :: PEParser CParameterTypeListOptional
cParameterTypeListOptionalP =
  optionalP
    cParameterTypeListP
    CParameterTypeListOptional
    CParameterTypeListOptionalEmpty

cVarArgsOptionalP :: PEParser CVarArgsOptional
cVarArgsOptionalP =
  parserUnion
    [ CVarArgsOptional <$ (singleP LComma <* singleP LVarargs)
    , pure CVarArgsOptionalEmpty
    ]

cParameterListP :: PEParser CParameterList
cParameterListP =
  parserToPEParser
    (CParameterList <$> cParameterDeclarationP <*> cParameterListP')

cParameterListP' :: PEParser CParameterList'
cParameterListP' =
  parserUnion
    [ CParameterList' <$>
        (singleP LComma *> cParameterDeclarationP) <*>
        cParameterListP'
    , pure
        CParameterList'Empty
    ]

cParameterDeclarationP :: PEParser CParameterDeclaration
cParameterDeclarationP =
  parserToPEParser
    (CParameterDeclaration <$>
     cDeclarationSpecifiersP <*>
     cParameterDeclarationP')

cParameterDeclarationP' :: PEParser CParameterDeclaration'
cParameterDeclarationP' =
  parserUnion
    [ CParameterDeclaration' <$> cDeclaratorP
    , CParameterDeclaration'Abstract <$> cAbstractDeclaratorOptionalP
    ]

cIdentifierListP :: PEParser CIdentifierList
cIdentifierListP =
  parserToPEParser (CIdentifierList <$> cIdentifierP <*> cIdentifierListP')

cIdentifierListOptionalP :: PEParser CIdentifierListOptional
cIdentifierListOptionalP =
  optionalP
    cIdentifierListP
    CIdentifierListOptional
    CIdentifierListOptionalEmpty

cIdentifierListP' :: PEParser CIdentifierList'
cIdentifierListP' =
  parserUnion
    [ CIdentifierList' <$>
        (singleP LComma *> cIdentifierP) <*>
        cIdentifierListP'
    , pure
        CIdentifierList'Empty
    ]

cInitializerP :: PEParser CInitializer
cInitializerP =
  parserUnion
    [ CInitializerAssignment <$> cAssignmentExpressionP
    , CInitializerInitList <$> (braceP (cInitializerListP <* cCommaOptionalP))
    ]

cCommaOptionalP :: PEParser CLexeme
cCommaOptionalP =
  parserUnion
    [ singleP LComma
    , pure LComma
    ]

cInitializerListP :: PEParser CInitializerList
cInitializerListP =
  parserToPEParser (CInitializerList <$> cInitializerP <*> cInitializerListP')

cInitializerListP' :: PEParser CInitializerList'
cInitializerListP' =
  parserUnion
    [ CInitializerList' <$>
        (singleP LComma *> cInitializerP) <*>
        cInitializerListP'
    , pure
        CInitializerList'Empty
    ]

cTypeNameP :: PEParser CTypeName
cTypeNameP =
  parserToPEParser
    (CTypeName <$> cSpecifierQualifierListP <*> cAbstractDeclaratorOptionalP)

cAbstractDeclaratorP :: PEParser CAbstractDeclarator
cAbstractDeclaratorP =
  parserUnion
    [ CAbstractDeclaratorPointer <$>
        cPointerP
    , CAbstractDeclaratorDirect <$>
        cPointerOptionalP <*>
        cDirectAbstractDeclaratorP
    ]

cAbstractDeclaratorOptionalP :: PEParser CAbstractDeclaratorOptional
cAbstractDeclaratorOptionalP =
  optionalP
    cAbstractDeclaratorP
    CAbstractDeclaratorOptional
    CAbstractDeclaratorOptionalEmpty

cDirectAbstractDeclaratorP :: PEParser CDirectAbstractDeclarator
cDirectAbstractDeclaratorP =
  parserUnion
    [ CDirectAbstractDeclaratorParens <$>
        (parenthesisP cAbstractDeclaratorP) <*>
        cDirectAbstractDeclaratorP'
    , CDirectAbstractDeclaratorConst <$>
        (bracketP cConstantExpressionOptionalP) <*>
        cDirectAbstractDeclaratorP'
    , CDirectAbstractDeclaratorParams <$>
        (bracketP cParameterTypeListOptionalP) <*>
        cDirectAbstractDeclaratorP'
    ]

cDirectAbstractDeclaratorP' :: PEParser CDirectAbstractDeclarator'
cDirectAbstractDeclaratorP' =
  parserUnion
    [ CDirectAbstractDeclarator'Const <$>
        (bracketP cConstantExpressionOptionalP) <*>
        cDirectAbstractDeclaratorP'
    , CDirectAbstractDeclarator'Params <$>
        (bracketP cParameterTypeListOptionalP) <*>
        cDirectAbstractDeclaratorP'
    ]

cTypedefNameP :: PEParser CTypedefName
cTypedefNameP =
  parserToPEParser (CTypedefName <$> cIdentifierP)

cStatementP :: PEParser CStatement
cStatementP =
  parserUnion
    [ CStatementLabeled <$> cLabeledStatementP
    , CStatementExpression <$> cExpressionStatementP
    , CStatementCompound <$> cCompoundStatementP
    , CStatementSelection <$> cSelectionStatementP
    , CStatementIteration <$> cIterationStatementP
    , CStatementJump <$> cJumpStatementP
    ]

cLabeledStatementP :: PEParser CLabeledStatement
cLabeledStatementP =
  parserUnion
    [ CLabeledStatementId <$>
        cIdentifierP <*>
        (singleP LColon *> cStatementP)
    , CLabeledStatementCase <$>
        (singleP LCase *> cConstantExpressionP) <*>
        (singleP LColon *> cStatementP)
    , CLabeledStatementDefault <$>
        (singleP LDefault *> singleP LColon *> cStatementP)
    ]

cExpressionStatementP :: PEParser CExpressionStatement
cExpressionStatementP =
  parserToPEParser
    (CExpressionStatement <$> cExpressionOptionalP <* singleP LSemiColon)

cCompoundStatementP :: PEParser CCompoundStatement
cCompoundStatementP =
  braceP $ parserToPEParser
    (CCompoundStatement <$>
     cDeclarationListOptionalP <*>
     cStatementListOptionalP)

cStatementListP :: PEParser CStatementList
cStatementListP =
  parserToPEParser
    (CStatementList <$> cStatementP <*> cStatementListOptionalP)

cStatementListOptionalP :: PEParser CStatementListOptional
cStatementListOptionalP =
  optionalP cStatementListP CStatementListOptional CStatementListOptionalEmpty

cSelectionStatementP :: PEParser CSelectionStatement
cSelectionStatementP =
  parserUnion
    [ CSelectionStatementIf <$>
        (singleP LIf *> parenthesisP cExpressionP) <*>
        cStatementP <*>
        cElseOptionalP
    , CSelectionStatementSwitch <$>
        (singleP LSwitch *> parenthesisP cExpressionP) <*>
        cStatementP
    ]

cElseOptionalP :: PEParser CElseOptional
cElseOptionalP =
  parserUnion
    [ CElseOptional <$> (singleP LElse *> cStatementP)
    , pure CElseOptionalEmpty
    ]

cIterationStatementP :: PEParser CIterationStatement
cIterationStatementP =
  parserUnion
    [ CIterationStatementWhile <$>
        (singleP LWhile *> parenthesisP cExpressionP) <*>
        cStatementP
    , CIterationStatementDoWhile <$>
        (singleP LDo *> cStatementP) <*>
        (singleP LWhile *> parenthesisP cExpressionP <* singleP LSemiColon)
    , CIterationStatementFor <$>
        (singleP LFor *> singleP LParenthesisOpen *> cExpressionOptionalP) <*>
        (singleP LSemiColon *> cExpressionOptionalP) <*>
        (singleP LSemiColon *> cExpressionOptionalP) <*>
        (singleP LParenthesisClose *> cStatementP)
    ]

cJumpStatementP :: PEParser CJumpStatement
cJumpStatementP =
  parserUnion
    [ CJumpStatementGoto <$>
        (singleP LGoto *> cIdentifierP <* singleP LSemiColon)
    , CJumpStatementContinue <$
        (singleP LContinue <* singleP LSemiColon)
    , CJumpStatementBreak <$
        (singleP LBreak <* singleP LSemiColon)
    , CJumpStatementReturn <$>
        (singleP LReturn *> cExpressionOptionalP <* singleP LSemiColon)
    ]


cExpressionP :: PEParser CExpression
cExpressionP =
  parserToPEParser (CExpression <$> cAssignmentExpressionP <*> cExpressionP')

cExpressionOptionalP :: PEParser CExpressionOptional
cExpressionOptionalP =
  optionalP cExpressionP CExpressionOptional CExpressionOptionalEmpty

cExpressionP' :: PEParser CExpression'
cExpressionP' =
  parserUnion
    [ CExpression' <$>
        (singleP LComma *> cAssignmentExpressionP) <*>
        cExpressionP'
    , pure
        CExpression'Empty
    ]

cAssignmentExpressionP :: PEParser CAssignmentExpression
cAssignmentExpressionP =
  parserUnion
    [ CAssignmentExpression <$>
        cUnaryExpressionP <*>
        cAssignmentOperatorP <*>
        cAssignmentExpressionP
    , CAssignmentExpressionConditional <$>
        cConditionalExpressionP
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

cConditionalExpressionP :: PEParser CConditionalExpression
cConditionalExpressionP =
  parserToPEParser
    (CConditionalExpression <$> cLogicalOrExpressionP <*> cTernaryOptionalP)

cTernaryOptionalP :: PEParser CTernaryOptional
cTernaryOptionalP =
  parserUnion
    [ CTernaryOptional <$>
        (singleP LTernary *> cExpressionP) <*>
        (singleP LColon *> cConditionalExpressionP)
    , pure
        CTernaryOptionalEmpty
    ]

cConstantExpressionP :: PEParser CConstantExpression
cConstantExpressionP =
  parserToPEParser (CConstantExpression <$> cConditionalExpressionP)

cConstantExpressionOptionalP :: PEParser CConstantExpressionOptional
cConstantExpressionOptionalP =
  optionalP
    cConstantExpressionP
    CConstantExpressionOptional
    CConstantExpressionOptionalEmpty

cLogicalOrExpressionP :: PEParser CLogicalOrExpression
cLogicalOrExpressionP =
  parserToPEParser
    (CLogicalOrExpression <$>
     cLogicalAndExpressionP <*>
     cLogicalOrExpressionP')

cLogicalOrExpressionP' :: PEParser CLogicalOrExpression'
cLogicalOrExpressionP' =
  parserUnion
    [ CLogicalOrExpression' <$>
        (singleP LOr *> cLogicalAndExpressionP) <*>
        cLogicalOrExpressionP'
    , pure
        CLogicalOrExpression'Empty
    ]

cLogicalAndExpressionP :: PEParser CLogicalAndExpression
cLogicalAndExpressionP =
  parserToPEParser
    (CLogicalAndExpression <$>
     cInclusiveOrExpressionP <*>
     cLogicalAndExpressionP')

cLogicalAndExpressionP' :: PEParser CLogicalAndExpression'
cLogicalAndExpressionP' =
  parserUnion
    [ CLogicalAndExpression' <$>
        (singleP LAnd *> cInclusiveOrExpressionP) <*>
        cLogicalAndExpressionP'
    , pure
        CLogicalAndExpression'Empty
    ]

cInclusiveOrExpressionP :: PEParser CInclusiveOrExpression
cInclusiveOrExpressionP =
  parserToPEParser
    (CInclusiveOrExpression <$>
     cExclusiveOrExpressionP <*>
     cInclusiveOrExpressionP')

cInclusiveOrExpressionP' :: PEParser CInclusiveOrExpression'
cInclusiveOrExpressionP' =
  parserUnion
    [ CInclusiveOrExpression' <$>
        (singleP LBitwiseOr *> cExclusiveOrExpressionP) <*>
        cInclusiveOrExpressionP'
    , pure
        CInclusiveOrExpression'Empty
    ]

cExclusiveOrExpressionP :: PEParser CExclusiveOrExpression
cExclusiveOrExpressionP =
  parserToPEParser
    (CExclusiveOrExpression <$> cAndExpressionP <*> cExclusiveOrExpressionP')

cExclusiveOrExpressionP' :: PEParser CExclusiveOrExpression'
cExclusiveOrExpressionP' =
  parserUnion
    [ CExclusiveOrExpression' <$>
        (singleP LBitwiseXor *> cAndExpressionP) <*>
        cExclusiveOrExpressionP'
    , pure
        CExclusiveOrExpression'Empty
    ]

cAndExpressionP :: PEParser CAndExpression
cAndExpressionP =
  parserToPEParser
    (CAndExpression <$> cEqualityExpressionP <*> cAndExpressionP')

cAndExpressionP' :: PEParser CAndExpression'
cAndExpressionP' =
  parserUnion
    [ CAndExpression' <$>
        (singleP LAmp *> cEqualityExpressionP) <*>
        cAndExpressionP'
    , pure
        CAndExpression'Empty
    ]

cEqualityExpressionP :: PEParser CEqualityExpression
cEqualityExpressionP =
  parserToPEParser
    (CEqualityExpression <$> cRelationalExpressionP <*> cEqualityExpressionP')

cEqualityExpressionP' :: PEParser CEqualityExpression'
cEqualityExpressionP' =
  parserUnion
    [ CEqualityExpression'EQ <$>
        (singleP LEquals *> cRelationalExpressionP) <*>
        cEqualityExpressionP'
    , CEqualityExpression'NEQ  <$>
        (singleP LNotEquals *> cRelationalExpressionP) <*>
        cEqualityExpressionP'
    , pure
        CEqualityExpression'Empty
    ]

cRelationalExpressionP :: PEParser CRelationalExpression
cRelationalExpressionP =
  parserToPEParser
    (CRelationalExpression <$> cShiftExpressionP <*> cRelationalExpressionP')

cRelationalExpressionP' :: PEParser CRelationalExpression'
cRelationalExpressionP' =
  parserUnion
    [ CRelationalExpression'LT <$>
        (singleP LLT *> cShiftExpressionP) <*>
        cRelationalExpressionP'
    , CRelationalExpression'LTE <$>
        (singleP LLTE *> cShiftExpressionP) <*>
        cRelationalExpressionP'
    , CRelationalExpression'GT <$>
        (singleP LGT *> cShiftExpressionP) <*>
        cRelationalExpressionP'
    , CRelationalExpression'GTE <$>
        (singleP LGTE *> cShiftExpressionP) <*>
        cRelationalExpressionP'
    , pure
        CRelationalExpression'Empty
    ]

cShiftExpressionP :: PEParser CShiftExpression
cShiftExpressionP =
  parserToPEParser
    (CShiftExpression <$> cAdditiveExpressionP <*> cShiftExpressionP')

cShiftExpressionP' :: PEParser CShiftExpression'
cShiftExpressionP' =
  parserUnion
    [ CShiftExpression'Left <$>
        (singleP LBitShiftLeft *> cAdditiveExpressionP) <*>
        cShiftExpressionP'
    , CShiftExpression'Right <$>
        (singleP LBitShiftRight *> cAdditiveExpressionP) <*>
        cShiftExpressionP'
    , pure
        CShiftExpression'Empty
    ]

cAdditiveExpressionP :: PEParser CAdditiveExpression
cAdditiveExpressionP =
  parserToPEParser
    (CAdditiveExpression <$>
     cMultiplicativeExpressionP <*>
     cAdditiveExpressionP')

cAdditiveExpressionP' :: PEParser CAdditiveExpression'
cAdditiveExpressionP' =
  parserUnion
    [ CAdditiveExpression'Add <$>
        (singleP LPlus *> cMultiplicativeExpressionP) <*>
        cAdditiveExpressionP'
    , CAdditiveExpression'Sub <$>
        (singleP LMinus *> cMultiplicativeExpressionP) <*>
        cAdditiveExpressionP'
    , pure
        CAdditiveExpression'Empty
    ]

cMultiplicativeExpressionP :: PEParser CMultiplicativeExpression
cMultiplicativeExpressionP =
  parserToPEParser
    (CMultiplicativeExpression <$>
     cCastExpressionP <*>
     cMultiplicativeExpressionP')

cMultiplicativeExpressionP' :: PEParser CMultiplicativeExpression'
cMultiplicativeExpressionP' =
  parserUnion
    [ CMultiplicativeExpression'Mul <$>
        (singleP LStar *> cCastExpressionP) <*>
        cMultiplicativeExpressionP'
    , CMultiplicativeExpression'Div <$>
        (singleP LDivision *> cCastExpressionP) <*>
        cMultiplicativeExpressionP'
    , CMultiplicativeExpression'Mod <$>
        (singleP LModulo *> cCastExpressionP) <*>
        cMultiplicativeExpressionP'
    , pure
        CMultiplicativeExpression'Empty
    ]

cCastExpressionP :: PEParser CCastExpression
cCastExpressionP =
  parserUnion
    [ CCastExpression <$> parenthesisP cTypeNameP <*> cCastExpressionP
    , CCastExpressionUnary <$> cUnaryExpressionP
    ]

cUnaryExpressionP :: PEParser CUnaryExpression
cUnaryExpressionP =
  parserUnion
    [ CUnaryExpressionInc <$>
        (singleP LIncrement *> cUnaryExpressionP)
    , CUnaryExpressionDec <$>
        (singleP LDecrement *> cUnaryExpressionP)
    , CUnaryExpressionUnaryOp <$>
        cUnaryOperatorP <*>
        cCastExpressionP
    , CUnaryExpressionSizeof <$>
        (singleP LSizeof *> cUnaryExpressionP)
    , CUnaryExpressionSizeofType <$>
        (singleP LSizeof *> parenthesisP cTypeNameP)
    , CUnaryExpressionPostfix <$>
        cPostfixExpressionP
    ]

cUnaryOperatorP :: PEParser CUnaryOperator
cUnaryOperatorP =
  parserUnion
    [ CUnaryOperatorAnd <$ singleP LAmp
    , CUnaryOperatorMul <$ singleP LStar
    , CUnaryOperatorAdd <$ singleP LPlus
    , CUnaryOperatorSub <$ singleP LMinus
    , CUnaryOperatorBitwiseNot <$ singleP LBitwiseNot
    , CUnaryOperatorNot <$ singleP LNot
    ]

cPostfixExpressionP :: PEParser CPostfixExpression
cPostfixExpressionP =
  parserToPEParser
    (CPostfixExpression <$> cPrimaryExpressionP <*> cPostfixExpressionP')

cPostfixExpressionP' :: PEParser CPostfixExpression'
cPostfixExpressionP' =
  parserUnion
    [ CPostfixExpression'Bracket <$>
        bracketP cExpressionP <*>
        cPostfixExpressionP'
    , CPostfixExpression'Paren <$>
        parenthesisP cArgumentExpressionListOptionalP <*>
        cPostfixExpressionP'
    , CPostfixExpression'Dot <$>
        (singleP LDot *> cIdentifierP) <*>
        cPostfixExpressionP'
    , CPostfixExpression'Arrow <$>
        (singleP LArrow *> cIdentifierP) <*>
        cPostfixExpressionP'
    , CPostfixExpression'Inc <$>
        (singleP LIncrement *> cPostfixExpressionP')
    , CPostfixExpression'Dec <$>
        (singleP LDecrement *> cPostfixExpressionP')
    , pure
        CPostfixExpression'Empty
    ]

cPrimaryExpressionP :: PEParser CPrimaryExpression
cPrimaryExpressionP =
  parserUnion
    [ CPrimaryExpressionId <$> cIdentifierP
    , CPrimaryExpressionConst <$> cConstantP
    , CPrimaryExpressionString <$> stringLiteralP
    , CPrimaryExpressionParen <$> parenthesisP cExpressionP
    ]

cArgumentExpressionListP :: PEParser CArgumentExpressionList
cArgumentExpressionListP =
  parserToPEParser
    (CArgumentExpressionList <$>
     cAssignmentExpressionP <*>
     cArgumentExpressionListP')

cArgumentExpressionListOptionalP :: PEParser CArgumentExpressionListOptional
cArgumentExpressionListOptionalP =
  optionalP
    cArgumentExpressionListP
    CArgumentExpressionListOptional
    CArgumentExpressionListOptionalEmpty

cArgumentExpressionListP' :: PEParser CArgumentExpressionList'
cArgumentExpressionListP' =
  parserUnion
    [ CArgumentExpressionList' <$>
        (singleP LComma *> cAssignmentExpressionP) <*>
        cArgumentExpressionListP'
    , pure
        CArgumentExpressionList'Empty
    ]

cConstantP :: PEParser CConstant
cConstantP =
  parserUnion
    [ CConstantInt <$> intLiteralP
    , CConstantChar <$> charLiteralP
    , CConstantFloat <$> floatLiteralP
    , CConstantEnum <$> cIdentifierP
    ]

