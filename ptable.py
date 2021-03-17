from functools import reduce

INT = 1
STRING = 2
FLOAT = 3
CHAR = 4

class Terminal:
    def __eq__(self, item):
        if isinstance(item, Terminal):
            return self.value == item.value
        return False

    def __hash__(self):
        return hash(self.__repr__())


class Identifier(Terminal):
    def __repr__(self):
        return self.__class__.__name__


class KeyWord(Terminal):
    def __init__(self, word):
        self.value = word

    def __repr__(self):
        return f"{self.__class__.__name__}('{self.value}')"


class Literal(Terminal):
    def __init__(self, t):
        self.value = t

    def __repr__(self):
        return f"{self.__class__.__name__}({self.value})"

productions = [
    ('CIdentifier', [Identifier()]),
    ('CIdentifierOptional', ['CIdentifier']),
    ('CIdentifierOptional', []),
    ('CConstant', [Literal('FLOAT')]),
    ('CConstant', [Literal('INT')]),
    ('CConstant', ['CEnumerationConstant']),
    ('CConstant', [Literal('CHAR')]),
    ('CPrimaryExpression', ['CIdentifier']),
    ('CPrimaryExpression', ['CConstant']),
    ('CPrimaryExpression', [Literal('STRING')]),
    ('CPrimaryExpression', [KeyWord('('), 'CExpression', KeyWord(')')]),
    ('CPostfixExpression', ['CPrimaryExpression', 'CPostfixExpression\'']),
    ('CPostfixExpression\'', []),
    ('CPostfixExpression\'', [KeyWord('['), 'CPostfixExpression\'', KeyWord(']')]),
    ('CPostfixExpression\'', [KeyWord('('), 'CArgumentExpressionListOptional', KeyWord(')'), 'CPostfixExpression\'']),
    ('CPostfixExpression\'', [KeyWord('.'), 'CIdentifier', 'CPostfixExpression\'']),
    ('CPostfixExpression\'', [KeyWord('->'), 'CIdentifier', 'CPostfixExpression\'']),
    ('CPostfixExpression\'', [KeyWord('++'), 'CPostfixExpression\'']),
    ('CPostfixExpression\'', [KeyWord('--'), 'CPostfixExpression\'']),
    ('CArgumentExpressionList', ['CAssignmentExpression', 'CArgumentExpressionList\'']),
    ('CArgumentExpressionListOptional', ['CArgumentExpressionList']),
    ('CArgumentExpressionListOptional', []),
    ('CArgumentExpressionList\'', []),
    ('CArgumentExpressionList\'', [KeyWord('.'), 'CAssignmentExpression', 'CArgumentExpressionList\'']),
    ('CUnaryExpression', ['CPostfixExpression']),
    ('CUnaryExpression', [KeyWord('++'), 'CUnaryExpression']),
    ('CUnaryExpression', [KeyWord('--'), 'CUnaryExpression']),
    ('CUnaryExpression', ['CUnaryOperator', 'CCastExpression']),
    ('CUnaryExpression', [KeyWord('sizeof'), 'CUnaryExpression']),
    ('CUnaryExpression', [KeyWord('sizeof'), KeyWord('('), 'CTypeName', KeyWord(')')]),
    ('CUnaryOperator', [KeyWord('&')]),
    ('CUnaryOperator', [KeyWord('*')]),
    ('CUnaryOperator', [KeyWord('+')]),
    ('CUnaryOperator', [KeyWord('-')]),
    ('CUnaryOperator', [KeyWord('~')]),
    ('CUnaryOperator', [KeyWord('!')]),
    ('CCastExpression', ['CUnaryExpression']),
    ('CCastExpression', [KeyWord('('), 'CTypeName', KeyWord(')'), 'CCastExpression']),
    ('CMultiplicativeExpression', ['CCastExpression', 'CMultiplicativeExpression\'']),
    ('CMultiplicativeExpression\'', []),
    ('CMultiplicativeExpression\'', [KeyWord('*'), 'CCastExpression', 'CMultiplicativeExpression\'']),
    ('CMultiplicativeExpression\'', [KeyWord('/'), 'CCastExpression', 'CMultiplicativeExpression\'']),
    ('CMultiplicativeExpression\'', [KeyWord('%'), 'CCastExpression', 'CMultiplicativeExpression\'']),
    ('CAdditiveExpression', ['CMultiplicativeExpression', 'CAdditiveExpression\'']),
    ('CAdditiveExpression\'', []),
    ('CAdditiveExpression\'', [KeyWord('+'), 'CMultiplicativeExpression', 'CAdditiveExpression\'']),
    ('CAdditiveExpression\'', [KeyWord('-'), 'CMultiplicativeExpression', 'CAdditiveExpression\'']),
    ('CShiftExpression', ['CAdditiveExpression', 'CShiftExpression\'']),
    ('CShiftExpression\'', []),
    ('CShiftExpression\'', [KeyWord('<<'), 'CAdditiveExpression', 'CShiftExpression\'']),
    ('CShiftExpression\'', [KeyWord('>>'), 'CAdditiveExpression', 'CShiftExpression\'']),
    ('CRelationalExpression', ['CShiftExpression', 'CRelationalExpression\'']),
    ('CRelationalExpression\'', []),
    ('CRelationalExpression\'', [KeyWord('<'), 'CShiftExpression', 'CRelationalExpression\'']),
    ('CRelationalExpression\'', [KeyWord('>'), 'CShiftExpression', 'CRelationalExpression\'']),
    ('CRelationalExpression\'', [KeyWord('<='), 'CShiftExpression', 'CRelationalExpression\'']),
    ('CRelationalExpression\'', [KeyWord('>='), 'CShiftExpression', 'CRelationalExpression\'']),
    ('CEqualityExpression', ['CRelationalExpression', 'CEqualityExpression\'']),
    ('CEqualityExpression\'', []),
    ('CEqualityExpression\'', [KeyWord('=='), 'CRelationalExpression', 'CEqualityExpression\'']),
    ('CEqualityExpression\'', [KeyWord('!='), 'CRelationalExpression', 'CEqualityExpression\'']),
    ('CAndExpression', ['CEqualityExpression', 'CAndExpression\'']),
    ('CAndExpression\'', []),
    ('CAndExpression\'', [KeyWord('&'), 'CEqualityExpression', 'CAndExpression\'']),
    ('CExclusiveOrExpression', ['CAndExpression', 'CExclusiveOrExpression\'']),
    ('CExclusiveOrExpression\'', []),
    ('CExclusiveOrExpression\'', [KeyWord('^'), 'CAndExpression', 'CExclusiveOrExpression\'']),
    ('CInclusiveOrExpression', ['CExclusiveOrExpression', 'CInclusiveOrExpression\'']),
    ('CInclusiveOrExpression\'', []),
    ('CInclusiveOrExpression\'', [KeyWord('|'), 'CExclusiveOrExpression', 'CInclusiveOrExpression\'']),
    ('CLogicalAndExpression', ['CInclusiveOrExpression', 'CLogicalAndExpression\'']),
    ('CLogicalAndExpression\'', []),
    ('CLogicalAndExpression\'', [KeyWord('&&'), 'CInclusiveOrExpression', 'CLogicalAndExpression\'']),
    ('CLogicalOrExpression', ['CLogicalAndExpression', 'CLogicalOrExpression\'']),
    ('CLogicalOrExpression\'', []),
    ('CLogicalOrExpression\'', [KeyWord('||'), 'CInclusiveOrExpression', 'CLogicalAndExpression\'']),
    ('CConditionalExpression', ['CLogicalOrExpression']),
    ('CConditionalExpression', ['CLogicalOrExpression', KeyWord('?'), 'CExpression', KeyWord(':'), 'CConditionalExpression']),
    ('CAssignmentExpression', ['CConditionalExpression']),
    ('CAssignmentExpression', ['CUnaryExpression', 'CAssignmentOperator', 'CAssignmentExpression']),
    ('CAssignmentOperator', [KeyWord('=')]),
    ('CAssignmentOperator', [KeyWord('*=')]),
    ('CAssignmentOperator', [KeyWord('/=')]),
    ('CAssignmentOperator', [KeyWord('%=')]),
    ('CAssignmentOperator', [KeyWord('+=')]),
    ('CAssignmentOperator', [KeyWord('-=')]),
    ('CAssignmentOperator', [KeyWord('<<=')]),
    ('CAssignmentOperator', [KeyWord('>>=')]),
    ('CAssignmentOperator', [KeyWord('&=')]),
    ('CAssignmentOperator', [KeyWord('^=')]),
    ('CAssignmentOperator', [KeyWord('|=')]),
    ('CExpression', ['CAssignmentExpression', 'CExpression\'']),
    ('CExpression\'', []),
    ('CExpression\'', [KeyWord(','), 'CAssignmentExpression', 'CExpression\'']),
    ('CExpressionOptional', ['CExpression']),
    ('CExpressionOptional', []),
    ('CConstantExpression', ['CConditionalExpression']),
    ('CConstantExpressionOptional', []),
    ('CConstantExpressionOptional', ['CConstantExpression']),
    ('CDeclaration', ['CDeclarationSpecifiers', 'CInitDeclaratorListOptional']),
    ('CDeclarationSpecifiers', ['CStorageClassSpecifier', 'CDeclarationSpecifiersOptional']),
    ('CDeclarationSpecifiers', ['CTypeSpecifier', 'CDeclarationSpecifiersOptional']),
    ('CDeclarationSpecifiers', ['CTypeQualifier', 'CDeclarationSpecifiersOptional']),
    ('CDeclarationSpecifiersOptional', ['CDeclarationSpecifiers']),
    ('CDeclarationSpecifiersOptional', []),
    ('CInitDeclaratorList', ['CInitDeclarator', 'CInitDeclaratorList\'']),
    ('CInitDeclaratorListOptional', ['CInitDeclaratorList']),
    ('CInitDeclaratorListOptional', []),
    ('CInitDeclaratorList\'', []),
    ('CInitDeclaratorList\'', [KeyWord(','), 'CInitDeclarator', 'CInitDeclaratorList\'']),
    ('CInitDeclarator', ['CDeclarator']),
    ('CInitDeclarator', ['CDeclarator', KeyWord('='), 'CInitializer']),
    ('CStorageClassSpecifier', [KeyWord('typedef')]),
    ('CStorageClassSpecifier', [KeyWord('extern')]),
    ('CStorageClassSpecifier', [KeyWord('static')]),
    ('CStorageClassSpecifier', [KeyWord('auto')]),
    ('CStorageClassSpecifier', [KeyWord('register')]),
    ('CTypeSpecifier', [KeyWord('void')]),
    ('CTypeSpecifier', [KeyWord('char')]),
    ('CTypeSpecifier', [KeyWord('short')]),
    ('CTypeSpecifier', [KeyWord('int')]),
    ('CTypeSpecifier', [KeyWord('long')]),
    ('CTypeSpecifier', [KeyWord('float')]),
    ('CTypeSpecifier', [KeyWord('double')]),
    ('CTypeSpecifier', [KeyWord('signed')]),
    ('CTypeSpecifier', [KeyWord('unsigned')]),
    ('CTypeSpecifier', ['CStructOrUnionSpecifier']),
    ('CTypeSpecifier', ['CEnumSpecifier']),
    ('CTypeSpecifier', ['CTypedefName']),
    ('CStructOrUnionSpecifier', ['CStructOrUnion', 'CIdentifierOptional', KeyWord('{'), 'CStructDeclarationList', KeyWord('}')]),
    ('CStructOrUnionSpecifier', ['CStructOrUnion', 'CIdentifier']),
    ('CStructOrUnion', [KeyWord('struct')]),
    ('CStructOrUnion', [KeyWord('union')]),
    ('CStructDeclarationList', ['CStructDeclaration']),
    ('CStructDeclarationList', ['CStructDeclaration', 'CStructDeclarationList']),
    ('CStructDeclaration', ['CSpecifierQualifierList', 'CStructDeclaratorList', KeyWord(';')]),
    ('CSpecifierQualifierList', ['CTypeSpecifier', 'CSpecifierQualifierListOptional']),
    ('CSpecifierQualifierList', ['CTypeQualifier', 'CSpecifierQualifierListOptional']),
    ('CSpecifierQualifierListOptional', ['CSpecifierQualifierList']),
    ('CSpecifierQualifierListOptional', []),
    ('CStructDeclaratorList', ['CStructDeclarator', 'CStructDeclaratorList\'']),
    ('CStructDeclaratorList\'', [KeyWord(','), 'CStructDeclarator', 'CStructDeclaratorList\'']),
    ('CStructDeclaratorList\'', []),
    ('CStructDeclarator', ['CDeclarator']),
    ('CStructDeclarator', ['CDeclaratorOptional', KeyWord(':'), 'CConstantExpression']),
    ('CEnumSpecifier', [KeyWord('enum'), 'CIdentifierOptional', KeyWord('{'), 'CEnumeratorList', KeyWord('}')]),
    ('CEnumSpecifier', [KeyWord('enum'), 'CIdentifier']),
    ('CEnumeratorList', ['CEnumerator', 'CEnumeratorList\'']),
    ('CEnumeratorList\'', []),
    ('CEnumeratorList\'', [KeyWord(','), 'CEnumerator', 'CEnumeratorList\'']),
    ('CEnumerator', ['CEnumerationConstant']),
    ('CEnumerator', ['CEnumerationConstant', KeyWord('='), 'CConstantExpression']),
    ('CEnumerationConstant', ['CIdentifier']),
    ('CTypeQualifier', [KeyWord('const')]),
    ('CTypeQualifier', [KeyWord('volatile')]),
    ('CDeclarator', ['CPointerOptional', 'CDirectDeclarator']),
    ('CDeclaratorOptional', ['CDeclarator']),
    ('CDeclaratorOptional', []),
    ('CDirectDeclarator', ['CIdentifier', 'CDirectDeclarator\'']),
    ('CDirectDeclarator', [KeyWord('('), 'CDeclarator', KeyWord(')'), 'CDirectDeclarator\'']),
    ('CDirectDeclarator\'', []),
    ('CDirectDeclarator\'', [KeyWord('['), 'CConstantExpressionOptional', KeyWord(']'), 'CDirectDeclarator\'']),
    ('CDirectDeclarator\'', [KeyWord('['), 'CParameterTypeList', KeyWord(']'), 'CDirectDeclarator\'']),
    ('CDirectDeclarator\'', [KeyWord('['), 'CIdentifierListOptional', KeyWord(']'), 'CDirectDeclarator\'']),
    ('CPointer', [KeyWord('*'), 'CTypeQualifierListOptional']),
    ('CPointer', [KeyWord('*'), 'CTypeQualifierListOptional', 'CPointer']),
    ('CPointerOptional', []),
    ('CPointerOptional', ['CPointer']),
    ('CTypeQualifierList', ['CTypeQualifier']),
    ('CTypeQualifierList', ['CTypeQualifierList']),
    ('CTypeQualifierListOptional', ['CTypeQualifierList']),
    ('CTypeQualifierListOptional', []),
    ('CParameterTypeList', ['CParameterList']),
    ('CParameterTypeList', ['CParameterList', KeyWord(','), KeyWord('...')]),
    ('CParameterTypeListOptional', []),
    ('CParameterTypeListOptional', ['CParameterTypeList']),
    ('CParameterList', ['CParameterDeclaration', 'CParameterList\'']),
    ('CParameterList\'', []),
    ('CParameterList\'', [KeyWord(','), 'CParameterDeclaration', 'CParameterList\'']),
    ('CParameterDeclaration', ['CDeclarationSpecifiers', 'CDeclarator']),
    ('CParameterDeclaration', ['CDeclarationSpecifiers', 'CAbstractDeclaratorOptional']),
    ('CIdentifierList', ['CIdentifier', 'CIdentifierList\'']),
    ('CIdentifierList\'', []),
    ('CIdentifierList\'', [KeyWord(','), 'CIdentifier', 'CIdentifierList\'']),
    ('CIdentifierListOptional', ['CIdentifierList']),
    ('CIdentifierListOptional', []),
    ('CTypeName', ['CSpecifierQualifierList', 'CAbstractDeclaratorOptional']),
    ('CAbstractDeclarator', ['CPointer']),
    ('CAbstractDeclarator', ['CPointerOptional', 'CDirectAbstractDeclarator']),
    ('CAbstractDeclaratorOptional', ['CAbstractDeclarator']),
    ('CAbstractDeclaratorOptional', []),
    ('CDirectAbstractDeclarator', [KeyWord('('), 'CAbstractDeclarator', KeyWord(')'), 'CDirectAbstractDeclarator\'']),
    ('CDirectAbstractDeclarator', [KeyWord('['), 'CConstantExpressionOptional', KeyWord(']'), 'CDirectAbstractDeclarator\'']),
    ('CDirectAbstractDeclarator', [KeyWord('('), 'CParameterTypeList', KeyWord(')'), 'CDirectAbstractDeclarator\'']),
    ('CDirectAbstractDeclarator\'', []),
    ('CDirectAbstractDeclarator\'', [KeyWord('['), 'CConstantExpressionOptional', KeyWord(']')]),
    ('CDirectAbstractDeclarator\'', [KeyWord('('), 'CParameterTypeListOptional', KeyWord(')')]),
    ('CTypedefName', ['CIdentifier']),
    ('CInitializer', ['CAssignmentExpression']),
    ('CInitializer', [KeyWord('{'), 'CInitializerList', KeyWord(','), KeyWord('}')]),
    ('CInitializer', [KeyWord('{'), 'CInitializerList', KeyWord('}')]),
    ('CInitializerList', ['CInitializer', 'CInitializerList\'']),
    ('CInitializerList\'', []),
    ('CInitializerList\'', [KeyWord(','), 'CInitializer', 'CInitializerList\'']),
    ('CStatement', ['CLabeledStatement']),
    ('CStatement', ['CCompoundStatement']),
    ('CStatement', ['CExpressionStatement']),
    ('CStatement', ['CSelectionStatement']),
    ('CStatement', ['CIterationStatement']),
    ('CStatement', ['CJumpStatement']),
    ('CLabeledStatement', ['CIdentifier', KeyWord(':'), 'CStatement']),
    ('CLabeledStatement', [KeyWord('case'), 'CConstantExpression', KeyWord(':'), 'CStatement']),
    ('CLabeledStatement', [KeyWord('default'), KeyWord(':'), 'CStatement']),
    ('CCompoundStatement', [KeyWord('{'), 'CDeclarationListOptional', 'CStatementListOptional', KeyWord('}')]),
    ('CDeclarationList', ['CDeclaration']),
    ('CDeclarationList', ['CDeclarationList', 'CDeclaration']),
    ('CDeclarationListOptional', ['CDeclarationList']),
    ('CDeclarationListOptional', []),
    ('CStatementList', ['CStatement']),
    ('CStatementList', ['CStatementList', 'CStatement']),
    ('CStatementListOptional', ['CStatementList']),
    ('CStatementListOptional', []),
    ('CExpressionStatement', ['CExpressionOptional', KeyWord(';')]),
    ('CSelectionStatement', [KeyWord('if'), KeyWord('('), 'CExpression', KeyWord(')'), 'CStatement']),
    ('CSelectionStatement', [KeyWord('if'), KeyWord('('), 'CExpression', KeyWord(')'), 'CStatement', KeyWord('else'), 'CStatement']),
    ('CSelectionStatement', [KeyWord('switch'), KeyWord('('), 'CExpression', KeyWord(')'), 'CStatement']),
    ('CIterationStatement', [KeyWord('while'), KeyWord('('), 'CExpression', KeyWord(')'), 'CStatement']),
    ('CIterationStatement', [KeyWord('do'), 'CStatement', KeyWord('while'), KeyWord('('), 'CExpression', KeyWord(')'), KeyWord(';')]),
    ('CIterationStatement', [KeyWord('for'), KeyWord('('), 'CExpressionOptional', KeyWord(';'), 'CExpressionOptional', KeyWord(';'), 'CExpressionOptional', 'CStatement']),
    ('CJumpStatement', [KeyWord('goto'), 'CIdentifier', KeyWord(';')]),
    ('CJumpStatement', [KeyWord('continue'), KeyWord(';')]),
    ('CJumpStatement', [KeyWord('break'), KeyWord(';')]),
    ('CJumpStatement', [KeyWord('return'), 'CExpressionOptional', KeyWord(';')]),
    ('CTranslationUnit', ['CExternalDeclaration']),
    ('CTranslationUnit', ['CExternalDeclaration', 'CTranslationUnit']),
    ('CExternalDeclaration', ['CFunctionDefinition']),
    ('CExternalDeclaration', ['CDeclaration']),
    ('CFunctionDefinition', ['CDeclarationSpecifiersOptional', 'CDeclarator']),
    ('CFunctionDefinition', ['CDeclarationListOptional', 'CCompoundStatement']),
]

def first(first_sets):
    changed = False
    for nt, sequence in productions:
        if not sequence:
            if None not in first_sets[nt]:
                first_sets[nt].add(None)
                changed = True
            continue
        if isinstance(sequence[0], Terminal):
            if sequence[0] not in first_sets[nt]:
                changed = True
                first_sets[nt].add(sequence[0])
            continue
        rhs = first_sets[sequence[0]]
        previous = sequence[0]
        for item in sequence[1:]:
            if isinstance(previous, Terminal):
                rhs = rhs.union(set([previous]))
                break
            if None in first_sets[previous]:
                if isinstance(item, Terminal):
                    rhs = rhs.union(set([item]))
                    break
                rhs = rhs.union(first_sets[item] - set([None]))
            else:
                break
            previous = item
        if not any(isinstance(x, Terminal) for x in sequence) \
            and all(None in first_sets[x] for x in sequence):
            rhs.add(None)
        if rhs - first_sets[nt]:
            changed = True
            first_sets[nt] = first_sets[nt].union(rhs)
    if changed:
        return first(first_sets)
    return first_sets

def follow(follow_sets, first_sets):
    changed = False
    for nt, sequence in productions:
        if not sequence:
            continue
        trailer = follow_sets[nt]
        for item in reversed(sequence):
            if isinstance(item, Terminal):
                trailer = set([item])
                continue
            if trailer - follow_sets[item]:
                changed = True
                follow_sets[item] = follow_sets[item].union(trailer)
            if None in first_sets[item]:
                trailer = trailer.union(first_sets[item] - set([None]))
            else:
                trailer = first_sets[item]
    if changed:
        return follow(follow_sets, first_sets)
    return follow_sets

def first_plus(first_sets, follow_sets):
    for nt, sequence in productions:
        if not any(isinstance(item, Terminal) for item in sequence) \
            and all(None in first_sets[item] for item in sequence):
            yield reduce(set.union, sequence, follow_sets[nt])
            continue
        s = set()
        for item in sequence:
            if isinstance(item, Terminal):
                s.add(item)
                break
            s = s.union(first_sets[item] - set([None]))
            if None not in first_sets[item]:
                break
        yield s

_first_sets = {x: set() for x, _ in productions}

_follow_sets = {x: set() for x, _ in productions}
_follow_sets['CTranslationUnit'].add('EOF')

first_sets = first(_first_sets)
follow_sets = follow(_follow_sets, first_sets)

first_plus_sets = list(first_plus(first_sets, follow_sets))
nts = {nt: [] for nt, _ in productions}
for (nt, seq), fpset in zip(productions, first_plus_sets):
    nts[nt].append((fpset, seq))

for nt, sets in nts.items():
    for s1, seq1 in sets:
        for s2, seq2 in sets:
            if set.intersection(s1, s2) and (seq1 != seq2) and (seq1.__repr__() < seq2.__repr__()):
                print(nt)
                print(seq1)
                print(seq2)

print()
print()
print()
print()

for (nt, seq), s in zip(productions, first_plus_sets):
    if nt == 'CPostfixExpression\'':
        print(seq, s)


