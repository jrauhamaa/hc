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
    ('CTranslationUnit', ['CExternalDeclaration']),
    ('CTranslationUnit', ['CTranslationUnit', 'CExternalDeclaration']),
    ('CExternalDeclaration', ['CFunctionDefinition']),
    ('CExternalDeclaration', ['CDeclaration']),
    ('CFunctionDefinition', ['CDeclarationSpecifiersOptional', 'CDeclarator', 'CDeclarationListOptional', 'CCompoundStatement']),
    ('CDeclaration', ['CDeclarationSpecifiers', 'CInitDeclaratorListOptional', KeyWord(';')]),
    ('CDeclarationList', ['CDeclaration']),
    ('CDeclarationList', ['CDeclarationList', 'CDeclaration']),
    ('CDeclarationListOptional', ['CDeclarationList']),
    ('CDeclarationListOptional', []),
    ('CDeclarationSpecifiers', ['CStorageClassSpecifier', 'CDeclarationSpecifiersOptional']),
    ('CDeclarationSpecifiers', ['CTypeSpecifier', 'CDeclarationSpecifiersOptional']),
    ('CDeclarationSpecifiers', ['CTypeQualifier', 'CDeclarationSpecifiersOptional']),
    ('CDeclarationSpecifiersOptional', ['CDeclarationSpecifiers']),
    ('CDeclarationSpecifiersOptional', []),
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
    ('CTypeQualifier', [KeyWord('const')]),
    ('CTypeQualifier', [KeyWord('volatile')]),
    ('CStructOrUnionSpecifier', ['CStructOrUnion', 'CIdentifierOptional', KeyWord('{'), 'CStructDeclarationList', KeyWord('}')]),
    ('CStructOrUnionSpecifier', ['CStructOrUnion', 'CIdentifier']),
    ('CStructOrUnion', [KeyWord('struct')]),
    ('CStructOrUnion', [KeyWord('union')]),
    ('CStructDeclarationList', ['CStructDeclaration']),
    ('CStructDeclarationList', ['CStructDeclarationList', 'CStructDeclaration']),
    ('CInitDeclaratorList', ['CInitDeclarator']),
    ('CInitDeclaratorList', ['CInitDeclaratorList', KeyWord(','), 'CInitDeclarator']),
    ('CInitDeclaratorListOptional', ['CInitDeclaratorList']),
    ('CInitDeclaratorListOptional', []),
    ('CInitDeclarator', ['CDeclarator']),
    ('CInitDeclarator', ['CDeclarator', KeyWord('='), 'CInitializer']),
    ('CStructDeclaration', ['CSpecifierQualifierList', 'CStructDeclaratorList', KeyWord(';')]),
    ('CSpecifierQualifierList', ['CTypeSpecifier', 'CSpecifierQualifierListOptional']),
    ('CSpecifierQualifierList', ['CTypeQualifier', 'CSpecifierQualifierListOptional']),
    ('CSpecifierQualifierListOptional', ['CSpecifierQualifierList']),
    ('CSpecifierQualifierListOptional', []),
    ('CStructDeclaratorList', ['CStructDeclarator']),
    ('CStructDeclaratorList', ['CStructDeclaratorList', KeyWord(','), 'CStructDeclarator']),
    ('CStructDeclarator', ['CDeclarator']),
    ('CStructDeclarator', ['CDeclaratorOptional', KeyWord(':'), 'CConstantExpression']),
    ('CEnumSpecifier', [KeyWord('enum'), 'CIdentifierOptional', KeyWord('{'), 'CEnumeratorList', KeyWord('}')]),
    ('CEnumSpecifier', [KeyWord('enum'), 'CIdentifier']),
    ('CEnumeratorList', ['CEnumerator']),
    ('CEnumeratorList', ['CEnumeratorList', KeyWord(','), 'CEnumerator']),
    ('CEnumerator', ['CIdentifier']),
    ('CEnumerator', ['CIdentifier', KeyWord('='), 'CConstantExpression']),
    ('CDeclarator', ['CPointerOptional', 'CDirectDeclarator']),
    ('CDeclaratorOptional', ['CDeclarator']),
    ('CDeclaratorOptional', []),
    ('CDirectDeclarator', ['CIdentifier']),
    ('CDirectDeclarator', [KeyWord('('), 'CDeclarator', KeyWord(')')]),
    ('CDirectDeclarator', ['CDirectDeclarator', KeyWord('['), 'CConstantExpressionOptional', KeyWord(']')]),
    ('CDirectDeclarator', ['CDirectDeclarator', KeyWord('['), 'CParameterTypeList', KeyWord(']')]),
    ('CDirectDeclarator', ['CDirectDeclarator', KeyWord('['), 'CIdentifierListOptional', KeyWord(']')]),
    ('CPointer', [KeyWord('*'), 'CTypeQualifierListOptional']),
    ('CPointer', [KeyWord('*'), 'CTypeQualifierListOptional', 'CPointer']),
    ('CPointerOptional', []),
    ('CPointerOptional', ['CPointer']),
    ('CTypeQualifierList', ['CTypeQualifier']),
    ('CTypeQualifierList', ['CTypeQualifierList', 'CTypeQualifier']),
    ('CTypeQualifierListOptional', ['CTypeQualifierList']),
    ('CTypeQualifierListOptional', []),
    ('CParameterTypeList', ['CParameterList']),
    ('CParameterTypeList', ['CParameterList', KeyWord(','), KeyWord('...')]),
    ('CParameterTypeListOptional', []),
    ('CParameterTypeListOptional', ['CParameterTypeList']),
    ('CParameterList', ['CParameterDeclaration']),
    ('CParameterList', ['CParameterList', KeyWord(','), 'CParameterDeclaration']),
    ('CParameterDeclaration', ['CDeclarationSpecifiers', 'CDeclarator']),
    ('CParameterDeclaration', ['CDeclarationSpecifiers', 'CAbstractDeclaratorOptional']),
    ('CIdentifierList', ['CIdentifier']),
    ('CIdentifierList', ['CIdentifierList', KeyWord(','), 'CIdentifier']),
    ('CIdentifierListOptional', ['CIdentifierList']),
    ('CIdentifierListOptional', []),
    ('CInitializer', ['CAssignmentExpression']),
    ('CInitializer', [KeyWord('{'), 'CInitializerList', KeyWord('}')]),
    ('CInitializer', [KeyWord('{'), 'CInitializerList', KeyWord(','), KeyWord('}')]),
    ('CInitializerList', ['CInitializer']),
    ('CInitializerList', ['CInitializerList', KeyWord(','), 'CInitializer']),
    ('CTypeName', ['CSpecifierQualifierList', 'CAbstractDeclaratorOptional']),
    ('CAbstractDeclarator', ['CPointer']),
    ('CAbstractDeclarator', ['CPointerOptional', 'CDirectAbstractDeclarator']),
    ('CAbstractDeclaratorOptional', ['CAbstractDeclarator']),
    ('CAbstractDeclaratorOptional', []),
    ('CDirectAbstractDeclarator', [KeyWord('('), 'CAbstractDeclarator', KeyWord(')')]),
    ('CDirectAbstractDeclarator', ['CDirectAbstractDeclaratorOptional', KeyWord('['), 'CConstantExpressionOptional', KeyWord(']')]),
    ('CDirectAbstractDeclarator', ['CDirectAbstractDeclaratorOptional', KeyWord('('), 'CParameterTypeListOptional', KeyWord(')')]),
    ('CDirectAbstractDeclaratorOptional', []),
    ('CDirectAbstractDeclaratorOptional', ['CDirectAbstractDeclarator']),
    ('CTypedefName', ['CIdentifier']),
    ('CStatement', ['CLabeledStatement']),
    ('CStatement', ['CCompoundStatement']),
    ('CStatement', ['CExpressionStatement']),
    ('CStatement', ['CSelectionStatement']),
    ('CStatement', ['CIterationStatement']),
    ('CStatement', ['CJumpStatement']),
    ('CLabeledStatement', ['CIdentifier', KeyWord(':'), 'CStatement']),
    ('CLabeledStatement', [KeyWord('case'), 'CConstantExpression', KeyWord(':'), 'CStatement']),
    ('CLabeledStatement', [KeyWord('default'), KeyWord(':'), 'CStatement']),
    ('CExpressionStatement', ['CExpressionOptional', KeyWord(';')]),
    ('CCompoundStatement', [KeyWord('{'), 'CDeclarationListOptional', 'CStatementListOptional', KeyWord('}')]),
    ('CStatementList', ['CStatement']),
    ('CStatementList', ['CStatementList', 'CStatement']),
    ('CStatementListOptional', ['CStatementList']),
    ('CStatementListOptional', []),
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
    ('CExpression', ['CAssignmentExpression']),
    ('CExpression', ['CExpression', KeyWord(','), 'CAssignmentExpression']),
    ('CExpressionOptional', ['CExpression']),
    ('CExpressionOptional', []),
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
    ('CConditionalExpression', ['CLogicalOrExpression']),
    ('CConditionalExpression', ['CLogicalOrExpression', KeyWord('?'), 'CExpression', KeyWord(':'), 'CConditionalExpression']),
    ('CConstantExpression', ['CConditionalExpression']),
    ('CConstantExpressionOptional', []),
    ('CConstantExpressionOptional', ['CConstantExpression']),
    ('CLogicalOrExpression', ['CLogicalAndExpression']),
    ('CLogicalOrExpression', ['CLogicalOrExpression', KeyWord('||'), 'CLogicalAndExpression']),
    ('CLogicalAndExpression', ['CInclusiveOrExpression']),
    ('CLogicalAndExpression', ['CLogicalAndExpression', KeyWord('&&'), 'CInclusiveOrExpression']),
    ('CInclusiveOrExpression', ['CExclusiveOrExpression']),
    ('CInclusiveOrExpression', ['CInclusiveOrExpression', KeyWord('|'), 'CExclusiveOrExpression']),
    ('CExclusiveOrExpression', ['CAndExpression']),
    ('CExclusiveOrExpression', ['CExclusiveOrExpression', KeyWord('^'), 'CAndExpression']),
    ('CAndExpression', ['CEqualityExpression']),
    ('CAndExpression', ['CAndExpression', KeyWord('&'), 'CEqualityExpression']),
    ('CEqualityExpression', ['CRelationalExpression']),
    ('CEqualityExpression', ['CEqualityExpression', KeyWord('=='), 'CRelationalExpression']),
    ('CEqualityExpression', ['CEqualityExpression', KeyWord('!='), 'CRelationalExpression']),
    ('CRelationalExpression', ['CShiftExpression']),
    ('CRelationalExpression', ['CRelationalExpression', KeyWord('<'), 'CShiftExpression']),
    ('CRelationalExpression', ['CRelationalExpression', KeyWord('>'), 'CShiftExpression']),
    ('CRelationalExpression', ['CRelationalExpression', KeyWord('<='), 'CShiftExpression']),
    ('CRelationalExpression', ['CRelationalExpression', KeyWord('>='), 'CShiftExpression']),
    ('CShiftExpression', ['CAdditiveExpression']),
    ('CShiftExpression', ['CShiftExpression', KeyWord('<<'), 'CAdditiveExpression']),
    ('CShiftExpression', ['CShiftExpression', KeyWord('>>'), 'CAdditiveExpression']),
    ('CAdditiveExpression', ['CMultiplicativeExpression']),
    ('CAdditiveExpression', ['CAdditiveExpression', KeyWord('+'), 'CMultiplicativeExpression']),
    ('CAdditiveExpression', ['CAdditiveExpression', KeyWord('-'), 'CMultiplicativeExpression']),
    ('CMultiplicativeExpression', ['CCastExpression']),
    ('CMultiplicativeExpression', ['CMultiplicativeExpression', KeyWord('*'), 'CCastExpression']),
    ('CMultiplicativeExpression', ['CMultiplicativeExpression', KeyWord('/'), 'CCastExpression']),
    ('CMultiplicativeExpression', ['CMultiplicativeExpression', KeyWord('%'), 'CCastExpression']),
    ('CCastExpression', ['CUnaryExpression']),
    ('CCastExpression', [KeyWord('('), 'CTypeName', KeyWord(')'), 'CCastExpression']),
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
    ('CPostfixExpression', ['CPrimaryExpression']),
    ('CPostfixExpression', ['CPostfixExpression', KeyWord('['), 'CExpression', KeyWord(']')]),
    ('CPostfixExpression', ['CPostfixExpression', KeyWord('('), 'CArgumentExpressionListOptional', KeyWord(')')]),
    ('CPostfixExpression', ['CPostfixExpression', KeyWord('.'), 'CIdentifier']),
    ('CPostfixExpression', ['CPostfixExpression', KeyWord('->'), 'CIdentifier']),
    ('CPostfixExpression', ['CPostfixExpression', KeyWord('++')]),
    ('CPostfixExpression', ['CPostfixExpression', KeyWord('--')]),
    ('CPrimaryExpression', ['CIdentifier']),
    ('CPrimaryExpression', ['CConstant']),
    ('CPrimaryExpression', [Literal('STRING')]),
    ('CPrimaryExpression', [KeyWord('('), 'CExpression', KeyWord(')')]),
    ('CArgumentExpressionList', ['CAssignmentExpression']),
    ('CArgumentExpressionList', ['CArgumentExpressionList', KeyWord(','), 'CAssignmentExpression']),
    ('CArgumentExpressionListOptional', ['CArgumentExpressionList']),
    ('CArgumentExpressionListOptional', []),
    ('CConstant', [Literal('INT')]),
    ('CConstant', ['CEnumerationConstant']),
    ('CConstant', [Literal('CHAR')]),
    ('CConstant', [Literal('FLOAT')]),
    ('CIdentifier', [Identifier()]),
    ('CIdentifierOptional', ['CIdentifier']),
    ('CIdentifierOptional', []),
    ('CEnumerationConstant', ['CIdentifier']),
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
                print(set.intersection(s1,s2))
                print(seq1)
                print(seq2)
