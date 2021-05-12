module IR where

import Data.Bits (xor, (.&.), (.|.), shiftL, shiftR, complement)
import Data.Char (ord)

import ParseItem
import Utils

evaluateConstantIntExpression :: CConstantExpression -> Either Error Int
evaluateConstantIntExpression (CConstantExpression condExpr) =
  constIntConditionalExpression $ parseItem condExpr

constIntConditionalExpression :: CConditionalExpression -> Either Error Int
constIntConditionalExpression (CConditionalExpression orExpr ternaryOpt) = do
  x <- constIntLogicalOrExpression $ parseItem orExpr
  constIntTernaryOptional x $ parseItem ternaryOpt

constIntTernaryOptional :: Int -> CTernaryOptional -> Either Error Int
constIntTernaryOptional x CTernaryOptionalEmpty = return x
constIntTernaryOptional x (CTernaryOptional expr condExpr) =
  if x == 0
    then constIntConditionalExpression $ parseItem condExpr
    else constIntExpression $ parseItem expr

constIntLogicalOrExpression :: CLogicalOrExpression -> Either Error Int
constIntLogicalOrExpression (CLogicalOrExpression andExpr orExpr') = do
  x <- constIntLogicalAndExpression $ parseItem andExpr
  if x == 0
    then constIntLogicalOrExpression' $ parseItem orExpr'
    else return x

constIntLogicalOrExpression' :: CLogicalOrExpression' -> Either Error Int
constIntLogicalOrExpression' CLogicalOrExpression'Empty = return 0
constIntLogicalOrExpression' (CLogicalOrExpression' andExpr orExpr') = do
  x <- constIntLogicalAndExpression $ parseItem andExpr
  if x == 0
    then constIntLogicalOrExpression' $ parseItem orExpr'
    else return x

constIntLogicalAndExpression :: CLogicalAndExpression -> Either Error Int
constIntLogicalAndExpression (CLogicalAndExpression orExpr andExpr') = do
  x <- constIntInclusiveOrExpression $ parseItem orExpr
  if x == 0
    then return 0
    else constIntLogicalAndExpression' $ parseItem andExpr'

constIntLogicalAndExpression' :: CLogicalAndExpression' -> Either Error Int
constIntLogicalAndExpression' CLogicalAndExpression'Empty = return 0
constIntLogicalAndExpression' (CLogicalAndExpression' orExpr andExpr') = do
  x <- constIntInclusiveOrExpression $ parseItem orExpr
  if x == 0
    then return 0
    else constIntLogicalAndExpression' $ parseItem andExpr'

constIntInclusiveOrExpression :: CInclusiveOrExpression -> Either Error Int
constIntInclusiveOrExpression (CInclusiveOrExpression exclOr inclOr') = do
  x <- constIntExclusiveOrExpression $ parseItem exclOr
  constIntInclusiveOrExpression' x $ parseItem inclOr'

constIntInclusiveOrExpression' :: Int -> CInclusiveOrExpression' -> Either Error Int
constIntInclusiveOrExpression' x CInclusiveOrExpression'Empty = return x
constIntInclusiveOrExpression' x (CInclusiveOrExpression' exclOr inclOr') = do
  x' <- constIntExclusiveOrExpression $ parseItem exclOr
  constIntInclusiveOrExpression' (x .|. x') (parseItem inclOr')

constIntExclusiveOrExpression :: CExclusiveOrExpression -> Either Error Int
constIntExclusiveOrExpression (CExclusiveOrExpression andExpr orExpr') = do
  x <- constIntAndExpression $ parseItem andExpr
  constIntExclusiveOrExpression' x $ parseItem orExpr'

constIntExclusiveOrExpression' :: Int -> CExclusiveOrExpression' -> Either Error Int
constIntExclusiveOrExpression' x CExclusiveOrExpression'Empty = return x
constIntExclusiveOrExpression' x (CExclusiveOrExpression' andExpr orExpr') = do
  x' <- constIntAndExpression $ parseItem andExpr
  constIntExclusiveOrExpression' (x `xor` x') (parseItem orExpr')

constIntAndExpression :: CAndExpression -> Either Error Int
constIntAndExpression (CAndExpression eqExpr andExpr') = do
  x <- constIntEqualityExpression $ parseItem eqExpr
  constIntAndExpression' x $ parseItem andExpr'

constIntAndExpression' :: Int -> CAndExpression' -> Either Error Int
constIntAndExpression' x CAndExpression'Empty = return x
constIntAndExpression' x (CAndExpression' eqExpr andExpr') = do
  x' <- constIntEqualityExpression $ parseItem eqExpr
  constIntAndExpression' (x .&. x') (parseItem andExpr')

constIntEqualityExpression :: CEqualityExpression -> Either Error Int
constIntEqualityExpression (CEqualityExpression relExpr eqExpr') = do
  x <- constIntRelationalExpression $ parseItem relExpr
  constIntEqualityExpression' x $ parseItem eqExpr'

constIntEqualityExpression' :: Int -> CEqualityExpression' -> Either Error Int
constIntEqualityExpression' x CEqualityExpression'Empty = return x
constIntEqualityExpression' x (CEqualityExpression'EQ relExpr eqExpr') = do
  x' <- constIntRelationalExpression $ parseItem relExpr
  if x == x'
    then constIntEqualityExpression' x' $ parseItem eqExpr'
    else return 0
constIntEqualityExpression' x (CEqualityExpression'NEQ relExpr eqExpr') = do
  x' <- constIntRelationalExpression $ parseItem relExpr
  if x /= x'
    then constIntEqualityExpression' x' $ parseItem eqExpr'
    else return 0

constIntRelationalExpression :: CRelationalExpression -> Either Error Int
constIntRelationalExpression (CRelationalExpression shiftExpr relExpr') = do
  x <- constIntShiftExpression $ parseItem shiftExpr
  constIntRelationalExpression' x $ parseItem relExpr'

constIntRelationalExpression' :: Int -> CRelationalExpression' -> Either Error Int
constIntRelationalExpression' x CRelationalExpression'Empty = return x
constIntRelationalExpression' x (CRelationalExpression'LT shiftExpr relExpr') = do
  x' <- constIntShiftExpression $ parseItem shiftExpr
  if x < x'
    then constIntRelationalExpression' x' $ parseItem relExpr'
    else return 0
constIntRelationalExpression' x (CRelationalExpression'LTE shiftExpr relExpr') = do
  x' <- constIntShiftExpression $ parseItem shiftExpr
  if x <= x'
    then constIntRelationalExpression' x' $ parseItem relExpr'
    else return 0
constIntRelationalExpression' x (CRelationalExpression'GT shiftExpr relExpr') = do
  x' <- constIntShiftExpression $ parseItem shiftExpr
  if x > x'
    then constIntRelationalExpression' x' $ parseItem relExpr'
    else return 0
constIntRelationalExpression' x (CRelationalExpression'GTE shiftExpr relExpr') = do
  x' <- constIntShiftExpression $ parseItem shiftExpr
  if x >= x'
    then constIntRelationalExpression' x' $ parseItem relExpr'
    else return 0

constIntShiftExpression :: CShiftExpression -> Either Error Int
constIntShiftExpression (CShiftExpression addExpr shiftExpr') = do
  x <- constIntAdditiveExpression $ parseItem addExpr
  constIntShiftExpression' x $ parseItem shiftExpr'

constIntShiftExpression' :: Int -> CShiftExpression' -> Either Error Int
constIntShiftExpression' x CShiftExpression'Empty = return x
constIntShiftExpression' x (CShiftExpression'Left addExpr shiftExpr') = do
  x' <- constIntAdditiveExpression $ parseItem addExpr
  constIntShiftExpression' (x `shiftL` x') $ parseItem shiftExpr'
constIntShiftExpression' x (CShiftExpression'Right addExpr shiftExpr') = do
  x' <- constIntAdditiveExpression $ parseItem addExpr
  constIntShiftExpression' (x `shiftR` x') (parseItem shiftExpr')

constIntAdditiveExpression :: CAdditiveExpression -> Either Error Int
constIntAdditiveExpression (CAdditiveExpression multExpr addExpr') = do
  x <- constIntMultiplicativeExpression $ parseItem multExpr
  constIntAdditiveExpression' x $ parseItem addExpr'

constIntAdditiveExpression' :: Int -> CAdditiveExpression' -> Either Error Int
constIntAdditiveExpression' x CAdditiveExpression'Empty = return x
constIntAdditiveExpression' x (CAdditiveExpression'Add multExpr addExpr') = do
  x' <- constIntMultiplicativeExpression $ parseItem multExpr
  constIntAdditiveExpression' (x + x') (parseItem addExpr')
constIntAdditiveExpression' x (CAdditiveExpression'Sub multExpr addExpr') = do
  x' <- constIntMultiplicativeExpression $ parseItem multExpr
  constIntAdditiveExpression' (x - x') (parseItem addExpr')

constIntMultiplicativeExpression :: CMultiplicativeExpression -> Either Error Int
constIntMultiplicativeExpression (CMultiplicativeExpression castExpr multExpr') = do
  x <- constIntCastExpression $ parseItem castExpr
  constIntMultiplicativeExpression' x $ parseItem multExpr'

constIntMultiplicativeExpression' :: Int -> CMultiplicativeExpression' -> Either Error Int
constIntMultiplicativeExpression' x CMultiplicativeExpression'Empty = return x
constIntMultiplicativeExpression' x (CMultiplicativeExpression'Mul castExpr multExpr') = do
  x' <- constIntCastExpression $ parseItem castExpr
  constIntMultiplicativeExpression' (x * x') (parseItem multExpr')
constIntMultiplicativeExpression' x (CMultiplicativeExpression'Div castExpr multExpr') = do
  x' <- constIntCastExpression $ parseItem castExpr
  constIntMultiplicativeExpression' (x `div` x') (parseItem multExpr')
constIntMultiplicativeExpression' x (CMultiplicativeExpression'Mod castExpr multExpr') = do
  x' <- constIntCastExpression $ parseItem castExpr
  constIntMultiplicativeExpression' (x `mod` x') (parseItem multExpr')

constIntCastExpression :: CCastExpression -> Either Error Int
constIntCastExpression (CCastExpressionUnary unaryExpr) =
  constIntUnaryExpression $ parseItem unaryExpr
constIntCastExpression (CCastExpression i _) =
  Left $ SyntaxError (parseLoc i) "type cast in a constant expression"

constIntUnaryExpression :: CUnaryExpression -> Either Error Int
constIntUnaryExpression (CUnaryExpressionPostfix postfixExpr) =
  constIntPostfixExpression $ parseItem postfixExpr
constIntUnaryExpression (CUnaryExpressionInc i) =
  Left $ SyntaxError (parseLoc i) "increment operator in a constant expression"
constIntUnaryExpression (CUnaryExpressionDec i) =
  Left $ SyntaxError (parseLoc i) "decrement operator in a constant expression"
constIntUnaryExpression (CUnaryExpressionUnaryOp op castExpr) =
  case parseItem op of
    CUnaryOperatorAdd ->
      constIntCastExpression $ parseItem castExpr
    CUnaryOperatorSub ->
      negate <$> constIntCastExpression (parseItem castExpr)
    CUnaryOperatorBitwiseNot ->
      complement <$> constIntCastExpression (parseItem castExpr)
    CUnaryOperatorNot -> do
      x <- constIntCastExpression $ parseItem castExpr
      if x == 0
        then return 1
        else return 0
    _ ->
      Left $ SyntaxError
               (parseLoc op)
               "Illegal operator in a constant expression"
constIntUnaryExpression (CUnaryExpressionSizeof expr) =
  Left $ SyntaxError (parseLoc expr) "sizeof operator in a constant expression"
constIntUnaryExpression (CUnaryExpressionSizeofType typeName) =
  Left $ SyntaxError (parseLoc typeName) "sizeof operator in a constant expression"

constIntPostfixExpression :: CPostfixExpression -> Either Error Int
constIntPostfixExpression (CPostfixExpression primaryExpr postfixExpr') = do
  x <- constIntPrimaryExpression $ parseItem primaryExpr
  constIntPostfixExpression' (parseLoc postfixExpr') x $ parseItem postfixExpr'

constIntPostfixExpression' :: Coordinates -> Int -> CPostfixExpression' -> Either Error Int
constIntPostfixExpression' _ x CPostfixExpression'Empty = return x
constIntPostfixExpression' c _ _ =
  Left $ SyntaxError c "Illegal operator in a constant expression"

constIntPrimaryExpression :: CPrimaryExpression -> Either Error Int
constIntPrimaryExpression (CPrimaryExpressionId i) =
  Left $ SyntaxError (parseLoc i) "variable name in a constant expression"
constIntPrimaryExpression (CPrimaryExpressionString s) =
  Left $ SyntaxError (parseLoc s) "string literal in a constant expression"
constIntPrimaryExpression (CPrimaryExpressionParen expr) =
  constIntExpression $ parseItem expr
constIntPrimaryExpression (CPrimaryExpressionConst c) =
  constIntConstant $ parseItem c

constIntConstant :: CConstant -> Either Error Int
constIntConstant (CConstantInt i) = return $ parseItem i
constIntConstant (CConstantChar c) = return $ ord $ parseItem c
constIntConstant (CConstantFloat f) =
  Left $ SyntaxError (parseLoc f) "float literal in an integer expression"
constIntConstant (CConstantEnum e) =
  Left $ SyntaxError (parseLoc e) "enum value in an integer expression"

constIntExpression :: CExpression -> Either Error Int
constIntExpression (CExpression assignmentExpr expr') = do
  x <- constIntAssignmentExpression $ parseItem assignmentExpr
  constIntExpression' x $ parseItem expr'

constIntExpression' :: Int -> CExpression' -> Either Error Int
constIntExpression' x CExpression'Empty = return x
constIntExpression' _ (CExpression' i _) =
  Left $ SyntaxError (parseLoc i) "comma in a constant expression"

constIntAssignmentExpression :: CAssignmentExpression -> Either Error Int
constIntAssignmentExpression (CAssignmentExpressionConditional condExpr) =
  constIntConditionalExpression $ parseItem condExpr
constIntAssignmentExpression (CAssignmentExpression i _ _) =
  Left $ SyntaxError (parseLoc i) "assignment in a constant expression"


