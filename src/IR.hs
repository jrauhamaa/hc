module IR where

import Data.Bits (xor, (.&.), (.|.), shiftL, shiftR, complement)
import Data.Char (ord)

import ParseItem
import Utils (Location, Error(..))

type EvaluationResult = Either Error Double

doubleToInteger :: Location -> Double -> Either Error Int
doubleToInteger c x =
  if fromIntegral (floor x :: Int) == x
    then return $ floor x
    else Left . TypeError c $ "expected int value instead of float"

evaluateConstantExpression :: CConstantExpression -> EvaluationResult
evaluateConstantExpression (CConstantExpression condExpr) =
  evaluateConditionalExpression $ parseItem condExpr

evaluateConditionalExpression :: CConditionalExpression -> EvaluationResult
evaluateConditionalExpression (CConditionalExpression orExpr ternaryOpt) = do
  x <- evaluateLogicalOrExpression $ parseItem orExpr
  evaluateTernaryOptional x $ parseItem ternaryOpt

evaluateTernaryOptional :: Double -> CTernaryOptional -> EvaluationResult
evaluateTernaryOptional x CTernaryOptionalEmpty = return x
evaluateTernaryOptional x (CTernaryOptional expr condExpr) =
  if x == 0
    then evaluateConditionalExpression $ parseItem condExpr
    else evaluateExpression $ parseItem expr

evaluateLogicalOrExpression :: CLogicalOrExpression -> EvaluationResult
evaluateLogicalOrExpression (CLogicalOrExpression andExpr orExpr') = do
  x <- evaluateLogicalAndExpression $ parseItem andExpr
  if x == 0
    then evaluateLogicalOrExpression' $ parseItem orExpr'
    else return x

evaluateLogicalOrExpression' :: CLogicalOrExpression' -> EvaluationResult
evaluateLogicalOrExpression' CLogicalOrExpression'Empty = return 0
evaluateLogicalOrExpression' (CLogicalOrExpression' andExpr orExpr') = do
  x <- evaluateLogicalAndExpression $ parseItem andExpr
  if x == 0
    then evaluateLogicalOrExpression' $ parseItem orExpr'
    else return x

evaluateLogicalAndExpression :: CLogicalAndExpression -> EvaluationResult
evaluateLogicalAndExpression (CLogicalAndExpression orExpr andExpr') = do
  x <- evaluateInclusiveOrExpression $ parseItem orExpr
  if x == 0
    then return 0
    else evaluateLogicalAndExpression' x $ parseItem andExpr'

evaluateLogicalAndExpression' :: Double -> CLogicalAndExpression' -> EvaluationResult
evaluateLogicalAndExpression' x CLogicalAndExpression'Empty = return x
evaluateLogicalAndExpression' _ (CLogicalAndExpression' orExpr andExpr') = do
  x <- evaluateInclusiveOrExpression $ parseItem orExpr
  if x == 0
    then return 0
    else evaluateLogicalAndExpression' x $ parseItem andExpr'

evaluateInclusiveOrExpression :: CInclusiveOrExpression -> EvaluationResult
evaluateInclusiveOrExpression (CInclusiveOrExpression exclOr inclOr') = do
  x <- evaluateExclusiveOrExpression $ parseItem exclOr
  evaluateInclusiveOrExpression' x $ parseItem inclOr'

evaluateInclusiveOrExpression' :: Double -> CInclusiveOrExpression' -> EvaluationResult
evaluateInclusiveOrExpression' x CInclusiveOrExpression'Empty = return x
evaluateInclusiveOrExpression' x (CInclusiveOrExpression' exclOr inclOr') = do
  x' <- evaluateExclusiveOrExpression $ parseItem exclOr
  ix <- doubleToInteger (parseLoc exclOr) x
  ix' <- doubleToInteger (parseLoc exclOr) x'
  evaluateInclusiveOrExpression' (fromIntegral (ix .|. ix')) (parseItem inclOr')

evaluateExclusiveOrExpression :: CExclusiveOrExpression -> EvaluationResult
evaluateExclusiveOrExpression (CExclusiveOrExpression andExpr orExpr') = do
  x <- evaluateAndExpression $ parseItem andExpr
  evaluateExclusiveOrExpression' x $ parseItem orExpr'

evaluateExclusiveOrExpression' :: Double -> CExclusiveOrExpression' -> EvaluationResult
evaluateExclusiveOrExpression' x CExclusiveOrExpression'Empty = return x
evaluateExclusiveOrExpression' x (CExclusiveOrExpression' andExpr orExpr') = do
  x' <- evaluateAndExpression $ parseItem andExpr
  ix <- doubleToInteger (parseLoc andExpr) x
  ix' <- doubleToInteger (parseLoc andExpr) x'
  evaluateExclusiveOrExpression' (fromIntegral (ix `xor` ix')) (parseItem orExpr')

evaluateAndExpression :: CAndExpression -> EvaluationResult
evaluateAndExpression (CAndExpression eqExpr andExpr') = do
  x <- evaluateEqualityExpression $ parseItem eqExpr
  evaluateAndExpression' x $ parseItem andExpr'

evaluateAndExpression' :: Double -> CAndExpression' -> EvaluationResult
evaluateAndExpression' x CAndExpression'Empty = return x
evaluateAndExpression' x (CAndExpression' eqExpr andExpr') = do
  x' <- evaluateEqualityExpression $ parseItem eqExpr
  ix <- doubleToInteger (parseLoc eqExpr) x
  ix' <- doubleToInteger (parseLoc eqExpr) x'
  evaluateAndExpression' (fromIntegral (ix .&. ix')) (parseItem andExpr')

evaluateEqualityExpression :: CEqualityExpression -> EvaluationResult
evaluateEqualityExpression (CEqualityExpression relExpr eqExpr') = do
  x <- evaluateRelationalExpression $ parseItem relExpr
  evaluateEqualityExpression' x $ parseItem eqExpr'

evaluateEqualityExpression' :: Double -> CEqualityExpression' -> EvaluationResult
evaluateEqualityExpression' x CEqualityExpression'Empty = return x
evaluateEqualityExpression' x (CEqualityExpression'EQ relExpr eqExpr') = do
  x' <- evaluateRelationalExpression $ parseItem relExpr
  if x == x'
    then evaluateEqualityExpression' x' $ parseItem eqExpr'
    else return 0
evaluateEqualityExpression' x (CEqualityExpression'NEQ relExpr eqExpr') = do
  x' <- evaluateRelationalExpression $ parseItem relExpr
  if x /= x'
    then evaluateEqualityExpression' x' $ parseItem eqExpr'
    else return 0

evaluateRelationalExpression :: CRelationalExpression -> EvaluationResult
evaluateRelationalExpression (CRelationalExpression shiftExpr relExpr') = do
  x <- evaluateShiftExpression $ parseItem shiftExpr
  evaluateRelationalExpression' x $ parseItem relExpr'

evaluateRelationalExpression' :: Double -> CRelationalExpression' -> EvaluationResult
evaluateRelationalExpression' x CRelationalExpression'Empty = return x
evaluateRelationalExpression' x (CRelationalExpression'LT shiftExpr relExpr') = do
  x' <- evaluateShiftExpression $ parseItem shiftExpr
  if x < x'
    then evaluateRelationalExpression' x' $ parseItem relExpr'
    else return 0
evaluateRelationalExpression' x (CRelationalExpression'LTE shiftExpr relExpr') = do
  x' <- evaluateShiftExpression $ parseItem shiftExpr
  if x <= x'
    then evaluateRelationalExpression' x' $ parseItem relExpr'
    else return 0
evaluateRelationalExpression' x (CRelationalExpression'GT shiftExpr relExpr') = do
  x' <- evaluateShiftExpression $ parseItem shiftExpr
  if x > x'
    then evaluateRelationalExpression' x' $ parseItem relExpr'
    else return 0
evaluateRelationalExpression' x (CRelationalExpression'GTE shiftExpr relExpr') = do
  x' <- evaluateShiftExpression $ parseItem shiftExpr
  if x >= x'
    then evaluateRelationalExpression' x' $ parseItem relExpr'
    else return 0

evaluateShiftExpression :: CShiftExpression -> EvaluationResult
evaluateShiftExpression (CShiftExpression addExpr shiftExpr') = do
  x <- evaluateAdditiveExpression $ parseItem addExpr
  evaluateShiftExpression' x $ parseItem shiftExpr'

evaluateShiftExpression' :: Double -> CShiftExpression' -> EvaluationResult
evaluateShiftExpression' x CShiftExpression'Empty = return x
evaluateShiftExpression' x (CShiftExpression'Left addExpr shiftExpr') = do
  x' <- evaluateAdditiveExpression $ parseItem addExpr
  ix <- doubleToInteger (parseLoc addExpr) x
  ix' <- doubleToInteger (parseLoc addExpr) x'
  evaluateShiftExpression'
    (fromIntegral (ix `shiftL` ix'))
    (parseItem shiftExpr')
evaluateShiftExpression' x (CShiftExpression'Right addExpr shiftExpr') = do
  x' <- evaluateAdditiveExpression $ parseItem addExpr
  ix <- doubleToInteger (parseLoc addExpr) x
  ix' <- doubleToInteger (parseLoc addExpr) x'
  evaluateShiftExpression'
    (fromIntegral (ix `shiftR` ix'))
    (parseItem shiftExpr')

evaluateAdditiveExpression :: CAdditiveExpression -> EvaluationResult
evaluateAdditiveExpression (CAdditiveExpression multExpr addExpr') = do
  x <- evaluateMultiplicativeExpression $ parseItem multExpr
  evaluateAdditiveExpression' x $ parseItem addExpr'

evaluateAdditiveExpression' :: Double -> CAdditiveExpression' -> EvaluationResult
evaluateAdditiveExpression' x CAdditiveExpression'Empty = return x
evaluateAdditiveExpression' x (CAdditiveExpression'Add multExpr addExpr') = do
  x' <- evaluateMultiplicativeExpression $ parseItem multExpr
  evaluateAdditiveExpression' (x + x') (parseItem addExpr')
evaluateAdditiveExpression' x (CAdditiveExpression'Sub multExpr addExpr') = do
  x' <- evaluateMultiplicativeExpression $ parseItem multExpr
  evaluateAdditiveExpression' (x - x') (parseItem addExpr')

evaluateMultiplicativeExpression :: CMultiplicativeExpression -> EvaluationResult
evaluateMultiplicativeExpression (CMultiplicativeExpression castExpr multExpr') = do
  x <- evaluateCastExpression $ parseItem castExpr
  evaluateMultiplicativeExpression' x $ parseItem multExpr'

evaluateMultiplicativeExpression' :: Double -> CMultiplicativeExpression' -> EvaluationResult
evaluateMultiplicativeExpression' x CMultiplicativeExpression'Empty = return x
evaluateMultiplicativeExpression' x (CMultiplicativeExpression'Mul castExpr multExpr') = do
  x' <- evaluateCastExpression $ parseItem castExpr
  evaluateMultiplicativeExpression' (x * x') (parseItem multExpr')
evaluateMultiplicativeExpression' x (CMultiplicativeExpression'Div castExpr multExpr') = do
  x' <- evaluateCastExpression $ parseItem castExpr
  ix <- doubleToInteger (parseLoc castExpr) x
  ix' <- doubleToInteger (parseLoc castExpr) x'
  evaluateMultiplicativeExpression' (fromIntegral (ix `div` ix')) (parseItem multExpr')
evaluateMultiplicativeExpression' x (CMultiplicativeExpression'Mod castExpr multExpr') = do
  x' <- evaluateCastExpression $ parseItem castExpr
  ix <- doubleToInteger (parseLoc castExpr) x
  ix' <- doubleToInteger (parseLoc castExpr) x'
  evaluateMultiplicativeExpression' (fromIntegral (ix `mod` ix')) (parseItem multExpr')

evaluateCastExpression :: CCastExpression -> EvaluationResult
evaluateCastExpression (CCastExpressionUnary unaryExpr) =
  evaluateUnaryExpression $ parseItem unaryExpr
evaluateCastExpression (CCastExpression i _) =
  Left $ SyntaxError (parseLoc i) "type cast in a constant expression"

evaluateUnaryExpression :: CUnaryExpression -> EvaluationResult
evaluateUnaryExpression (CUnaryExpressionPostfix postfixExpr) =
  evaluatePostfixExpression $ parseItem postfixExpr
evaluateUnaryExpression (CUnaryExpressionInc i) =
  Left $ SyntaxError (parseLoc i) "increment operator in a constant expression"
evaluateUnaryExpression (CUnaryExpressionDec i) =
  Left $ SyntaxError (parseLoc i) "decrement operator in a constant expression"
evaluateUnaryExpression (CUnaryExpressionUnaryOp op castExpr) =
  case parseItem op of
    CUnaryOperatorAdd ->
      evaluateCastExpression $ parseItem castExpr
    CUnaryOperatorSub ->
      negate <$> evaluateCastExpression (parseItem castExpr)
    CUnaryOperatorBitwiseNot -> do
      x <- evaluateCastExpression (parseItem castExpr)
      ix <- doubleToInteger (parseLoc op) x
      return . fromIntegral . complement $ ix
    CUnaryOperatorNot -> do
      x <- evaluateCastExpression $ parseItem castExpr
      if x == 0
        then return 1
        else return 0
    _ ->
      Left $ SyntaxError
               (parseLoc op)
               "Illegal operator in a constant expression"
evaluateUnaryExpression (CUnaryExpressionSizeof expr) =
  Left $ SyntaxError (parseLoc expr) "sizeof operator in a constant expression"
evaluateUnaryExpression (CUnaryExpressionSizeofType typeName) =
  Left $ SyntaxError (parseLoc typeName) "sizeof operator in a constant expression"

evaluatePostfixExpression :: CPostfixExpression -> EvaluationResult
evaluatePostfixExpression (CPostfixExpression primaryExpr postfixExpr') = do
  x <- evaluatePrimaryExpression $ parseItem primaryExpr
  evaluatePostfixExpression' (parseLoc postfixExpr') x $ parseItem postfixExpr'

evaluatePostfixExpression' :: Location -> Double -> CPostfixExpression' -> EvaluationResult
evaluatePostfixExpression' _ x CPostfixExpression'Empty = return x
evaluatePostfixExpression' c _ _ =
  Left $ SyntaxError c "Illegal operator in a constant expression"

evaluatePrimaryExpression :: CPrimaryExpression -> EvaluationResult
evaluatePrimaryExpression (CPrimaryExpressionId i) =
  Left $ SyntaxError (parseLoc i) "variable name in a constant expression"
evaluatePrimaryExpression (CPrimaryExpressionString s) =
  Left $ SyntaxError (parseLoc s) "string literal in a constant expression"
evaluatePrimaryExpression (CPrimaryExpressionParen expr) =
  evaluateExpression $ parseItem expr
evaluatePrimaryExpression (CPrimaryExpressionConst c) =
  evaluateConstant $ parseItem c

evaluateConstant :: CConstant -> EvaluationResult
evaluateConstant (CConstantInt i) = return . fromIntegral . parseItem $ i
evaluateConstant (CConstantChar c) = return . fromIntegral . ord . parseItem $ c
evaluateConstant (CConstantFloat f) = return $ parseItem f
evaluateConstant (CConstantEnum e) =
  Left $ SyntaxError (parseLoc e) "enum value in an integer expression"

evaluateExpression :: CExpression -> EvaluationResult
evaluateExpression (CExpression assignmentExpr expr') = do
  x <- evaluateAssignmentExpression $ parseItem assignmentExpr
  evaluateExpression' x $ parseItem expr'

evaluateExpression' :: Double -> CExpression' -> EvaluationResult
evaluateExpression' x CExpression'Empty = return x
evaluateExpression' _ (CExpression' i _) =
  Left $ SyntaxError (parseLoc i) "comma in a constant expression"

evaluateAssignmentExpression :: CAssignmentExpression -> EvaluationResult
evaluateAssignmentExpression (CAssignmentExpressionConditional condExpr) =
  evaluateConditionalExpression $ parseItem condExpr
evaluateAssignmentExpression (CAssignmentExpression i _ _) =
  Left $ SyntaxError (parseLoc i) "assignment in a constant expression"


