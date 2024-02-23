module Main where

import Text.Printf (printf)
import Control.Monad (unless)

-- Type for unary operator
data Operator1 = Sqrt | Neg

instance Show Operator1 where
  show Sqrt = "√"
  show Neg = "-"

instance Eq Operator1 where
  Sqrt == Sqrt = True
  Neg == Neg = True
  Neg == Sqrt = False
  Sqrt == Neg = False


-- Type for binary operator
data Operator2 = Div | Mul | Plus | Min deriving (Eq)

instance Show Operator2 where
  show Div = "/"
  show Mul = "*"
  show Plus = "+"
  show Min = "-"


-- Type for expression
data Expr = Arg Double | Marg Operator1 Expr | CE Expr Operator2 Expr

instance Show Expr where
  show (Arg value) = show value
  show (Marg op value)
    | op == Neg = "(" ++ (show op) ++ (show value) ++ ")"
    | op == Sqrt = (show op) ++ "(" ++ (show value) ++ ")"
  show (CE expr1 op expr2) = (show expr1) ++ (show op) ++ (show expr2)

instance Eq Expr where
  Arg value1 == Arg value2 = value1 == value2
  Marg op1 value1 == Marg op2 value2 =
   op1 == op2 && value1 == value2
  CE expr11 op1 expr12 == CE expr21 op2 expr22 =
   expr11 == expr21 && op1 == op2 && expr12 == expr22
  Arg value1 == Marg op value2 = False
  Marg op value2 == Arg value1 = False
  Arg value1 == CE expr1 op expr2 = False
  CE expr1 op expr2 == Arg value1 = False
  Marg op1 value2 == CE expr1 op2 expr2 = False
  CE expr1 op2 expr2 == Marg op1 value2 = False


data Error = OutOfPossibleValuesError Operator1 Double | ZeroDivisionError Double Double

instance Show Error where
  show (OutOfPossibleValuesError op value) = "OutOfPossibleValuesError: operator "
   ++ (show op) ++ " can not handle expression " ++ (show value)
  show (ZeroDivisionError chislitel znamenatel) = "ZeroDivisionError: numerator "
   ++ (show chislitel) ++ " can not be divided by denominator " ++ (show znamenatel)

instance Eq Error where
  OutOfPossibleValuesError op1 value1 == OutOfPossibleValuesError op2 value2 =
   op1 == op2 && value1 == value2
  ZeroDivisionError chislitel1 znamenatel1 == ZeroDivisionError chislitel2 znamenatel2 =
   chislitel1 == chislitel2 && znamenatel1 == znamenatel2
  ZeroDivisionError chislitel znamenatel == OutOfPossibleValuesError op expr = False
  OutOfPossibleValuesError op value == ZeroDivisionError chislitel znamenatel = False


defineOperationEvaluator :: Operator2 -> (Double -> Double -> Double)
defineOperationEvaluator op
  | op == Div = (/)
  | op == Mul = (*)
  | op == Plus = (+)
  | op == Min = (-)

eval :: Expr -> Either Error Double
eval (Arg value) = Right value

eval (Marg Neg expr) = case (eval expr) of
  Right result -> Right (-result)
  Left exception -> Left exception

eval (Marg Sqrt expr) = case (eval expr) of
  Right result -> if (result >= 0) then Right (result**0.5)
   else Left (OutOfPossibleValuesError Sqrt result)
  Left exception -> Left exception

eval (CE expr1 op expr2) = case (eval expr1) of
  Right result1 -> case (eval expr2) of
    Right result2 -> case (op) of
      Div -> if (result2 /= 0) then Right (defineOperationEvaluator op result1 result2)
       else Left (ZeroDivisionError result1 result2)
      _ -> Right (defineOperationEvaluator op result1 result2)
    Left exception -> Left exception
  Left exception -> Left exception


cases :: [(Expr, Either Error Double)]
cases = [
 -- Double as expression = Double
 (Arg 56, Right 56),
 -- Negative of expression = Negative double
 (Marg Neg (Arg 3), Right (-3)),
 (Marg Neg (Arg 0), Right 0),
 -- Negative of negative expression = Positive Double
 (Marg Neg (Marg Neg (Arg 3)), Right 3),
 -- Sqrt of expression = Double
 (Marg Sqrt (Arg 4), Right 2),
 (Marg Sqrt (Arg (169)), Right 13),
 (Marg Sqrt (Arg (0)), Right 0),
 -- Sqrt of negative value as expression = OutOfPossibleValuesError
 (Marg Sqrt (Arg (-169)), Left (OutOfPossibleValuesError Sqrt (-169))),
 -- Sqrt of negative expression = OutOfPossibleValuesError
 (Marg Sqrt (Marg Neg (Arg 169)), Left (OutOfPossibleValuesError Sqrt (-169))),
 -- Simple binary evaluations = Double
 (CE (Arg 1) Plus (Arg 2.2), Right 3.2),
 (CE (Arg 2) Min (Arg 1.25), Right 0.75),
 (CE (Arg 3.5) Mul (Arg 10), Right 35),
 (CE (Arg 4) Div (Arg 2), Right 2),
 (CE (Arg 5) Div (Arg 2), Right 2.5),
 (CE (Arg (-5)) Div (Arg 2), Right (-2.5)),
 (CE (Arg (-5)) Div (Marg Neg (Arg 2)), Right 2.5),
 (CE (Arg 0) Div (Arg 2), Right 0),
 -- Simple binary login with division by zero = ZeroDivisionError
 (CE (Arg 6) Div (Arg 0), Left (ZeroDivisionError 6 0)),
 (CE (Arg 0) Div (Arg 0), Left (ZeroDivisionError 0 0)),
 (CE (Arg 4) Plus (CE (Arg 5) Min (Arg 6)), Right 3),
 -- Complex expressions = Double
 (CE (CE (Arg 5) Min (Arg 6)) Plus (Arg 4), Right 3),
 (CE (Marg Neg (Arg 7)) Min (Marg Neg (Arg 3)), Right (-4)),
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg 9))) Plus (CE (Arg 0) Plus (Marg Neg (Arg 5))), Right 1),
 -- Complex expression with error in sub-expression = Exception of first sub expression Error
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (-9)))) Plus (CE (Arg 0) Plus (Marg Neg (Arg 5))), Left (OutOfPossibleValuesError Sqrt (-9))),
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (9)))) Plus (CE (Arg 2) Div (Marg Neg (Arg 0))), Left (ZeroDivisionError 2 0)),
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (-9)))) Plus (CE (Arg 2) Div (Marg Neg (Arg 0))), Left (OutOfPossibleValuesError Sqrt (-9)))
 ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)


main :: IO ()
main = do
  mapM_ (uncurry test) cases
