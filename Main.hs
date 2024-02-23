module Main where

import Text.Printf (printf)
import Control.Monad (unless)


data Operator1 = Sqrt | Neg

instance Show Operator1 where
  show Sqrt = "√"
  show Neg = "-"

instance Eq Operator1 where
  Sqrt == Sqrt = True
  Neg == Neg = True
  Neg == Sqrt = False
  Sqrt == Neg = False


data Operator2 = Div | Mul | Plus | Min deriving (Eq)

instance Show Operator2 where
  show Div = "/"
  show Mul = "*"
  show Plus = "+"
  show Min = "-"


data Expr = Arg Double | Marg Operator1 Expr | CE Expr Operator2 Expr

instance Show Expr where
  show (Arg value) = show value
  show (Marg op value)
    | op == Neg = "(" ++ (show op) ++ (show value) ++ ")"
    | op == Sqrt = (show op) ++ "(" ++ (show value) ++ ")"
  show (CE expr1 op expr2) = (show expr1) ++ (show op) ++ (show expr2)

instance Eq Expr where
  Arg value1 == Arg value2 = value1 == value2
  Marg op1 value1 == Marg op2 value2 = op1 == op2 && value1 == value2
  CE expr11 op1 expr12 == CE expr21 op2 expr22 = expr11 == expr21 && op1 == op2 && expr12 == expr22
  Arg value1 == Marg op value2 = False
  Marg op value2 == Arg value1 = False
  Arg value1 == CE expr1 op expr2 = False
  CE expr1 op expr2 == Arg value1 = False
  Marg op1 value2 == CE expr1 op2 expr2 = False
  CE expr1 op2 expr2 == Marg op1 value2 = False


data Error = OutOfPossibleValuesError Operator1 Double | ZeroDivisionError Double Double

instance Show Error where
  show (OutOfPossibleValuesError op value) = "OutOfPossibleValuesError: operator " ++ (show op) ++ " can not handle expression " ++ (show value)
  show (ZeroDivisionError chislitel znamenatel) = "ZeroDivisionError: numerator " ++ (show chislitel) ++ " can not be divided by denominator " ++ (show znamenatel)

instance Eq Error where
  OutOfPossibleValuesError op1 value1 == OutOfPossibleValuesError op2 value2 = op1 == op2 && value1 == value2
  ZeroDivisionError chislitel1 znamenatel1 == ZeroDivisionError chislitel2 znamenatel2 = chislitel1 == chislitel2 && znamenatel1 == znamenatel2
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
  Right result -> if (result >= 0) then Right (result**0.5) else Left (OutOfPossibleValuesError Sqrt result)
  Left exception -> Left exception

eval (CE expr1 op expr2) = case (eval expr1) of
  Right result1 -> case (eval expr2) of
    Right result2 -> case (op) of
      Div -> if (result2 /= 0) then Right (defineOperationEvaluator op result1 result2) else Left (ZeroDivisionError result1 result2)
      _ -> Right (defineOperationEvaluator op result1 result2)
    Left exception -> Left exception
  Left exception -> Left exception

--
--cases :: [(Expr, Either Error Double)]
--cases = undefined
--
--test :: Expr -> Either Error Double -> IO ()
--test expr expected =
--    let actual = eval expr in
--    unless (expected == actual) $ describeFailure actual
--  where
--    describeFailure actual =
--      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)
--
--
main :: IO ()
main = putStr "Start"
--  mapM_ (uncurry test) cases
