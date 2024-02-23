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


data Error = OutOfPossibleValuesError Operator1 Expr | ZeroDivisionError Expr Expr

instance Show Error where
  show (OutOfPossibleValuesError op expr) = "OutOfPossibleValuesError: operator " ++ (show op) ++ " can not handle expression " ++ (show expr)
  show (ZeroDivisionError chislitel znamenatel) = "ZeroDivisionError: numerator " ++ (show chislitel) ++ " can not be divided by denominator " ++ (show znamenatel)

instance Eq Error where
  OutOfPossibleValuesError op1 expr1 == OutOfPossibleValuesError op2 expr2 = op1 == op2 && expr1 == expr2
  ZeroDivisionError chislitel1 znamenatel1 == ZeroDivisionError chislitel2 znamenatel2 = chislitel1 == chislitel2 && znamenatel1 == znamenatel2
  ZeroDivisionError chislitel znamenatel == OutOfPossibleValuesError op expr = False
  OutOfPossibleValuesError op expr == ZeroDivisionError chislitel znamenatel = False

--eval :: Expr -> Either Error Double
--eval = undefined
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
  