module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Map (lookup, fromList)


data MyEither a b = L a | R b deriving (Show, Read)


instance Functor (MyEither a) where
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap f (L value) = L value
  fmap f (R value) = R (f value)


-- Type for unary operator
data Operator1 = Sqrt | Neg deriving (Eq)

instance Show Operator1 where
  show Sqrt = "√"
  show Neg = "-"


-- Type for binary operator
data Operator2 = Div | Mul | Plus | Min | In deriving (Eq)

instance Show Operator2 where
  show Div = "/"
  show Mul = "*"
  show Plus = "+"
  show Min = "-"
  show In = "^"


-- Type for expression
data Expr = Arg Double | Var String | Marg Operator1 Expr | CE Expr Operator2 Expr

instance Show Expr where
  show (Arg value) = show value
  show (Var variable) = show variable
  show (Marg op value)
    | op == Neg = "(" ++ (show op) ++ (show value) ++ ")"
    | op == Sqrt = (show op) ++ "(" ++ (show value) ++ ")"
  show (CE expr1 op expr2) = (show expr1) ++ (show op) ++ (show expr2)

instance Eq Expr where
  Arg value1 == Arg value2 = value1 == value2
  Var variable1 == Var variable2 = variable1 == variable2
  Marg op1 value1 == Marg op2 value2 =
   op1 == op2 && value1 == value2
  CE expr11 op1 expr12 == CE expr21 op2 expr22 =
   expr11 == expr21 && op1 == op2 && expr12 == expr22
  _ == _ = False


data Error =
  OutOfPossibleValuesError Operator1 Double
  | ZeroDivisionError Double
  | IncorrectDegreeOfValue Double
  | VariableDoesNotExist String

instance Show Error where
  show (OutOfPossibleValuesError op value) = "OutOfPossibleValuesError: operator "
   ++ (show op) ++ " can not handle expression " ++ (show value)
  show (ZeroDivisionError chislitel) = "ZeroDivisionError: numerator "
   ++ (show chislitel) ++ " can not be divided by 0"
  show (IncorrectDegreeOfValue value) = "IncorrectDegreeOfValue: value 0 in "
   ++ (show value) ++ " degree does not exist"
  show (VariableDoesNotExist variable) = "VariableDoesNotExist: variable "
   ++ variable ++ " is not defined"

instance Eq Error where
  OutOfPossibleValuesError op1 value1 == OutOfPossibleValuesError op2 value2 =
   op1 == op2 && value1 == value2
  ZeroDivisionError chislitel1 == ZeroDivisionError chislitel2 =
   chislitel1 == chislitel2
  IncorrectDegreeOfValue value1 == IncorrectDegreeOfValue value2 = value1 == value2
  VariableDoesNotExist var1 == VariableDoesNotExist var2 = var1 == var2
  _ == _ = False


defineOperationEvaluator :: Operator2 -> (Double -> Double -> Double)
defineOperationEvaluator op
  | op == Div = (/)
  | op == Mul = (*)
  | op == Plus = (+)
  | op == Min = (-)
  | op == In = (**)


eval :: Expr -> [(String, Double)] -> Either Error Double
eval (Arg value) list = Right value

eval (Var variable) list = let search_result = Data.Map.lookup variable (fromList list)
                           in  case (search_result) of
                             Just x -> Right x
                             Nothing -> Left (VariableDoesNotExist variable)

eval (Marg Neg expr) list = case (eval expr list) of
  Right result -> Right (-result)
  Left exception -> Left exception

eval (Marg Sqrt expr) list = case (eval expr list) of
  Right result -> if (result >= 0) then Right (result**0.5)
   else Left (OutOfPossibleValuesError Sqrt result)
  Left exception -> Left exception

eval (CE expr1 op expr2) list = case (eval expr1 list) of
  Right result1 -> case (eval expr2 list) of
    Right result2 -> case (op) of
      Div -> if (result2 /= 0) then Right (defineOperationEvaluator op result1 result2)
       else Left (ZeroDivisionError result1)
      In -> if (result1 == 0 && result2 < 0) then Left (IncorrectDegreeOfValue result2)
       else Right (defineOperationEvaluator op result1 result2)
      _ -> Right (defineOperationEvaluator op result1 result2)
    Left exception -> Left exception
  Left exception -> Left exception


rule :: Expr -> Expr
rule (Arg value) = Arg value

rule (Var variable) = Var variable

rule (Marg Neg (Marg Neg expr)) = expr
rule (Marg Neg (Arg 0)) = Arg 0
rule (Marg Sqrt (Arg 0)) = Arg 0
rule (Marg op expr) = Marg op expr

rule (CE (Arg 0) Mul expr2) = Arg 0
rule (CE expr1 Mul (Arg 0)) = Arg 0

rule (CE (Arg 1) Mul expr2) = expr2
rule (CE expr1 Mul (Arg 1)) = expr1

rule (CE (Arg 0) Plus expr2) = expr2
rule (CE expr1 Plus (Arg 0)) = expr1

rule (CE (Arg 0) Min expr2) = (Marg Neg expr2)
rule (CE expr1 Min (Arg 0)) = expr1

rule (CE expr1 Div (Arg 1)) = expr1
rule (CE expr1 Div (Marg Neg (Arg 1))) = Marg Neg expr1

rule (CE expr1 op expr2)
  | (expr1 == expr2) && (op == Min) = Arg 0
  | (expr1 == expr2) && (op == Div) = Arg 1
  | (expr1 == expr2) && (op == Plus) = CE (Arg 2) Mul expr1
  | (expr1 == expr2) && (op == Mul) = CE expr1 In (Arg 2)
  | otherwise = CE expr1 op expr2


simplify :: Expr -> Expr
simplify (Arg value) = rule (Arg value)

simplify (Var variable) = rule (Var variable)

simplify (Marg anyOperator1 expr) = rule (Marg anyOperator1 (simplify expr))

simplify (CE expr1 op expr2) = rule (CE (simplify expr1) op (simplify expr2))


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
 (CE (Arg 2) In (Arg 3), Right 8),
 (CE (Arg 0) In (Arg 2), Right 0),
 (CE (Arg 5) In (Arg 0), Right 1),
 (CE (Arg 0) In (Arg 0), Right 1),
 -- Simple binary expression with division by zero = ZeroDivisionError
 (CE (Arg 6) Div (Arg 0), Left (ZeroDivisionError 6)),
 (CE (Arg 0) Div (Arg 0), Left (ZeroDivisionError 0)),
 (CE (Arg 4) Plus (CE (Arg 5) Min (Arg 6)), Right 3),
 -- Simple binary expression with zero-degree = IncorrectDegreeOfValue
 (CE (Arg 0) In (Arg (-5)), Left (IncorrectDegreeOfValue (-5))),
 -- Complex expressions = Double
 (CE (CE (Arg 5) Min (Arg 6)) Plus (Arg 4), Right 3),
 (CE (Marg Neg (Arg 7)) Min (Marg Neg (Arg 3)), Right (-4)),
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg 9))) Plus (CE (Arg 0) Plus (Marg Neg (Arg 5))), Right 1),
 -- Complex expression with error in sub-expression = Exception of first sub expression Error
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (-9)))) Plus (CE (Arg 0) Plus (Marg Neg (Arg 5))), Left (OutOfPossibleValuesError Sqrt (-9))),
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (9)))) Plus (CE (Arg 2) Div (Marg Neg (Arg 0))), Left (ZeroDivisionError 2)),
 (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (-9)))) Plus (CE (Arg 2) Div (Marg Neg (Arg 0))), Left (OutOfPossibleValuesError Sqrt (-9))),
 -- Variables
 (Var "x", Right 2),
 (Var "y", Right 0),
 (CE (Var "x") Mul (Arg 3), Right 6),
 (CE (Var "x") Mul (Var "y"), Right 0),
 (CE (Var "x") Plus (Var "y"), Right 2),
 (CE (Var "x") In (Var "y"), Right 1),
 (CE (Var "y") In (Arg (-2)), Left (IncorrectDegreeOfValue (-2))),
 (CE (Var "z") Mul (Arg 3), Left (VariableDoesNotExist "z")),
 (CE (Arg 3) Mul (Var "z"), Left (VariableDoesNotExist "z"))
 ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr [("x", 2), ("y", 0)] in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)


main :: IO ()
main = do
  mapM_ (uncurry test) cases
