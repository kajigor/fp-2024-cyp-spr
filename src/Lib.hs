module Lib (eval, simplify) where

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))
import Data.Map (lookup, fromList)

--defineOperationEvaluator :: Num a => Operator2 -> (a -> a -> a)
defineOperationEvaluator op
  | op == Div = (/)
  | op == Mul = (*)
  | op == Plus = (+)
  | op == Min = (-)
  | op == In = (**)


--eval :: Num a => Expr a -> [(String, a)] -> Either Error a
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


rule :: Eq a => Num a => Expr a -> Expr a
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


simplify :: Eq a => Num a => Expr a -> Expr a
simplify (Arg value) = rule (Arg value)

simplify (Var variable) = rule (Var variable)

simplify (Marg anyOperator1 expr) = rule (Marg anyOperator1 (simplify expr))

simplify (CE expr1 op expr2) = rule (CE (simplify expr1) op (simplify expr2))
