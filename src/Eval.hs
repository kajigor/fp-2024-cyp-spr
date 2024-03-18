module Eval (eval) where

import Error
import Expr
import qualified Data.Map.Strict as M

eval :: (Floating a, Ord a) => Expr a -> M.Map String a -> Either Error a 
eval (Const n) _ = Right n
eval (Var s) xs = evalVarValue s xs
eval (SquareRoot exp) xs = evalSquareRootExp (eval exp xs)
eval (BinExpr binOp exp1 exp2) xs = evalBinExp binOp (eval exp1 xs) (eval exp2 xs) 

evalVarValue :: String -> M.Map String a -> Either Error a 
evalVarValue s map = case M.lookup s map of
  Nothing -> Left NoSuchVariable
  Just a -> Right a 

evalSquareRootExp :: (Floating a, Ord a) =>  Either Error a -> Either Error a
evalSquareRootExp (Right n) = if n < 0 then Left SquareRootOfNegative else Right (sqrt n)
evalSquareRootExp err = err

evalBinExp ::  (Floating a, Ord a) => BinOperator -> Either Error a ->  Either Error a -> Either Error a
evalBinExp _ (Left err) _ = Left err
evalBinExp _ _ (Left err) = Left err
evalBinExp Plus (Right n1) (Right n2) = Right (n1 + n2)
evalBinExp Minus (Right n1) (Right n2) = Right (n1 - n2)
evalBinExp Multiply (Right n1) (Right n2) = Right (n1 * n2)
evalBinExp Power (Right n1) (Right n2) = Right (n1 ** n2)
evalBinExp Divide (Right n1) (Right n2) = evalDivideExp n1 n2

evalDivideExp :: (Floating a, Ord a) => a -> a -> Either Error a
evalDivideExp _ 0 = Left DivisionByZero
evalDivideExp n1 n2 = Right (n1 / n2)
