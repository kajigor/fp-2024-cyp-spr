module Eval ( eval, func ) where

import Expr ( Expr (..), Op (..))
import Error ( Error (..) )

import qualified Data.Map.Strict as M

func :: (Num a, Fractional a, Floating a, Eq a) => Op -> a -> a -> Either Error a
func Add a b = Right (a + b)
func Sub a b = Right (a - b)
func Mul a b = Right (a * b)
func Div a 0 = Left DivideByZero
func Div a b = Right (a / b)
func Pow a b = Right (a ** b)

evalSqrt :: (Ord a, Num a, Floating a) => Either Error a -> Either Error a
evalSqrt (Right v)
  | v < 0 = Left SquareRootOfNegative
  | otherwise = Right (sqrt v)
evalSqrt err = err

evalBin :: (Floating a, Eq a) => Op -> Either Error a -> Either Error a -> Either Error a
evalBin op (Right v1) (Right v2) = func op v1 v2
evalBin _ (Right _) err = err
evalBin _ err _ = err

eval :: (Ord a, Floating a) => Expr a -> M.Map String a -> Either Error a
eval (Num x) _ = Right x
eval (Var x) st = case M.lookup x st of
  Just x  -> Right x
  Nothing -> Left IncorrectVariableName
eval (Sqrt e) st = evalSqrt (eval e st)
eval (BinOp op e1 e2) st = evalBin op (eval e1 st) (eval e2 st)