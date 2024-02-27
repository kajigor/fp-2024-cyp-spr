module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either
import Distribution.SPDX (LicenseId(AFL_1_2))

data Expr 
  = Multiply Expr Expr 
  | Sum Expr Expr
  | SquareRoot Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Number Double


instance Show Expr where 
  show (Multiply x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
  show (Sum x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
  show (SquareRoot x) = "sqrt(" ++ show x ++ ")"
  show (Div x y) = "(" ++ show x ++ "/" ++ show y ++ ")"
  show (Pow x y) = "(" ++ show x ++ "^" ++ show y ++ ")"
  show (Number x) = show x

instance Eq Expr where 
  Multiply x y == Multiply x1 y1 = x == x1 && y == y1
  Sum x y == Sum x1 y1 = x == x1 && y == y1
  SquareRoot x == SquareRoot y = x == y
  Div x y == Div x1 y1 = x == x1 && y == y1
  Pow x y == Pow x1 y1 = x == x1 && y == y1
  Number x == Number y = x == y
  _ == _ = False

data Error = DivByZero | NegativeRoot

instance Show Error where 
  show DivByZero = show "you divide something by zero!"
  show NegativeRoot = show "you take root of an negative number!"

instance Eq Error where 
  DivByZero == DivByZero = True
  NegativeRoot == NegativeRoot = True
  _ == _ = False
  

eval :: Expr -> Either Error Double 
eval (Number x) = Right x
eval (Div x y) 
  | isLeft (eval x) = eval x 
  | fromRight 1.0 (eval y) == 0.0 = Left DivByZero
  | isLeft (eval y) = eval y 
  | otherwise = Right (fromRight 1.0 (eval x) / fromRight 1.0 (eval y))
eval (Pow x y)
  | isLeft (eval x) = eval x 
  | isLeft (eval y) = eval y 
  | otherwise = Right (fromRight 1.0 (eval x) ** fromRight 1.0 (eval y))
eval (SquareRoot x)
  | isLeft (eval x) = eval x 
  | fromRight 1.0 (eval x) < 0.0 = Left NegativeRoot
  | otherwise = Right (sqrt(fromRight 1.0 (eval x)))
eval (Multiply x y) 
  | isLeft (eval x) = eval x 
  | isLeft (eval y) = eval y 
  | otherwise = Right (fromRight 1.0 (eval x) * fromRight 1.0 (eval y))
eval (Sum x y) 
  | isLeft (eval x) = eval x 
  | isLeft (eval y) = eval y 
  | otherwise = Right (fromRight 1.0 (eval x) + fromRight 1.0 (eval y))

cases :: [(Expr, Either Error Double)]
cases =  [
  (Number 4.0, Right 4.0),
  (SquareRoot (Number 4.0), Right 2.0),
  (SquareRoot (Number (-4.0)), Left NegativeRoot),
  (Sum (Number 4.0) (Multiply (Number 2.0) (Number 3.0)), Right 10.0),
  (SquareRoot (Sum (Number 4.0) (Multiply (Number 4.0) (Number 3.0))), Right 4.0),
  (Div (Number 10.0) (Number 2.0), Right 5.0),
  (Div (Number 10.0) (Number 0.0), Left DivByZero),
  (SquareRoot (Sum (Number (-4.0)) (Multiply (Number (-4.0)) (Number 3.0))), Left NegativeRoot),
  (Pow (Div (Number 10.0) (Number 5.0)) (Multiply (Number 2.0) (Number 2.0)), Right 16.0) 
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
  