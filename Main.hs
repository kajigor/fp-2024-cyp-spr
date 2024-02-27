module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either ( isLeft, fromRight, isRight )

data Op = Add | Sub | Mul | Div | Pow

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

data Expr
  = Num Double
  | Sqrt Expr
  | BinOp Op Expr Expr

instance Show Expr where
  show (Num x) = show x
  show (Sqrt e) = "(sqrt " ++ show e ++ ")"
  show (BinOp op e1 e2) = '(' : show e1 ++ (' ' : show op) ++ " " ++ show e2 ++ ")"

data Error = DivideByZero | SquareRootOfNegative
  deriving (Eq, Show)

func :: Op -> Double -> Double -> Either Error Double
func Add a b = Right (a + b)
func Sub a b = Right (a - b)
func Mul a b = Right (a * b)
func Div a 0 = Left DivideByZero
func Div a b = Right (a / b)
func Pow a b = Right (a ** b)

evalSqrt :: Either Error Double -> Either Error Double
evalSqrt (Right v)
  | v < 0 = Left SquareRootOfNegative
  | otherwise = Right (sqrt v)
evalSqrt err = err

evalBin :: Op -> Either Error Double -> Either Error Double -> Either Error Double
evalBin op (Right v1) (Right v2) = func op v1 v2
evalBin _ (Right _) err = err
evalBin _ err _ = err

eval :: Expr -> Either Error Double
eval (Num x) = Right x
eval (Sqrt e) = evalSqrt (eval e)
eval (BinOp op e1 e2) = evalBin op (eval e1) (eval e2)

instance Eq Expr where
  e1 == e2 = eval e1 == eval e2


cases :: [(Expr, Either Error Double)]
cases = [
  (Num 1, Right 1),
  (Sqrt (Num (-1)), Left SquareRootOfNegative),
  (Sqrt (Num 4), Right 2),
  (BinOp
    Add
      (BinOp Sub (Num 4) (Num 1))
      (BinOp Mul (Num 2) (Num 3)),
    Right 9),
  (BinOp
    Div
      (BinOp Div (Num 4) (BinOp Mul (Num 1) (Num 0)))
      (Sqrt (Num (-1))),
    Left DivideByZero),
  (BinOp
    Div
      (Sqrt (Num (-1)))
      (BinOp Div (Num 4) (BinOp Sub (Num 1) (Num 1))),
    Left SquareRootOfNegative),
  (BinOp
    Div
    (Sqrt (Num 100))
    (BinOp Div (Num 10) (BinOp Sub (Num 6) (Num 1))),
    Right 5),
  (BinOp
    Pow
      (BinOp
        Add
        (BinOp Sub (Num 4) (Num 1))
        (BinOp Mul (Num 2) (Num 3)))
      (BinOp 
        Div 
        (Num 10) 
        (BinOp Sub (Num 3) (Num 1))),
    Right (9 ^ 5))
  ]

test :: Expr -> Either Error Double -> IO ()
test expr expected =
    let actual = eval expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)


main :: IO ()
main = do
  mapM_ (uncurry test) cases
