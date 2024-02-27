module Main where

import Text.Printf (printf)
import Control.Monad (unless)

data Operation =
  Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Show, Eq)

data Expr =
  Number Double
  | Root Expr
  | BinOp Expr Expr Operation

instance Show Expr where
  show (Number x) = show x
  show (Root x) = "√(" ++ show x ++ ")"
  show (BinOp e1 e2 op) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"


instance Eq Expr where
  (==) e1 e2 = eval e1 == eval e2

data Error =
  ZeroDiv
  | NegRoot
  deriving Eq

instance Show Error where
  show ZeroDiv = "A division by zero occured"
  show NegRoot = "A square root of a negative number was taken"

eval :: Expr -> Either Error Double
eval (Number x) = Right x
eval (Root x) =
    case eval x of
    Right result -> if result >= 0 then Right $ sqrt result else Left NegRoot
    Left err -> Left err
eval (BinOp x y op) =
    case eval x of
    Right result1 -> case eval y of
      Right result2 -> applyOp op result1 result2
      Left err -> Left err
    Left err -> Left err


applyOp :: Operation -> Double -> Double -> Either Error Double
applyOp Div x y
  | y /= 0 = Right (x / y)
  | otherwise = Left ZeroDiv
applyOp op x y = Right (opToFun op x y)
  where opToFun :: Operation -> Double -> Double -> Double
        opToFun Plus = (+)
        opToFun Minus = (-)
        opToFun Mul = (*)
        opToFun Pow = (**)

cases :: [(Expr, Either Error Double)]
cases = [
  (Number 10, Right 10),
  (Number (-5), Right (-5)),
  (Root (Number 16), Right 4),
  (Root (Number (-9)), Left NegRoot),
  (BinOp (Number 5) (Number 3) Plus, Right 8),
  (BinOp (Number 7) (Number 2) Minus, Right 5),
  (BinOp (Number 2) (Number 4) Mul, Right 8),
  (BinOp (Number 10) (Number 2) Div, Right 5),
  (BinOp (Number 10) (Number 0) Div, Left ZeroDiv),
  (BinOp (Number 2) (Number 3) Pow, Right 8),
  (
    BinOp (Root (Number 64)) (Number 2) Plus,
    Right 10
  ),
  (
    BinOp (Root (Number (-9))) (Number 3) Mul,
    Left NegRoot
  ),
  (
    BinOp (BinOp (Number 5) (Number 0) Div) (Number 3) Mul,
    Left ZeroDiv
  ),
  (
    BinOp (BinOp (Number 4) (Number 3) Mul) (Number 2) Div,
    Right 6
  )
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
