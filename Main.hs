{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Text.Parsec.Token (GenLanguageDef(reservedNames))

data Op = Add | Sub | Mul | Div | Pow deriving Eq

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

data Expr a
  = Num a
  | Var String
  | Sqrt (Expr a)
  | BinOp Op (Expr a) (Expr a) deriving Eq

instance Show a => Show (Expr a) where
  show (Num x) = show x
  show (Var s) = show s
  show (Sqrt e) = "(sqrt " ++ show e ++ ")"
  show (BinOp op e1 e2) = '(' : show e1 ++ (' ' : show op) ++ " " ++ show e2 ++ ")"

data Error = DivideByZero | SquareRootOfNegative | IncorrectVariableName
  deriving (Eq, Show)

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

findVar :: Eq t => t -> [(t, b)] -> Either Error b
findVar x [] = Left IncorrectVariableName
findVar x ((y, res) : ys) = if x == y then Right res else findVar x ys

eval :: (Ord a, Floating a) => Expr a -> [(String, a)] -> Either Error a
eval (Num x) _ = Right x
eval (Var x) st = findVar x st
eval (Sqrt e) st = evalSqrt (eval e st)
eval (BinOp op e1 e2) st = evalBin op (eval e1 st) (eval e2 st)

newtype MyEither a b = MyEither (Either a b)

instance Functor (MyEither a) where
  fmap :: (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  fmap f (MyEither (Right x)) = MyEither (Right (f x))
  fmap _ (MyEither (Left x))  = MyEither (Left x)

newtype MyArrow a b = MyArrow ((->) a b)

instance Functor (MyArrow a) where
  fmap :: (a2 -> b) -> MyArrow a1 a2 -> MyArrow a1 b
  fmap f (MyArrow g) = MyArrow (f . g)

simplify :: (Floating a, Eq a) => Expr a -> Expr a
simplify (Sqrt x) = oneSimplify (Sqrt (simplify x))
simplify (BinOp op l r) = oneSimplify (BinOp op (simplify l) (simplify r))
simplify x = x

oneSimplify :: (Floating a, Eq a) => Expr a -> Expr a
oneSimplify (Sqrt (Num a)) = Num (sqrt a)
oneSimplify (BinOp op (Num a) (Num b)) =
  case calc of
    Right x -> Num x
    otherwise -> BinOp op (Num a) (Num b)
  where calc = func op a b
oneSimplify (BinOp Add (Num 0.0) b) = b
oneSimplify (BinOp Add a (Num 0.0)) = a
oneSimplify (BinOp Sub a (Num 0.0)) = a
oneSimplify (BinOp Mul (Num 1.0) b) = b
oneSimplify (BinOp Mul a (Num 1.0)) = a
oneSimplify (BinOp Mul (Num 0.0) b) = Num 0.0
oneSimplify (BinOp Mul a (Num 0.0)) = Num 0.0
oneSimplify (BinOp Div (Num 0.0) b) = Num 0.0
oneSimplify (BinOp Pow (Num 0.0) b) = Num 0.0
oneSimplify (BinOp Pow a (Num 1.0)) = a
oneSimplify expr = expr

instance Fractional a => Num (Expr a) where
  (+) = BinOp Add
  (*) = BinOp Mul
  negate = BinOp Sub (Num 0.0)

cases :: Fractional a => [(Expr a, Either Error a)]
cases = [
  (Num 1.0, Right 1.0),
  (Sqrt (Num (-1.0)), Left SquareRootOfNegative),
  (Sqrt (Num 4), Right 2),
  (BinOp
    Add
      (BinOp Sub (Num 4) (Num 1))
      (BinOp Mul (Var "two") (Num 3)),
    Right 9),
  (BinOp
    Div
      (BinOp Div (Var "four") (BinOp Mul (Num 1) (Num 0)))
      (Sqrt (Num (-1))),
    Left DivideByZero),
  (BinOp
    Div
      (Sqrt (Num (-1)))
      (BinOp Div (Var "four") (BinOp Sub (Var "x") (Var "x"))),
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
        (BinOp Sub (Var "four") (Num 1))
        (BinOp Mul (Num 2) (Num 3)))
      (BinOp
        Div
        (Num 10)
        (BinOp Sub (Num 3) (Num 1))),
    Right (9 ^ 5)),
      (BinOp
    Pow
      (BinOp
        Add
        (BinOp Sub (Var "abracadabra") (Num 1))
        (BinOp Mul (Num 2) (Num 3)))
      (BinOp
        Div
        (Num 10)
        (BinOp Sub (Num 3) (Num 1))),
    Left IncorrectVariableName)
  ]

testState :: Fractional a => [(String, a)]
testState = [
  ("x", 1.0),
  ("y", 3.0),
  ("z", -6.0),
  ("two", 2.0),
  ("four", 4.0)
  ]

hardToSimple :: Fractional a => [(Expr a, Expr a)]
hardToSimple = [
  (Num 1.0, Num 1.0),
  (Var "x", Var "x"),
  ((Num 1.0) + (Num 2.0) + (Num 3.0), Num 6.0),
  ((Num 5.0) * (Num 2.0) - (Num 3.0), Num 7.0),
  ((Num 5.0) * (Num 2.0) - (Sqrt (Num 9.0)), Num 7.0),
  ((Var "x") + (Num 0.0), (Var "x")),
  ((Num 0.0) + (Var "x"), (Var "x")),
  ((Var "x") - (Num 0.0), (Var "x")),
  ((Var "x") * (Num 0.0), Num 0.0),
  ((Num 0.0) * (Var "x"), Num 0.0),
  ((Num 1.0) * (Var "x"), (Var "x")),
  ((Var "x") * (Num 1.0), (Var "x")),
  (BinOp Div (Num 0.0) (Var "x"), (Num 0.0)), 
  (BinOp Pow (Num 0.0) (Var "x"), (Num 0.0)),  
  (BinOp Pow (Var "x") (Num 1.0), (Var "x"))
  ]

test :: (Ord a, Floating a, Show a) => Expr a -> Either Error a -> IO ()
test expr expected =
    let actual = eval expr testState in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

testSimplified :: (Ord a, Floating a, Show a) => Expr a -> Expr a -> IO ()
testSimplified expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)


main :: IO ()
main = do
  mapM_ (uncurry test) cases
  mapM_ (uncurry testSimplified) hardToSimple
