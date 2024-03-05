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

data Expr a =
  Number a
  | Root (Expr a)
  | BinOp (Expr a) (Expr a) Operation
  | Variable String
  deriving Eq

instance Show a => Show (Expr a) where
  show (Number x) = show x
  show (Root x) = "√(" ++ show x ++ ")"
  show (BinOp e1 e2 op) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (Variable name) = name


instance Num a => Num (Expr a) where
  (+) e1 e2 = BinOp e1 e2 Plus
  (-) e1 e2 = BinOp e1 e2 Minus
  (*) e1 e2 = BinOp e1 e2 Mul
  negate e = BinOp (Number 0) e Minus
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger


data Error =
  ZeroDiv
  | NegRoot
  | UndefinedVariable String
  deriving Eq

instance Show Error where
  show ZeroDiv = "A division by zero occured"
  show NegRoot = "A square root of a negative number was taken"
  show (UndefinedVariable name) = "The variable " ++ name ++ " is not defined"

eval :: RealFloat a =>  Expr a -> [(String, a)] -> Either Error a
eval (Number x) _ = Right x
eval (Root x) env =
    case eval x env of
    Right result -> if result >= 0 then Right $ sqrt result else Left NegRoot
    Left err -> Left err
eval (BinOp x y op) env =
    case eval x env of
    Right result1 -> case eval y env of
      Right result2 -> applyOp op result1 result2
      Left err -> Left err
    Left err -> Left err
eval (Variable name) vars =
    case lookup name vars of
    Just x -> Right x
    Nothing -> Left (UndefinedVariable name)


applyOp :: RealFloat a => Operation -> a -> a -> Either Error a
applyOp Div x y
  | y == 0 = Left ZeroDiv
  | otherwise = Right (x / y)
applyOp Pow x y
  | isNaN (x ** y) = Left NegRoot
  | otherwise = Right (x ** y)
applyOp op x y = Right (opToFun op x y)
  where opToFun :: RealFloat a => Operation -> a -> a -> a
        opToFun Plus = (+)
        opToFun Minus = (-)
        opToFun Mul = (*)

simplify :: RealFloat a => Expr a -> Expr a
simplify (BinOp 0 _ Mul) = Number 0
simplify (BinOp _ 0 Mul) = Number 0
simplify (BinOp 1 x Mul) = x
simplify (BinOp x 1 Mul) = x
simplify (BinOp x 0 Plus) = x
simplify (BinOp 0 x Plus) = x
simplify (BinOp e (Number 0.5) Pow) = Root $ simplify e
simplify (BinOp x y Minus)
  | x == y = Number 0
simplify (BinOp x y op) = BinOp (simplify x) (simplify y) op
simplify (Root x)
  | simplify x == 0 = 0
  | simplify x == 1 = 1
simplify (Root x) = Root $ simplify x
simplify x = x


cases :: RealFloat a => [((Expr a, [(String, a)]), Either Error Double)]
cases = [
  ((Number 10, []), Right 10),
  ((Number $ -5, []), Right $ -5),
  ((Root (Number 16), []), Right 4),
  ((Root (Number $ -9), []), Left NegRoot),
  ((BinOp (Number 5) (Number 3) Plus, []), Right 8),
  ((BinOp (Number 7) (Number 2) Minus, []), Right 5),
  ((BinOp (Number 2) (Number 4) Mul, []), Right 8),
  ((BinOp (Number 10) (Number 2) Div, []), Right 5),
  ((BinOp (Number 10) (Number 0) Div, []), Left ZeroDiv),
  ((BinOp (Number 2) (Number 3) Pow, []), Right 8),
  ((BinOp (Number $ -2) (Number 0.5) Pow, []), Left NegRoot),
  (
    (BinOp (Root (Number 64)) (Number 2) Plus, []),
    Right 10
  ),
  (
    (BinOp (Root (Number $ -9)) (Number 3) Mul, []),
    Left NegRoot
  ),
  (
    (BinOp (BinOp (Number 5) (Number 0) Div) (Number 3) Mul, []),
    Left ZeroDiv
  ),
  (
    (BinOp (BinOp (Number 4) (Number 3) Mul) (Number 2) Div, []),
    Right 6
  ),
  ((Variable "x", [("x", 10)]), Right 10),
  ((Variable "x", []), Left $ UndefinedVariable "x"),
  ((BinOp (Variable "x") (Number 2) Plus, [("x", 10)]), Right 12),
  ((BinOp (Variable "x") (Number 2) Plus, []), Left $ UndefinedVariable "x"),
  ((BinOp (Number 2) (Variable "x") Div, [("x", 0)]), Left ZeroDiv),
  ((BinOp (Variable "x") (Variable "x") Plus, [("x", 10)]), Right 20),
  ((BinOp (Variable "x") (Variable "y") Mul, [("x", 10), ("y", 2)]), Right 20)
  ]

casesSimplify :: RealFloat a => [(Expr a, Expr a)]
casesSimplify = [
  (BinOp 5 (Number 0.5) Pow, Root 5),
  (BinOp 5 0 Mul, 0),
  (BinOp 0 5 Mul, 0),
  (BinOp (BinOp 8 7 Plus) 1 Mul, BinOp 8 7 Plus),
  (BinOp 1 (BinOp 8 7 Plus) Mul, BinOp 8 7 Plus),
  (BinOp (BinOp 8 7 Plus) 0 Plus, BinOp 8 7 Plus),
  (BinOp 0 (BinOp 8 7 Plus) Plus, BinOp 8 7 Plus),
  (BinOp (Variable "x") (Variable "x") Minus, 0),
  (BinOp (BinOp 0 5 Mul) (BinOp (BinOp 8 7 Plus) 0 Plus) Minus, BinOp 0 (BinOp 8 7 Plus) Minus),
  (Root (BinOp 0 5 Mul), 0),
  (Root (BinOp 1 1 Mul), 1),
  (Variable "x", Variable "x"),
  (Number 10, Number 10)
  ]

test :: (RealFloat a, Show a) => (Expr a, [(String, a)]) -> Either Error a -> IO ()
test (expr, env) expected =
    let actual = eval expr env in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

testSimplify :: (RealFloat a, Show a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

newtype MyEither a b = MyEither (Either a b) deriving (Show, Eq)

instance Functor (MyEither a) where
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap f (MyEither (Left x)) = MyEither (Left x)
  fmap f (MyEither (Right x)) = MyEither (Right (f x))

{- Proof:

  Identity :
  - fmap id (Left a)
    = Left (id a)
    = Left a
  - fmap id (Right a)
    = Right (id a)
    = Right a

  Composition:
  Let's explicitly evaluate all the cases:
  Left a:
  - fmap (f . g) (Left a)
    = Left a
  - (fmap f . fmap g) (Left a)
    = fmap f (fmap g (Left a))
    = fmap f (Left a)
    = Left a

  Right x:
  - fmap (f . g) (Right b)
    = Right ((f . g) b)
  - fmap f . fmap g (Right a)
    = fmap f (fmap g (Right b))
    = fmap f (Right (g b))
    = Right ((f . g) b)
-}

newtype MyArrow a b = MyArrow ((->) a b)

instance Functor (MyArrow a) where
  fmap :: (b -> c) -> MyArrow a b -> MyArrow a c
  fmap f (MyArrow g) = MyArrow (f . g)

{- Proof

  Identity:
    - fmap id (MyArrow a) =
      = MyArrow (id . a) =
      = MyArrow a

  Composition:
    - fmap f . fmap g (MyArrow a) =
      = fmap f MyArrow (g . a) =
      = MyArrow (f . g . a) =
      = fmap (f . g) (MyArrow a)

-}

main :: IO ()
main = do
    mapM_ (uncurry test) cases
    mapM_ (uncurry testSimplify) casesSimplify
