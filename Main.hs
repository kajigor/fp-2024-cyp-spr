module Main where

import Text.Printf (printf)
import qualified Data.Map as Map
import Control.Monad (unless)


-- 1. (1 point) What is the `Functor` instance of `Either`? Implement the instance and prove that the functor laws hold.
newtype MyEither a b = MyEither (Either a b)

instance Functor (MyEither a) where
    fmap f (MyEither (Right x)) = MyEither (Right (f x))
    fmap _ (MyEither (Left x)) = MyEither (Left x)

{-
Proof: we need to check 2 conditions:
1. fmap id == id
2. fmap (f . g) == fmap f . fmap g
Let's check the first one: fmap id == id
  fmap f (Right x) = Right (f x) <=> fmap id (Right x) = Right (id x) <=> fmap (Right x) = Right x
  fmap f (Left x) = Left x

Let's check the second one:
  fmap (f . g) (Right x) == Right ((f . g) x) = Right (f (g(x))
  fmap (f . g) (Left x) = Left x
  (fmap f . fmap g) (Right x) = fmap f (fmap g (Right x))
                              = fmap f (Right (g x))
                              = Right (f (g x))
 (fmap f . fmap g) (Left x) = fmap f (fmap g (Left x))
                            = fmap f (Left x)
                            = Left x
what was needed to prove that the functor laws hold
-}


{-
2. (1 point) What is the `Functor` instance of an arrow type? Hint: consider the type `a -> b` in its prefix notation: `(->) a b`.
Implement the instance and prove that the functor laws hold.
-}
newtype MyFunction a b = MyFunction (a -> b)
instance Functor (MyFunction a) where
    fmap f (MyFunction g) = MyFunction (f . g)

{-
 Proof:
 fmap id g = (\x -> id (g x))
          = (\x -> g x)
          = g
 fmap (f . g) h = (\x -> (f . g) (h x))
               = (\x -> f (g (h x)))
               = fmap f (g . h)
               = (fmap f) . (g . h)
 So fmap (f . g) = fmap f . fmap g holds
-}

-- 3. (1 point) Add `String` variables to the type of expressions from HWO3; make the type of constants to be a type parameter of `Expr`.

data Expr a
  = Var String
  | Num a
  | SquareRoot (Expr a)
  | Plus (Expr a) (Expr a)
  | Minus (Expr a) (Expr a)
  | Multiply (Expr a) (Expr a)
  | Divide (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)

instance Show a => Show (Expr a) where
  show :: (Expr a) -> String
  show expr = case expr of
    Var v -> show v
    Num a -> show a
    SquareRoot a -> printf "sqrt(%s)" (show a)
    Plus e1 e2 -> printf "(%s + %s)" (show e1) (show e2)
    Minus e1 e2 -> printf "(%s - %s)" (show e1) (show e2)
    Multiply e1 e2 -> printf "(%s * %s)" (show e1) (show e2)
    Divide e1 e2 -> printf "(%s / %s)" (show e1) (show e2)
    Pow e1 e2 -> printf "(%s ^ %s)" (show e1) (show e2)

instance Eq a => Eq (Expr a) where
  (==) :: (Expr a) -> (Expr a) -> Bool
  Var s1 == Var s2 = s1 == s2
  Num e1 == Num e2 = e1 == e2
  SquareRoot e1 == SquareRoot e2 = e1 == e2
  Plus e1 e2 == Plus e3 e4 = e1 == e3 && e2 == e4
  Minus e1 e2 == Minus e3 e4 = e1 == e3 && e2 == e4
  Multiply e1 e2 == Multiply e3 e4 = e1 == e3 && e2 == e4
  Divide e1 e2 == Divide e3 e4 = e1 == e3 && e2 == e4
  Pow e1 e2 == Pow e3 e4 = e1 == e3 && e2 == e4
  _ == _ = False

{-
4. (2 points) Modify the evaluator so it works with the new type of constants and variables. Add the new parameter to `eval` -- an associative list which maps variable names into their numerical values.
* `eval :: Expr a -> [(String, a)] -> Either Error a`
* Now a new type of error becomes possible. Modify `Error` type accordingly.
* Make sure to update tests.
-}

type VariableName = String
data Error a
  = SquareRootNegative (Expr a)
  | DivideByZero (Expr a)
  | UndefinedVariable VariableName

instance Show a => Show (Error a) where
  show :: (Error a) -> String
  show err = case err of
    SquareRootNegative e -> printf "Square root of negative number `%s`" (show e)
    DivideByZero e -> printf "Divide by zero `%s`" (show e)
    UndefinedVariable variableName -> printf "Undefined variable `%s`" (show variableName)

instance Eq a => Eq (Error a) where
  (==) :: (Error a) -> (Error a) -> Bool
  SquareRootNegative e1 == SquareRootNegative e2 = e1 == e2
  DivideByZero e1 == DivideByZero e2 = e1 == e2
  UndefinedVariable s1 == UndefinedVariable s2 = s1 == s2
  _ == _ = False

type Variables a = [(VariableName, a)]
type EvalResult a = Either (Error a) a

eval :: (Eq a, Ord a, Floating a) => Expr a -> Variables a -> EvalResult a
eval expr variables =
  case expr of
    Num e -> Right e
    SquareRoot e -> evalSquareRoot e variables
    Plus e1 e2 -> evalBinaryOp (+) e1 e2 variables
    Minus e1 e2 -> evalBinaryOp (-) e1 e2 variables
    Multiply e1 e2 -> evalBinaryOp (*) e1 e2 variables
    Divide e1 e2 -> evalDivide e1 e2 variables
    Pow e1 e2 -> evalBinaryOp (**) e1 e2 variables
    Var variable -> case lookup variable variables of
        Just x -> Right x
        Nothing -> Left (UndefinedVariable variable)

evalSquareRoot :: (Ord a, Floating a) => (Expr a) -> Variables a -> EvalResult a
evalSquareRoot e variables =
  case eval e variables of
    Left err -> Left err
    Right val -> if val < 0 then Left (SquareRootNegative e) else Right (sqrt val)

evalBinaryOp :: (Eq a, Ord a, Floating a) => (a -> a -> a) -> (Expr a) -> (Expr a) -> Variables a -> EvalResult a
evalBinaryOp op e1 e2 variables =
  case (eval e1 variables, eval e2 variables) of
    (Left err1, _) -> Left err1
    (_, Left err2) -> Left err2
    (Right val1, Right val2) -> Right (val1 `op` val2)

evalDivide :: (Eq a, Ord a, Floating a) => (Expr a) -> (Expr a) -> Variables a -> EvalResult a
evalDivide e1 e2 variables =
  case (eval e1 variables, eval e2 variables) of
    (Left err1, _) -> Left err1
    (_, Left err2) -> Left err2
    (Right val1, Right val2) -> if val2 == 0 then Left (DivideByZero e2) else Right (val1 / val2)

evalCases :: [(Expr Double, [(String, Double)] , Either (Error Double) Double)]
evalCases =
  [
    ((Num 1) `Plus` (Num 2), [], Right 3),
    ((Num 1) `Divide` (Num 0), [], Left (DivideByZero (Num 0))),
    ((Num 2) `Pow` (Num 3), [], Right 8),
    ((Num 2) `Pow` (Num (-3)), [], Right 0.125),
    ((Num (-2)) `Pow` (Num (-3)), [], Right (-0.125)),
    (SquareRoot (Num 4), [], Right 2),
    (SquareRoot (Num (-4)), [], Left (SquareRootNegative (Num (-4)))),
    ((Num 1) `Plus` (SquareRoot (Num 4)), [], Right 3),
    ((Num 1) `Divide` (SquareRoot (Num 4)), [], Right 0.5),
    ((Num 2) `Pow` (SquareRoot (Num (-4))), [], Left (SquareRootNegative (Num (-4)))),
    ((Num 1) `Plus` ((Num 2) `Pow` (SquareRoot (Num 4))), [], Right 5),
    ((Num 1) `Plus` ((Num 2) `Pow` (SquareRoot (Num (-4)))), [], Left (SquareRootNegative (Num (-4)))),
    ((Num 1) `Minus` (Num 2), [], Right (-1)),
    ((Num 1) `Minus` (SquareRoot (Num 4)), [], Right (-1)),
    ((Num 1) `Minus` ((Num 2) `Pow` (SquareRoot (Num 4))), [], Right (-3)),
    ((Num 1) `Multiply` (Num 2), [], Right 2),
    ((Num 1) `Multiply` (SquareRoot (Num 4)), [], Right 2),
    ((Num 1) `Multiply` ((Num 2) `Pow` (SquareRoot (Num 4))), [], Right 4),
    ((Num 1) `Multiply` ((Num 1) `Minus` (Num 2)), [], Right (-1)),
    ((Num 1) `Divide` (Num 2), [], Right 0.5),
    ((Num 1) `Divide` (SquareRoot (Num 4)), [], Right 0.5),
    ((Num 1) `Divide` ((Num 2) `Pow` (SquareRoot (Num 4))), [], Right 0.25),
    (Num 1, [], Right 1),
    (Num 0, [], Right 0),
    (Num (-1), [], Right (-1)),
    (((Num 2) `Pow` (SquareRoot (SquareRoot (Num 16)))) `Divide` (Num 2), [], Right 2),
    ((Var "x") `Plus` (Var "x"), [("x", 1)], Right 2),
    ((Var "x") `Plus` (Var "y"), [("x", 1), ("y", 2)], Right 3),
    ((( Var "x") `Plus` (Var "y")) `Plus` (Var "z"), [("x", 1), ("y", 2), ("z", 3)], Right 6),
    ((((( Var "x") `Plus` (Var "y")) `Plus` (Var "z")) `Plus` (Var "w")), [("x", 1), ("y", 2), ("z", 3), ("w", 4)], Right 10)
  ]

testEval :: Expr Double -> Variables Double  -> EvalResult Double -> IO ()
testEval expr variables expected =
    let actual = eval expr variables in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

{-
5. (2 points) Implement the function `simplify :: Expr a -> Expr a` that simplifies the expression according to common laws of arithmetics. Implement as many laws as you can think of.
    * Example laws: `0 * 42 == 0`, `1 * x == x`.
    * Add tests.
-}

{-
Laws:
- 0 * 42 = 0
- 42 * 0 = 0
- 1 * x = x
- x * 1 = x
- x + 0 = x
- 0 + x = x
- x - 0 = x
- 2 ^ 0 = 1
- 2 ^ 1 = 2
- x / 1 = x
- sqrt 0 = 0

Corner cases:
- 0 - x = -x
-}
simplify :: (Eq a, Ord a, Floating a) => Expr a -> Expr a
simplify expr =
  case expr of
    Multiply (Num 0) e -> Num 0
    Multiply e (Num 0) -> Num 0
    Multiply (Num 1) e -> e
    Multiply e (Num 1) -> e
    Plus (Num 0) e -> e
    Plus e (Num 0) -> e
    Minus e (Num 0) -> e
    Pow e (Num 0) -> Num 1
    Pow e (Num 1) -> e
    Divide e (Num 1) -> e
    SquareRoot (Num 0) -> Num 0
    Plus e1 e2 -> Plus (simplify e1) (simplify e2)
    Minus e1 e2 -> Minus (simplify e1) (simplify e2)
    Multiply e1 e2 -> Multiply (simplify e1) (simplify e2)
    Divide e1 e2 -> Divide (simplify e1) (simplify e2)
    Pow e1 e2 -> Pow (simplify e1) (simplify e2)
    SquareRoot e -> SquareRoot (simplify e)
    Var _ -> expr
    _ -> expr



simplifyCases :: [(Expr Double, Expr Double)]
simplifyCases =
  [
    ((Num 0) `Multiply` (Num 42),  (Num 0)),
    ((Num 5) `Multiply` (Num 42),  ((Num 5) `Multiply` (Num 42))),
    ((Num 42) `Multiply` (Num 0),  (Num 0)),
    ((Num 42) `Multiply` (Num 5),  ((Num 42) `Multiply` (Num 5))),
    ((Num 1) `Multiply` (Num 42),  (Num 42)),
    ((Num 42) `Multiply` (Num 1),  (Num 42)),
    ((Num 1) `Plus` (Num 0), (Num 1)),
    ((Num 1) `Plus` (Num 5), ((Num 1) `Plus` (Num 5))),
    ((Num 0) `Plus` (Num 1), (Num 1)),
    ((Num 1) `Minus` (Num 0), (Num 1)),
    ((Num 1) `Minus` (Num 5), ((Num 1) `Minus` (Num 5))),
    ((Num 0) `Minus` (Num 1), ((Num 0) `Minus` (Num 1))),
    ((Num 2) `Pow` (Num 0), (Num 1)),
    ((Num 2) `Pow` (Num 1), (Num 2)),
    ((Num 42) `Divide` (Num 1), (Num 42)),
    ((Num 42) `Divide` (Num 2), ((Num 42) `Divide` (Num 2)))
  ]

testSimplify :: Expr Double -> Expr Double -> IO ()
testSimplify expr simplified =
    let actual = simplify expr in
    unless (actual == simplified) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be `%s` but it was `%s`\n" (show expr) (show simplified) (show actual)

main :: IO ()
main = do
  mapM_ runEvalTest evalCases
  mapM_ runSimplifyTest simplifyCases
  where
    runEvalTest (expr, expected, vars) = testEval expr expected vars
    runSimplifyTest (expr, simplified) = testSimplify expr simplified
