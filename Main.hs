module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

-- 1. (1 point) Design an algebraic data type `Expr` for a binary expression.
--   * A number of type `Double` is an expression.
--   * A square root of an expression is an expression.
--   * Binary operators `+`, `-`, `*`, `/`, and `^` can be used to create an expression

data Expr
  = Num Double
  | SquareRoot Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Pow Expr Expr

--2. (1 point) Make `Expr` an instance of `Show` and `Eq`.

instance Show Expr where
  show :: Expr -> String
  show expr = case expr of
    Num e -> printf "%.2f" e
    SquareRoot e -> printf "sqrt(%s)" (show e)
    Plus e1 e2 -> printf "(%s + %s)" (show e1) (show e2)
    Minus e1 e2 -> printf "(%s - %s)" (show e1) (show e2)
    Multiply e1 e2 -> printf "(%s * %s)" (show e1) (show e2)
    Divide e1 e2 -> printf "(%s / %s)" (show e1) (show e2)
    Pow e1 e2 -> printf "(%s ^ %s)" (show e1) (show e2)

instance Eq Expr where
  (==) :: Expr -> Expr -> Bool
  Num e1 == Num e2 = e1 == e2
  SquareRoot e1 == SquareRoot e2 = e1 == e2
  Plus e1 e2 == Plus e3 e4 = e1 == e3 && e2 == e4
  Minus e1 e2 == Minus e3 e4 = e1 == e3 && e2 == e4
  Multiply e1 e2 == Multiply e3 e4 = e1 == e3 && e2 == e4
  Divide e1 e2 == Divide e3 e4 = e1 == e3 && e2 == e4
  Pow e1 e2 == Pow e3 e4 = e1 == e3 && e2 == e4
  _ == _ = False

-- 3. (1 point) Design an algebraic data type `Error` for possible errors which can happen while evaluating the expression (task 5).

data Error
  = SquareRootNegative Expr
  | DivideByZero Expr
  | InvalidArgument Expr

-- 4. (1 point) Make `Error` an instance of `Show` and `Eq`.

instance Show Error where
  show :: Error -> String
  show err = case err of
    SquareRootNegative e -> printf "Square root of negative number `%s`" (show e)
    DivideByZero e -> printf "Divide by zero `%s`" (show e)


instance Eq Error where
  (==) :: Error -> Error -> Bool
  SquareRootNegative e1 == SquareRootNegative e2 = e1 == e2
  DivideByZero e1 == DivideByZero e2 = e1 == e2
  _ == _ = False

-- 5. (2 point) Implement evaluator function `eval`. It may fail when taking a square root of an negative number or when dividing by zero.

eval :: Expr -> Either Error Double
eval expr =
  case expr of
    Num e -> Right e
    SquareRoot e -> evalSquareRoot e
    Plus e1 e2 -> evalBinaryOp (+) e1 e2
    Minus e1 e2 -> evalBinaryOp (-) e1 e2
    Multiply e1 e2 -> evalBinaryOp (*) e1 e2
    Divide e1 e2 -> evalDivide e1 e2
    Pow e1 e2 -> evalBinaryOp (**) e1 e2

evalSquareRoot :: Expr -> Either Error Double
evalSquareRoot e =
  case eval e of
    Left err -> Left err
    Right val -> if val < 0 then Left (SquareRootNegative e) else Right (sqrt val)

evalBinaryOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
evalBinaryOp op e1 e2 =
  case (eval e1, eval e2) of
    (Left err1, _) -> Left err1
    (_, Left err2) -> Left err2
    (Right val1, Right val2) -> Right (val1 `op` val2)

evalDivide :: Expr -> Expr -> Either Error Double
evalDivide e1 e2 =
  case (eval e1, eval e2) of
    (Left err1, _) -> Left err1
    (_, Left err2) -> Left err2
    (Right val1, Right val2) -> if val2 == 0 then Left (DivideByZero e2) else Right (val1 / val2)

-- 6. (1 point) Provide test cases for the evaluator: `cases`. Make sure running `main` prints nothing.

cases :: [(Expr, Either Error Double)]
cases =
  [
  ((Num 1) `Plus` (Num 2), Right 3),
  ((Num 1) `Divide` (Num 0), Left (DivideByZero (Num 0))),
  ((Num 2) `Pow` (Num 3), Right 8),
  ((Num 2) `Pow` (Num (-3)), Right 0.125),
  ((Num (-2)) `Pow` (Num (-3)), Right (-0.125)),
  (SquareRoot (Num 4), Right 2),
  (SquareRoot (Num (-4)), Left (SquareRootNegative (Num (-4)))),
  ((Num 1) `Plus` (SquareRoot (Num 4)), Right 3),
  ((Num 1) `Divide` (SquareRoot (Num 4)), Right 0.5),
  ((Num 2) `Pow` (SquareRoot (Num (-4))), Left (SquareRootNegative (Num (-4)))),
  ((Num 1) `Plus` ((Num 2) `Pow` (SquareRoot (Num 4))), Right 5),
  ((Num 1) `Plus` ((Num 2) `Pow` (SquareRoot (Num (-4)))), Left (SquareRootNegative (Num (-4)))),
  ((Num 1) `Minus` (Num 2), Right (-1)),
  ((Num 1) `Minus` (SquareRoot (Num 4)), Right (-1)),
  ((Num 1) `Minus` ((Num 2) `Pow` (SquareRoot (Num 4))), Right (-3)),
  ((Num 1) `Multiply` (Num 2), Right 2),
  ((Num 1) `Multiply` (SquareRoot (Num 4)), Right 2),
  ((Num 1) `Multiply` ((Num 2) `Pow` (SquareRoot (Num 4))), Right 4),
  ((Num 1) `Multiply` ((Num 1) `Minus` (Num 2)), Right (-1)),
  ((Num 1) `Divide` (Num 2), Right 0.5),
  ((Num 1) `Divide` (SquareRoot (Num 4)), Right 0.5),
  ((Num 1) `Divide` ((Num 2) `Pow` (SquareRoot (Num 4))), Right 0.25),
  (Num 1, Right 1),
  (Num 0, Right 0),
  (Num (-1), Right (-1)),
  (((Num 2) `Pow` (SquareRoot (SquareRoot (Num 16)))) `Divide` (Num 2), Right 2)
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
  