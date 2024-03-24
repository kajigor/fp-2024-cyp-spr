module Lib(Expr(..), Error(..), eval, EvalResult, Variables, simplify) where

import qualified Data.Map as Map
import Text.Printf (printf)

-- HW04: 3. (1 point) Add `String` variables to the type of expressions from HWO3; make the type of constants to be a type parameter of `Expr`.

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
HW04: 4. (2 points) Modify the evaluator so it works with the new type of constants and variables. Add the new parameter to `eval` -- an associative list which maps variable names into their numerical values.
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
  show err = case err of
    SquareRootNegative e -> printf "Square root of negative number `%s`" (show e)
    DivideByZero e -> printf "Divide by zero `%s`" (show e)
    UndefinedVariable variableName -> printf "Undefined variable `%s`" (show variableName)

instance Eq a => Eq (Error a) where
  SquareRootNegative e1 == SquareRootNegative e2 = e1 == e2
  DivideByZero e1 == DivideByZero e2 = e1 == e2
  UndefinedVariable s1 == UndefinedVariable s2 = s1 == s2
  _ == _ = False

type Variables a = [(VariableName, a)]
type EvalResult a = Either (Error a) a

-- | Evaluates an expression
-- If variables contains duplicate values for same variable name, so last value will be used.
-- >>> eval (Var "x") [("x", 2)]
-- Right 2
-- >>> eval (Var "x") [("x", 2), ("x", 3)]
-- Right 3
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
    Var variable -> case Map.lookup variable variableNameToValue of
        Just x -> Right x
        Nothing -> Left (UndefinedVariable variable)
      where
        variableNameToValue = Map.fromList variables

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


{-
HW04: 5. (2 points) Implement the function `simplify :: Expr a -> Expr a` that simplifies the expression according to common laws of arithmetics. Implement as many laws as you can think of.
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
simplify :: (Eq a, Num a) => Expr a -> Expr a
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
