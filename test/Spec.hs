-- HW05: (4 points) Replace your hand-written test suite with a tasty test suite. Ensure good coverage.

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import Lib (Expr(..), Error(..), eval, Variables, EvalResult, simplify)

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

testEval :: (Expr Double, Variables Double, Either (Error Double) Double) -> TestTree
testEval (expr, vars, expected) =
  testCase (printf "eval (%s) == %s" (show expr) (show expected)) $
    let actual = eval expr vars
    in assertEqual "" expected actual

testSimplify :: (Expr Double, Expr Double) -> TestTree
testSimplify (expr, simplified) =
  testCase (printf "simplify (%s) == %s" (show expr) (show simplified)) $
    let actual = simplify expr
    in assertEqual "" simplified actual

main :: IO ()
main = defaultMain $ testGroup "Tests"
         [ testGroup "Evaluation Tests" $ map testEval evalCases
         , testGroup "Simplify Tests" $ map testSimplify simplifyCases
         ]