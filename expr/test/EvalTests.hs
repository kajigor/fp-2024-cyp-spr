module EvalTests where

import Test.Tasty
import Test.Tasty.HUnit
import Eval
import Expr

evalTests :: TestTree
evalTests = testGroup "Eval tests"
  [ testCase "base test" $ assertEqual "eval on number is number" (eval (Expr 1.0) []) (Right 1.0)
  , testCase "Sq test" $ assertEqual "Sq must be square root" (eval (Sq (Expr 4.0)) []) (Right 2.0)
  , testCase "base Error" $ assertEqual "taking sqrt of negative must fail the whole calculation" (eval (Expr 1.0 :+ Sq (Expr (-4.0))) []) (Left (Error "sqrt from negative taken"))
  , testCase "base operations" $ assertEqual "eval must calculate expressions" (eval ((Expr 2.0 :^ Expr 3.0) :+ ((Expr 2.0 :* Expr 3.0) :- (Expr 4.0 :/ Expr 1.0))) []) (Right 10.0)
  , testCase "multiple errors" $ assertEqual "from multiple errors we keep first in recursion" (eval ((Expr 1.0 :/ Expr 0.0) :+ Sq (Expr (-1.0))) []) (Left (Error "divided by zero"))
  , testCase "negative base error" $ assertEqual "no negative base allowed" (eval (Expr (-1.0) :^ Expr 3.0) []) (Left (Error "power used with non-positive base"))
  , testCase "zero base error" $ assertEqual "base must be greater than zero" (eval (Expr 0.0 :^ Expr (-3.0)) []) (Left (Error "power used with non-positive base"))
  , testCase "base variable substitution" $ assertEqual "eval must substitute variable" (eval (Var "x") [("x", 8.0)]) (Right 8.0)
  , testCase "variable substitution without value" $ assertEqual "eval must fail if variable is unknown" (eval (Var "x") []) (Left (Error "Unknown variable: x"))
  , testCase "variable substitution with too many values" $ assertEqual "eval must fail for multiple definitions of variable" (eval (Var "x") [("x", 2), ("x", 3)]) (Left (Error "Non-unique variable value: x"))
  , testCase "base variable substitution with other error" $ assertEqual "eval with variables cannot avoid errors" (eval ((Var "x") :+ (Sq (Expr (-1.0)))) [("x", 8.0)]) (Left (Error "sqrt from negative taken"))
  ]
