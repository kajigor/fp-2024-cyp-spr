module SimplifyTests where

import Test.Tasty
import Test.Tasty.HUnit
import Simplify
import Expr

simplifyTests :: TestTree
simplifyTests = testGroup "Simplify tests"
  [ testCase "basic simplify" $ assertEqual "many reductions should hold" (simplify (Expr 1.0 :+ (Expr 0.0 :* (Expr 2.0 :+ Expr 3.0)) :+ Expr 0.0 :+ (Expr 2.0 :^ Expr 1.0))) (Expr 1.0 + Expr 2.0)
  , testCase "simplify with variables" $ assertEqual "reductions on variables must hold" (simplify (Expr 0.0 + (Expr 1.0 * Var "x") + Expr 0.0)) (Var "x")
  , testCase "Variable elimination" $ assertEqual "variable can be eliminated" (simplify (1 + 0 * Var "x")) 1
  ]
