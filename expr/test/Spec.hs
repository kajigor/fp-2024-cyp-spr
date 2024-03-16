
import Expr
import Error
import Simplify
import Eval
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import Control.Exception (assert)



evalTests :: TestTree
evalTests = testGroup "Eval tests"
  [ testCase "Base test1" $ assertEqual "eval on number is number" (eval (Const 777) M.empty) (Right 777)
  , testCase "Base test2" $ assertEqual "" (eval (Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3)) M.empty) (Right ( ((  ( ( sqrt 25 + 3)**2 - 4 * 7) * 2) / 5)**3))
  , let e1 = Mult (Const 4) (Const 5)
        e2 = Plus (Const 1) (Const (-1)) in
    testCase "Zero division test" $ assertEqual "" (eval (Div e1 e2) M.empty) ( Left (DivisionByZero e1 e2))
  , let e = Minus (Const 1) (Const 100) in
  
    testCase "RootOfNegative error" $ assertEqual "" (eval (SquareRoot e) M.empty) (Left $ RootOfNegative e)
  , testCase "Var test" $ assertEqual "" (eval (Var "x") $ M.fromList [("x", 100)]) (Right 100)

  
  , testCase "Undefined variable" $ assertEqual "" (eval (Plus (Var "z") (Var "y")) $ M.fromList [("x", 10), ("y", 20)]) ( Left $ UndefinedVariable "z")
  , testCase "Base test 3" $ assertEqual "" (eval (Minus (Var "x") (Var "x")) $ M.fromList [("x", 0)]) (Right 0)
  , testCase "Pow test 0^0" $ assertEqual "" (eval (Pow (Var "x") (Var "y")) $ M.fromList [("x", 0), ("y", 0)]) (Right 1)

  ]

simplifyTests :: TestTree
simplifyTests = testGroup "Simplify tests"
  [ testCase "Base simplify" $ assertEqual "" (simplify (Mult (Const 0) (Const 5))) (Const 0)
  , testCase "Simplify with variables" $ assertEqual "" (simplify (Plus (Mult (Const 1) (Var "x")) (Const 0))) (Var "x")
  , testCase "Variable elimination" $ assertEqual "variable can be eliminated" (simplify (1 + 0 * Var "x")) 1

  , testCase "Complex simplify" $ assertEqual "" (simplify (Plus (Const 0) (       
                Mult  (Const 1) (    
                      Minus (Div (Const 42) (Const 1) ) (Const 0) 
                )
                ))) 42
  , let e = Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3) in
    testCase "Don't perform wrong simplifications" $ assertEqual "" (simplify e) e
  ]

exprTests :: TestTree
exprTests = testGroup "Expr tests" [evalTests, simplifyTests]

main :: IO()
main = defaultMain exprTests