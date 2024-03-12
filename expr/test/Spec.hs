
import Expr
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import Control.Exception (assert)
{-
type ExprConstCaseType = Double

cases :: [(Expr ExprConstCaseType, [(String, ExprConstCaseType)] , Either (Error ExprConstCaseType) ExprConstCaseType)]
cases = [ 
        (Const 777, [],  Right 777),
        (Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3), [],  Right ( ((  ( ( sqrt 25 + 3)**2 - 4 * 7) * 2) / 5)**3)),  
        let e1 = Mult (Const 4) (Const 5)
            e2 = Plus (Const 1) (Const (-1)) in
            (Div e1 e2, [],  Left (DivisionByZero e1 e2)),
        let e = Minus (Const 1) (Const 100) in
        (SquareRoot e, [],  Left (RootOfNegative e)),
        (Var "x", [("x", 100)], Right 100),
        (Plus (Var "x") (Var "y"), [("x", 10.0), ("y", 20.0)], Right 30.0),
        (Plus (Var "z") (Var "y"), [("x", 10), ("y", 20)], Left $ UndefinedVariable "z"),
        (Minus (Var "x") (Var "x"), [("x", 777)], Right 0), 
        (Minus (Var "x") (Var "x"), [("x", 14)], Right 0), 
        (Pow (Var "x") (Var "y"), [("x", 2), ("y", 10)], Right 1024 ),
        (Pow (Var "x") (Var "y"), [("x", 2), ("y", 0)], Right 1 ),
        (Pow (Var "x") (Var "y"), [("x", 0), ("y", 0)], Right 1 ),
        (Plus (Var "x") (Var "y"), [("x", 10), ("x", 20), ("y", 20)], Left $ InvalidMapping [("x", 10), ("x", 20)]),
        (Plus (Var "x") (Var "y"), [("x", 10), ("x", 20), ("y", 20), ("y", 30)], Left $ InvalidMapping [("x", 10), ("x", 20), ("y", 20), ("y", 30)]),
        (Plus (Var "x") (Var "y"), [("x", 10), ("x", 20), ("y", 30)], Left $ InvalidMapping [("x", 10), ("x", 20)])
        ] 

test :: Expr ExprConstCaseType -> [(String, ExprConstCaseType)]  -> Either (Error ExprConstCaseType) ExprConstCaseType -> IO () 
test expr var_list expected = 
    let actual = eval expr var_list in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 
    
simplifyTests :: [(Expr ExprConstCaseType, Expr ExprConstCaseType)]
simplifyTests = [ (Mult (Const 0) (Const 5), Const 0),
          (Plus (Mult (Const 1) (Var "x")) (Const 0), Var "x"),
          (Plus (Const 0) (       
                Mult  (Const 1) (    
                      Minus (Div (Const 42) (Const 1) ) (Const 0) 
                )
                )
        , Const 42), 
        (Plus (Const 1) (Mult (Const 0) (Var "x") ), Const 1), 
        (Minus (Const 1) (Const 1), Const 0),
        (Minus (Var "x") (Var "x"), Const 0),
        let e = Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3) in
        (Minus e e, Const 0),
        (Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3), Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3)),
        (Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Var "x")) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3), Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Var "x")) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3))]


testSimplify :: Expr ExprConstCaseType -> Expr ExprConstCaseType-> IO () 
testSimplify exprToSimplify expectedExpr = 
    let actual = simplify exprToSimplify in 
    unless (expectedExpr == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "simplify (%s) should be %s but it was %s\n" (show exprToSimplify) (show expectedExpr) (show actual) 


-}


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