module Main(main) where
import Test.Tasty (defaultMain, testGroup, TestTree)
import Eval (eval)
import Expr (Expr(..))
import Operations (Operation(..))
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import qualified Data.Map.Strict as M 
import Error (Error(..))
import Simplify (simplify)

testEval :: TestTree
testEval = 
    testGroup "Eval" [baseDef, deepNested, varTests]
  where 
    {baseDef = testGroup "Base defenitions tests" [
      testCase "sqrt(1.0) == 1.0" $ eval (Sqrt (Const 1.0)) M.empty @?= Right 1.0
      , testCase "sqrt(9) + 2^7 == 131" $ eval (Bin (Sqrt (Const 9.0)) Plus (Bin (Const 2.0) Pow (Const 7.0))) M.empty @?= Right 131.0
      , testCase "sqrt(-1) == ERROR: NegRoot" $ eval (Sqrt (Const (-1.0))) M.empty @?= Left( NegativeRoot "-1.0")
      , testCase "(7 * 2) / (4 - 4) == DivZero" $ eval (Bin (Bin (Const 7.0) Mult (Const 2.0)) Div (Bin (Const 4.0) Minus (Const 4.0))) M.empty @?= Left (DivByZero "14.0")
      , testCase "7 / 2 == 3.5" $ eval (Bin (Const 7.0) Div (Const 2.0)) M.empty @?= Right 3.5
      ];
    deepNested = testGroup "Deep nested tests" [
      testCase "(7 / sqrt(4)) - ((sqrt(sqrt(sqrt(256)))) * 2)" $ eval (Bin (Bin (Const 7.0) Div (Sqrt (Const 4.0))) Minus (Bin (Sqrt (Sqrt (Sqrt  (Const 256.0)))) Mult (Const 2))) M.empty @?= Right (-0.5)
      , testCase "1 + 1 + 1 + 1 + 1 * 2 + 1 * 3 - 3 - 3 == 3" (
         eval (1 + 1 + 1 + 1 + 1 * 2 + 1 * 3 - 3 - 3) M.empty @?= Right 3)
    ];
    varTests = testGroup "Variables tests" [
      testCase "3 + x == 4, x = 1" $ eval (Bin (Const 3.0) Plus (Var "x")) (M.fromList [("x", 1.0)]) @?= Right 4.0
      , testCase "3 + x == UndefVar 'x', y = 1" $ eval (Bin (Const 3.0) Plus (Var "x")) (M.fromList [("y", 1.0)]) @?= Left( UndefVar "x")
    ]
    }
testSimplify :: TestTree
testSimplify = 
    testGroup "Simplify" [plusTests, minusTests, sqrtTests, multTests, divTests]
  where {
    plusTests = testGroup "Plus" [
        testCase "1 + 0 + 1 == 1 + 1" $ simplify (1 + 0 + 1) @?= 1 + 1 
      , testCase "1 + 1 + 2 == 1 + 1 + 2" $ simplify (1 + 1 + 2) @?= 1 + 1 + 2
    ];
    minusTests = testGroup "Minus" [
        testCase "1 - (1 - 0) == 0" $ simplify (1 - 1 - 0) @?= 0
      , testCase "1 - (2 - 0) == 1 - 2" $ simplify (1 - (2 - 0)) @?= 1 - 2
      , testCase "1 - (0 - 1)" $ simplify ((Bin (Const 1.0) Minus (Bin (Const 0.0) Minus (Const 1.0)))) @?= Bin (Const 1.0) Minus (Const (-1.0))
      , testCase "x - x == 0" $ simplify (Bin (Var "x") Minus (Var "x")) @?= Const 0
      ];
    sqrtTests = testGroup "Sqrt" [
        testCase "sqrt(1) == 1" $ simplify( Sqrt (Const 1.0)) @?= Const 1.0
      , testCase "sqrt(0) == 0" $ simplify(Sqrt (Const 0.0)) @?= Const 0.0
      , testCase "sqrt(2) == sqrt(2)" $ simplify(Sqrt (Const 2.0)) @?= Sqrt (Const 2.0)
    ];
    multTests = testGroup "Mult" [
        testCase "3 * 0 == 0" $ simplify (3 * 0) @?= 0
      , testCase "0 * sqrt(7) == 0" $ simplify (Bin (Const 0.0) Mult (Sqrt (Const 7.0))) @?= 0
      , testCase "z * 1 == z" $ simplify (Bin (Var "z") Mult (Const 1)) @?= Var "z"
      , testCase "(1 + 7) * 1 == 1 + 7" $ simplify ((1 + 7) * 1) @?= 1 + 7
    ];
    divTests = testGroup "Div" [
        testCase "0 / x == 0 / x" $ simplify (Bin (Const 0) Div (Var "x")) @?= Bin (Const 0) Div (Var "x")
      , testCase "y / 1 == y" $ simplify (Bin (Var "y") Div (Const 1.0)) @?= Var ("y")
      , testCase "(1 + 7) / 1 == 1 + 7" $ simplify (Bin (Bin (Const 1.0) Plus (Const 7.0)) Div (Const 1)) @?= 1 + 7
    ]
    }
  

main:: IO()
main = 
  defaultMain $ testGroup "Tests" [testEval, testSimplify]