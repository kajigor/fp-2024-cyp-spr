import Test.Tasty (defaultMain, testGroup, TestTree)
import Expr
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure)
import Data.Map.Strict (fromList)

testEval :: TestTree
testEval = 
    testGroup "Eval" [testBinOp, testSquareRoot, testVariables, testMixed, testError] where
        testBinOp = testGroup "testBinOp"
            [
                testCase "30 + 20 == 50" $ eval (BinExpr Plus (Const 30) (Const 20)) (fromList []) @?= Right 50
                , testCase "100 - 115.5 == -15.5" $ eval (BinExpr Minus (Const 100) (Const 115.5)) (fromList []) @?= Right (-15.5)
                , testCase "50 * 7.5 == 375" $ eval (BinExpr Multiply (Const 50) (Const 7.5)) (fromList []) @?=  Right 375
                , testCase "2 ^ 4 == 16" $ eval (BinExpr Power (Const 2) (Const 4)) (fromList []) @?=  Right 16
                , testCase "21 / 2 == 16" $ eval (BinExpr Divide (Const 21) (Const 2)) (fromList []) @?=  Right 10.5
            ]

        testSquareRoot = testGroup "testSquareRoot" 
            [
                testCase "sqrt(4) == 2" $ eval (SquareRoot (Const 4)) (fromList []) @?= Right 2
            ]

        testVariables = testGroup "testVariables: [x = 1, y = 2, z = 3]"
            [
                testCase "x == 1" $ eval (Var "x") variables @?= Right 1
                , testCase "y + 20 == 22" $ eval (BinExpr Plus (Var "y") (Const 20)) variables @?= Right 22
                , testCase "(z + y) ^ (z - x) == 25" $ eval (BinExpr Power (BinExpr Plus (Var "z") (Var "y")) (BinExpr Minus (Var "z") (Var "x"))) variables @?= Right 25
            ] where
                variables = fromList [("x", 1), ("y", 2), ("z", 3)]
        
        testMixed = testGroup "testMixed"
            [
                testCase "(12 - 4) * sqrt(20 ^ 2) == 160" $ eval (BinExpr Multiply (BinExpr Minus (Const 12) (Const 4)) (SquareRoot (BinExpr Power (Const 20) (Const 2)))) (fromList []) @?= Right 160
                , 
                testCase "(12 * 4) + (2 ^ 5) == 80" $ eval (BinExpr Plus (BinExpr Multiply (Const 12) (Const 4)) (BinExpr Power (Const 2) (Const 5))) (fromList []) @?= Right 80
            ]

        testError = testGroup "testError"
            [
                testCase "37 / 0 == division by zero error" $ eval (BinExpr Divide (Const 37) (Const 0)) (fromList []) @?= Left DivisionByZero
                , testCase "sqrt(-24) == square root of negative error" $ eval (SquareRoot (Const (-24))) (fromList []) @?= Left SquareRootOfNegative
                , testCase "150 * 50 / (0 ^ 2) == division by zero error" $ eval (BinExpr Divide (BinExpr Multiply (Const 150) (Const 50)) (BinExpr Power (Const 0) (Const 2))) (fromList []) @?= Left DivisionByZero
                , testCase "sqrt(-345) + 100 ^ 4 == square root of negative error" $ eval (BinExpr Plus (SquareRoot (Const (-345))) (BinExpr Power (Const 100) (Const 4))) (fromList []) @?= Left SquareRootOfNegative
                , testCase "(1 / 0) * sqrt(a) == division by zero error" $ eval (BinExpr Multiply (BinExpr Divide (Const 1) (Const 0)) (SquareRoot (Var "a"))) (fromList []) @?= Left DivisionByZero
                , testCase "b == no such variable error" $ eval (Var "b") (fromList []) @?= Left NoSuchVariable
            ]
        
testSimplify :: TestTree
testSimplify = 
    testGroup "Simplify" [testPlus, testMinus, testMultiply, testDivide, testPower, testSquareRoot, testMixed, testNoSimplify] where
        testPlus = testGroup "testPlus"
            [
                testCase "0 + 5 => 5" $ simplify (BinExpr Plus (Const 0) (Const 5)) @?= Const 5
                , 
                testCase "5 + 0 => 5" $ simplify (BinExpr Plus (Const 5) (Const 0)) @?= Const 5
            ]
        testMinus = testGroup "testMinus"
            [
                testCase "x - 0 => x" $ simplify (BinExpr Minus (Var "x") (Const 0)) @?= Var "x"
                , testCase "0 - x => 0 - x" $ simplify (BinExpr Minus (Const 0) (Var "x")) @?= BinExpr Minus (Const 0) (Var "x")
            ]
        testMultiply = testGroup "testMultiply"
            [
                testCase "x * 0 => 0" $ simplify (BinExpr Multiply (Var "x") (Const 0)) @?= Const 0
                , testCase "0 * x => 0" $ simplify (BinExpr Multiply (Const 0) (Var "x")) @?= Const 0
                , testCase "1 * x => x" $ simplify (BinExpr Multiply (Const 1) (Var "x")) @?= Var "x"
                , testCase "x * 1 => x" $ simplify (BinExpr Multiply (Var "x") (Const 1)) @?= Var "x"
            ]
        testDivide = testGroup "testDivide"
            [
                testCase "x / 1 => x" $ simplify (BinExpr Divide (Var "x") (Const 1)) @?= Var "x"
            ]
        testPower = testGroup "testPower"
            [
                testCase " 5 ^ 1 => 5" $ simplify (BinExpr Power  (Const 5) (Const 1)) @?= Const 5
                , testCase " 1 ^ 5 => 1" $ simplify (BinExpr Power  (Const 1) (Const 5)) @?= Const 1
                , testCase " x ^ 0 => 1" $ simplify (BinExpr Power  (Var "x") (Const 0)) @?= Const 1
                , testCase " 0 ^ x => 0" $ simplify (BinExpr Power (Const 0) (Var "x")) @?= Const 0
            ]
        testSquareRoot = testGroup "testSquareRoot"
            [
                testCase "sqrt(0) => 0" $ simplify (SquareRoot (Const 0)) @?= Const 0
                , testCase "sqrt(0) => 0" $ simplify (SquareRoot (Const 1)) @?= Const 1
            ]
        testMixed = testGroup "testMixed"
            [
                testCase "1 * (0 + x) => x" $ simplify (BinExpr Multiply (Const 1) (BinExpr Plus (Const 0) (Var "x"))) @?= Var "x"
                , testCase "1 * (x - sqrt(0)) => x" $ simplify (BinExpr Multiply (Const 1) (BinExpr Minus (Var "x") (SquareRoot (Const 0)))) @?= Var "x"
                , testCase "x ^ 1 - 2 / 1 => x - 2" $ simplify (BinExpr Minus (BinExpr Power (Var "x") (Const 1)) (BinExpr Divide (Const 2) (Const 1))) @?= BinExpr Minus (Var "x") (Const 2)
                , testCase "0 * 1 * (1 * 0) => 0" $ simplify (BinExpr Multiply (BinExpr Multiply (Const 0) (Const 1)) (BinExpr Multiply (Const 1) (Const 0))) @?= Const 0
            ]
        testNoSimplify = testGroup "testNoSimplify"
            [
                testCase "12 * 24 / 3 => 12 * 24 / 3" $ simplify (BinExpr Multiply (Const 12) (BinExpr Divide (Const 24) (Const 3))) @?= BinExpr Multiply (Const 12) (BinExpr Divide (Const 24) (Const 3))
                , testCase "1 / 12 ^ 3 => 1 / 12 ^ 3" $ simplify (BinExpr Divide (Const 1) (BinExpr Power (Const 12) (Const 3))) @?= (BinExpr Divide (Const 1) (BinExpr Power (Const 12) (Const 3)))
            ]

main :: IO ()
main = 
    defaultMain $ testGroup "Expression tests" [testEval, testSimplify]
