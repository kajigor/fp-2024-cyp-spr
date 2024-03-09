{-# OPTIONS_GHC -Wno-type-defaults #-}
module EvalTests (evalTests) where
import Test.Tasty
import Data.Map.Strict as Map
import Utils (testEvalNoArgs, testEvalCustomArgs)
import Lib (Expr(..), Error (..))


defaultArgs :: (Floating a) => Map String a
defaultArgs = Map.fromList [("x", 1.0), ("y", 2.0), ("z", 3.0), ("twice", 2.0), ("twice", 3.0)]

evalConstTests :: TestTree
evalConstTests = testGroup "Const" [
    testEvalNoArgs "1 == 1" (Const 1.0) (Right 1.0)
    ] 

evalVarTests :: TestTree
evalVarTests = testGroup "Var" [
    testEvalCustomArgs "x == 1 (x = 1)" (Var "x") defaultArgs (Right 1.0),
    testEvalNoArgs "Undefined variable is error" (Var "undefined") (Left VariableIsUndefined)
    ]

evalAddTests :: TestTree
evalAddTests = testGroup "Add" [
    testEvalNoArgs "1 + 1 == 2" (Add (Const 1.0) (Const 1.0)) (Right 2.0),
    testEvalCustomArgs "1 + x == 2 (x = 1)" (Add (Const 1) (Var "x")) defaultArgs (Right 2.0) 
    ]

evalSubtractTests :: TestTree
evalSubtractTests = testGroup "Subtract" [
    testEvalNoArgs "3 - 1 == 2" (Subtract (Const 3.0) (Const 1.0)) (Right 2.0),
    testEvalCustomArgs "2 - x == 1" (Subtract (Const 2) (Var "x")) defaultArgs (Right 1.0),
    testEvalCustomArgs "x - 2 == -1" (Subtract (Var "x") (Const 2)) defaultArgs (Right (-1.0)),
    testEvalCustomArgs "Defined twice (x - x == 0)" (Subtract (Var "twice") (Var "twice")) defaultArgs (Right 0.0)
    ]

evalMultiplyTests :: TestTree
evalMultiplyTests = testGroup "Multiply" [
    testEvalNoArgs "2 * 2 == 4" (Multiply (Const 2.0) (Const 2.0)) (Right 4.0),
    testEvalCustomArgs "z * y == 6 (z = 3, y = 2)" (Multiply (Var "y") (Var "z")) defaultArgs (Right 6.0)
    ]

evalDivideTests :: TestTree
evalDivideTests = testGroup "Divide" [
    testEvalNoArgs "5 / 2 == 2.5" (Divide (Const 5.0) (Const 2.0)) (Right 2.5),
    testEvalCustomArgs "x / x == 1 (x = 1)" (Divide (Var "x") (Var "x")) defaultArgs (Right 1.0),
    testEvalNoArgs "Division by zero is error (5 / 0 = undefined)" (Divide (Const 5.0) (Const 0.0)) (Left DivisorIsZero)
    ] 

evalSquareTests :: TestTree
evalSquareTests = testGroup "Square" [
    testEvalNoArgs "sqrt(4) == 2" (Square (Const 4.0)) (Right 2.0),
    testEvalCustomArgs "sqrt(x) == 1 (x = 1)" (Square (Var "x")) defaultArgs (Right 1.0),
    testEvalNoArgs "Square from negative is error (sqrt(-1) == undefined)" (Square (Const (-1.0))) (Left SquareRootIsNegative)
    ]

evalPowerTests :: TestTree
evalPowerTests = testGroup "Power" [
    testEvalNoArgs "3 ^ 3 == 27" (Power (Const 3.0) (Const 3.0)) (Right 27.0),
    testEvalCustomArgs "y ^ 2 == 4 (y = 2)" (Power (Var "y") (Const 2.0)) defaultArgs (Right 4.0),
    testEvalNoArgs "Power with negative base is error (-2 ^ 2 == undefined)" (Power (Const (-2.0)) (Const 2.0)) (Left PowerBaseIsNegative)
    ]

evalCompositeOperationsTests :: TestTree
evalCompositeOperationsTests = testGroup "Composite operations" [
    testEvalNoArgs "(5 * 3) + ((15 - 5) / 5) == 17" (Add (Multiply (Const 5) (Const 3)) (Divide (Subtract (Const 15) (Const 5)) (Const 5))) (Right 17.0),
    testEvalNoArgs "sqrt( sqrt( sqrt( 256 ) ) ) == 4" (Square (Square (Const 256.0))) (Right 4.0),
    testEvalNoArgs "2 ^ 2 ^ 2 == 16" (Power (Const 2) (Power (Const 2) (Const 2))) (Right 16)
    ]

evalTests :: TestTree
evalTests = testGroup "Eval tests" [
    evalConstTests,
    evalVarTests,
    evalAddTests,
    evalSubtractTests,
    evalMultiplyTests,
    evalDivideTests,
    evalSquareTests,
    evalPowerTests,
    evalCompositeOperationsTests
    ]