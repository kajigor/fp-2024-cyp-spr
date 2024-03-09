{-# OPTIONS_GHC -Wno-type-defaults #-}
module SimplifyTests (simplifyTests) where
import Test.Tasty
import Utils (testSimplify)
import Lib (Expr(..))
simplifyAddTests :: TestTree
simplifyAddTests = testGroup "Add" [
    testSimplify "1 + 0 == 1" (Add (Const 1.0) (Const 0.0)) (Const 1.0),
    testSimplify "0 + 2 == 2" (Add (Const 0.0) (Const 2.0)) (Const 2.0),
    testSimplify "0 + 4 + 0 == 4" (Add (Add (Const 0.0) (Const 4.0)) (Const 0.0)) (Const 4.0),
    testSimplify "x + 0 == x" (Add (Var "x") (Const 0.0)) (Var "x"),
    testSimplify "0 + x == x" (Add (Const 0.0) (Var "x")) (Var "x"),
    testSimplify "x + y == x + y" (Add (Var "x") (Var "y")) (Add (Var "x") (Var "y"))
    ]

simplifySubtractTests :: TestTree
simplifySubtractTests = testGroup "Subtract" [
    testSimplify "3 - 0 == 3" (Subtract (Const 3.0) (Const 0.0)) (Const 3.0),
    testSimplify "3 - 3 == 0" (Subtract (Const 3.0) (Const 3.0)) (Const 0.0),
    testSimplify "x - x == 0" (Subtract (Var "x") (Var "x")) (Const 0.0),
    testSimplify "3 - 2 == 3 - 2" (Subtract (Const 3.0) (Const 2.0)) (Subtract (Const 3.0) (Const 2.0)),
    testSimplify "x - y == x" (Subtract (Var "x") (Var "y")) (Subtract (Var "x") (Var "y"))
    ]

simplifyMultiplyTests :: TestTree
simplifyMultiplyTests = testGroup "Multiply" [
    testSimplify "2 * 1 == 2" (Multiply (Const 2.0) (Const 1.0)) (Const 2.0),
    testSimplify "1 * 2 == 2" (Multiply (Const 1.0) (Const 2.0)) (Const 2.0),
    testSimplify "x * 1 == x" (Multiply (Var "x") (Const 1.0)) (Var "x"),
    testSimplify "2 * 0 == 0" (Multiply (Const 1.0) (Var "x")) (Var "x"),
    testSimplify "0 * 2 == 0" (Multiply (Const 0.0) (Const 2.0)) (Const 0.0),
    testSimplify "x * 0 == 0" (Multiply (Var "x") (Const 0.0)) (Const 0.0),
    testSimplify "0 * x == 0" (Multiply (Const 0.0) (Var "x")) (Const 0.0),
    testSimplify "(2 + 3) * 0 == (2 + 3) * 0" (Multiply (Add (Const 2.0) (Const 3.0)) (Const 0.0)) (Multiply (Add (Const 2.0) (Const 3.0)) (Const 0.0))
    ]

simplfiyDivideTests :: TestTree
simplfiyDivideTests = testGroup "Divide" [
    testSimplify "0 / 5 == 0" (Divide (Const 0.0) (Const 5.0)) (Const 0.0),
    testSimplify "0 / x == 0 / x" (Divide (Const 0.0) (Var "x")) (Divide (Const 0.0) (Var "x")),
    testSimplify "5 / 5 == 1" (Divide (Const 5.0) (Const 5.0)) (Const 1.0),
    testSimplify "5 / 1 == 5" (Divide (Const 5.0) (Const 1.0)) (Const 5.0),
    testSimplify "x / 1 == x" (Divide (Var "x") (Const 1.0)) (Var "x"),
    testSimplify "x / x == x / x" (Divide (Var "x") (Var "x")) (Divide (Var "x") (Var "x"))
    ]

simplifyPowerTests :: TestTree
simplifyPowerTests = testGroup "Power" [
    testSimplify "5 ^ 1 == 5" (Power (Const 5.0) (Const 1.0)) (Const 5.0),
    testSimplify "5 ^ 0 == 1" (Power (Const 5.0) (Const 0.0)) (Const 1.0),
    testSimplify "-3 ^ 1 == -3 ^ 1" (Power (Const (-3)) (Const 1.0)) (Power (Const (-3)) (Const 1.0)),
    testSimplify "-3 ^ 0 == -3 ^ 0" (Power (Const (-3)) (Const 0.0)) (Power (Const (-3)) (Const 0.0)),
    testSimplify "x ^ 1 == x ^ 1" (Power (Var "x") (Const 1.0)) (Power (Var "x") (Const 1.0)),
    testSimplify "x ^ 0 == x ^ 0" (Power (Var "x") (Const 0.0)) (Power (Var "x") (Const 0.0))
    ]


simplifyTests :: TestTree
simplifyTests = testGroup "Simplify tests" [
    simplifyAddTests,
    simplifySubtractTests,
    simplifyMultiplyTests,
    simplfiyDivideTests,
    simplifyPowerTests
    ]