import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )
import Expr ( Expr (..), Op (..) )
import Eval ( eval )
import Simplify ( simplify )
import Error
import qualified Data.Map.Strict as M
import Main (testSimplified)

testEval :: TestTree
testEval =
    testGroup "Eval" [ testCorrect, testFailed, testPow, testSimplified ]
  where
    testEvalNoVarSuccess msg expr res =
       testCase msg $ eval expr M.empty @?= Right res
    testEvalWithVarSuccess msg expr res =
       testCase msg $ eval expr testMap @?= Right res
    testCaseShortNoVarSuccess expr res = testEvalNoVarSuccess (show expr ++ "==" ++ show res) expr res
    testCaseShortWithVarSuccess expr res = testEvalWithVarSuccess (show expr ++ "==" ++ show res) expr res
    testCorrect = testGroup "Correct expressions"
      [ testCaseShortNoVarSuccess (1 + 2) 3
      , testCaseShortNoVarSuccess (2 + 3) 5
      , testCaseShortNoVarSuccess (0 + 2) 2
      , testCaseShortNoVarSuccess (Num 1.0) 1.0
      , testCaseShortNoVarSuccess (Sqrt 4) 2
      , testCaseShortWithVarSuccess ((4-1)+((Var "two") * 3))  9
      , testCaseShortNoVarSuccess (BinOp Div (Sqrt 100) (BinOp Div 10 (6-1))) 5.0
      , testCaseShortWithVarSuccess 
        (BinOp Pow
            (((Var "four") - 1) + (2 * 3))
            (BinOp Div 10 (3 - 1)))
        (9 ^ 5)
      ]
    testEvalNoVarFailed expr err =
       testCase ("Must throw: `" ++ show err ++ "`") $ eval expr M.empty @?= Left err
    testEvalWithVarFailed expr err =
       testCase ("Must throw: `" ++ show err ++ "`") $ eval expr testMap @?= Left err
    testFailed = testGroup "Incorrect expressions"
      [ testEvalNoVarFailed (Sqrt (Num (-1.0))) SquareRootOfNegative
      , testEvalNoVarFailed (BinOp Div (Sqrt (-1)) (BinOp Div (Var "four") ((Var "x") - (Var "x")))) SquareRootOfNegative
      , testEvalWithVarFailed (BinOp Div (BinOp Div (Var "four") ((Var "x") - (Var "x"))) (Sqrt (-1))) DivideByZero
      , testEvalWithVarFailed 
          (BinOp Pow
            (((Var "abracadabra") - 1) + (2 * 3))
            (BinOp Div 10 ((Num 3) - (Num 1))))
          IncorrectVariableName
      ]
    testPow = testGroup "Pow"
      [ -- testEvalNoVarSuccess "(-4) ** 0.5 == NaN" (BinOp Pow (Num (-4)) (Num (0.5))) ((-4) ** 0.5) -- always fail due to NaN /= NaN
        testNaN "(-4) ** 0.5 == NaN" (BinOp Pow (Num (-4)) (Num (0.5)))
      ]
    testNaN msg expr = testCase msg $
      case eval expr M.empty of
        Right x -> assertBool "Should be NaN" (isNaN x)
        Left x -> assertFailure $ "Evaluation produced an error " ++ show x
    testSimplifiedSuccess expr res =
       testCase ("simplify (" ++ show expr ++ ") == " ++ show res) $ simplify expr @?= res
    testSimplified = testGroup "Simplify"
      [ testSimplifiedSuccess (Num 1.0) (Num 1.0)
      , testSimplifiedSuccess (Sqrt 4) 2
      , testSimplifiedSuccess ((4 - 1) + ((Var "two") * (Num 3))) 9
      , testSimplifiedSuccess (Var "x") (Var "x")
      , testSimplifiedSuccess (1 + 2 + 3) 6
      , testSimplifiedSuccess (5 * 2 - 3) 7
      , testSimplifiedSuccess (5 * 2 - (Sqrt (Num 9.0))) (Num 7.0)
      , testSimplifiedSuccess ((Var "x") +  0) (Var "x")
      , testSimplifiedSuccess (0 + (Var "x")) (Var "x")
      , testSimplifiedSuccess ((Var "x") - 0) (Var "x")
      , testSimplifiedSuccess ((Var "x") * 0) 0
      , testSimplifiedSuccess (0 * (Var "x")) 0
      , testSimplifiedSuccess (1 * (Var "x")) (Var "x")
      , testSimplifiedSuccess ((Var "x") * 1) (Var "x")
      , testSimplifiedSuccess (BinOp Div 0 (Var "x")) 0
      , testSimplifiedSuccess (BinOp Pow 0 (Var "x")) 0  
      , testSimplifiedSuccess (BinOp Pow (Var "x") 1) (Var "x")
      ]


main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [ testEval ]

testMap :: Fractional a => M.Map String a
testMap = M.fromList
  [ ("x", 1.0),
    ("y", 3.0),
    ("z", -6.0),
    ("two", 2.0),
    ("four", 4.0)
  ]
