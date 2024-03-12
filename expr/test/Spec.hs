import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure )
import Expr ( Expr (..), Op (..) )
import Eval ( eval )
import Error
import qualified Data.Map.Strict as M

testEval :: TestTree
testEval =
    testGroup "Eval" [ testCorrect, testPow ]
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
    testFailed = testGroup "Incorrect expressions"
      [ testEvalNoVarFailed (Sqrt (Num (-1.0))) SquareRootOfNegative
      , testEvalNoVarFailed (BinOp Div (Sqrt (-1)) (BinOp Div (Var "four") ((Var "x") - (Var "x")))) SquareRootOfNegative
      , testEvalNoVarFailed (BinOp Div (BinOp Div (Var "four") ((Var "x") - (Var "x"))) (Sqrt (-1))) DivideByZero
      , testEvalNoVarFailed 
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


-- cases :: Fractional a => [(Expr a, Either Error a)]
-- cases = [
--   (Num 1.0, Right 1.0),
--   (Sqrt (Num (-1.0)), Left SquareRootOfNegative),
--   (Sqrt (Num 4), Right 2),
--   (BinOp
--     Add
--       (BinOp Sub (Num 4) (Num 1))
--       (BinOp Mul (Var "two") (Num 3)),
--     Right 9),
--   (BinOp
--     Div
--       (BinOp Div (Var "four") (BinOp Mul (Num 1) (Num 0)))
--       (Sqrt (Num (-1))),
--     Left DivideByZero),
--   (BinOp
--     Div
--       (Sqrt (Num (-1)))
--       (BinOp Div (Var "four") (BinOp Sub (Var "x") (Var "x"))),
--     Left SquareRootOfNegative),
--   (BinOp
--     Div
--     (Sqrt (Num 100))
--     (BinOp Div (Num 10) (BinOp Sub (Num 6) (Num 1))),
--     Right 5),
--   (BinOp
--     Pow
--       (BinOp
--         Add
--         (BinOp Sub (Var "four") (Num 1))
--         (BinOp Mul (Num 2) (Num 3)))
--       (BinOp
--         Div
--         (Num 10)
--         (BinOp Sub (Num 3) (Num 1))),
--     Right (9 ^ 5)),
--       (BinOp
--     Pow
--       (BinOp
--         Add
--         (BinOp Sub (Var "abracadabra") (Num 1))
--         (BinOp Mul (Num 2) (Num 3)))
--       (BinOp
--         Div
--         (Num 10)
--         (BinOp Sub (Num 3) (Num 1))),
--     Left IncorrectVariableName)
--   ]