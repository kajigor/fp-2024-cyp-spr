module Utils(testEvalNoArgs, testEvalCustomArgs) where
import Test.Tasty (TestName, TestTree)
import Expr (Expr, Error, eval, simplify)
import StateDemo (execState)
import Data.Map.Strict as Map
import Test.Tasty.HUnit (testCase, (@?=))



testEvalCustomArgs :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Map String b -> Either Error b -> TestTree
testEvalCustomArgs msg expr dict expected =
     testCase msg $ execState (eval expr) dict @?= expected

testEvalNoArgs :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Either Error b -> TestTree
testEvalNoArgs msg expr = testEvalCustomArgs msg expr Map.empty