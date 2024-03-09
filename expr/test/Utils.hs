module Utils(testEvalNoArgs, testEvalCustomArgs, testSimplify) where
import Test.Tasty (TestName, TestTree)
import Lib (Expr, Error, eval, simplify)
import Data.Map.Strict as Map
import Test.Tasty.HUnit (testCase, (@?=))


testEvalCustomArgs :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Map String b -> Either Error b -> TestTree
testEvalCustomArgs msg expr dict expected =
     testCase msg $ eval expr dict @?= expected

testEvalNoArgs :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Either Error b -> TestTree
testEvalNoArgs msg expr = testEvalCustomArgs msg expr Map.empty


testSimplify :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Expr b -> TestTree
testSimplify msg from expected = 
     testCase msg $ simplify from @?= expected