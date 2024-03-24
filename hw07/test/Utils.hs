module Utils(testEvalNoArgs, testEvalCustomArgs, testParseExpression) where
import Test.Tasty (TestName, TestTree)
import Expr (Expr, Error, eval)
import StateDemo (execState)
import Data.Map.Strict as Map
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (Parser(runParser), parseExpression)



testEvalCustomArgs :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Map String b -> Either Error b -> TestTree
testEvalCustomArgs msg expr dict expected =
     testCase msg $ execState (eval expr) dict @?= expected

testEvalNoArgs :: (Show b, Ord b, Floating b) => TestName -> Expr b -> Either Error b -> TestTree
testEvalNoArgs msg expr = testEvalCustomArgs msg expr Map.empty


testParseExpression :: TestName -> String -> Either String (String, Expr Int) -> TestTree
testParseExpression msg input expected = testCase msg $ runParser parseExpression input @?= expected