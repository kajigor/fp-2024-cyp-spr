import EvalTests
import SimplifyTests
import Test.Tasty

exprTests :: TestTree
exprTests = testGroup "Expr tests" [evalTests, simplifyTests]

main :: IO()
main = defaultMain exprTests
