module Main (main) where
import Expr (Expr(..))
import Error (Error(..))
import Operations (Operation(..))
import Eval (eval)
import Control.Monad ( unless )
import Text.Printf (printf)
import Simplify (simplify)
import qualified Data.Map.Strict as M 

casesSimplify :: (RealFloat a, Ord a, Show a) => [(Expr a, Expr a)]
casesSimplify = [
  (Bin (Const 1.0) Plus (Bin (Const 0.0) Plus (Const 1.0)), Bin (Const 1.0) Plus (Const 1.0)),
  (Bin (Const 1.0) Minus (Bin (Const 0.0) Minus (Const 1.0)), Bin (Const 1.0) Minus (Const (-1.0))),
  (Bin (Const 1.0) Minus (Bin (Const 1.0) Minus (Const 0.0)), Bin (Const 1.0) Minus (Const 1.0)),
  (Sqrt (Const 1.0), Const 1.0),
  (Sqrt (Const 0.0), Const 0.0),
  (Bin (Const 3.0) Mult (Const 0.0), Const 0.0),
  (Bin (Const 0.0) Mult (Sqrt (Const 7.0)), Const 0.0),
  (Bin (Const 0) Div (Var "x"), Bin (Const 0) Div (Var "x")),
  (Bin (Bin (Const 1.0) Plus (Const 7.0)) Mult (Const 1), Bin (Const 1.0) Plus (Const 7.0)),
  (Bin (Var "x") Minus (Var "x"), Const 0.0),
  (Bin (Var "y") Div (Const 1.0), Var "y")
  ]


cases :: (Floating a, Ord a, Fractional a, Show a) => [(Expr a, M.Map String a ,Either Error a)]
cases = [
  (Sqrt $ Const 1.0, M.empty ,Right 1.0),
  (Bin (Sqrt (Const 9.0)) Plus (Bin (Const 2.0) Pow (Const 7.0)), M.empty, Right 131.0),
  (Sqrt (Const (-1.0)), M.empty ,Left $ NegativeRoot "-1.0"),
  (Bin (Bin (Const 7.0) Mult (Const 2.0)) Div (Bin (Const 4.0) Minus (Const 4.0)), M.empty, Left $ DivByZero "14.0"),
  (Bin (Const 7.0) Div (Const 2.0), M.empty, Right 3.5),
  (Bin (Bin (Const 7.0) Div (Sqrt (Const 4.0))) Minus (Bin (Sqrt (Sqrt (Sqrt  (Const 256.0)))) Mult (Const 2)), M.empty, Right (-0.5)),
  (Bin (Const 3.0) Plus (Var "x"), M.fromList [("x", 1.0)], Right 4.0),
  (Bin (Const 3.0) Plus (Var "x"), M.fromList [("y", 1.0)], Left $ UndefVar "x")

  ]

test :: (Floating a, Ord a, Fractional a, Show a) => Expr a -> M.Map String a -> Either Error a -> IO () 
test expr env expected = 
    let actual = eval expr env in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 
  
testSimplify :: (Show a, Eq a,  Fractional a, Ord a, Floating a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
  let actual = simplify expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (\(expr, env ,expected) -> test expr env expected) cases
  mapM_ (uncurry testSimplify) casesSimplify
