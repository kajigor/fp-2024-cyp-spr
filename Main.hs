module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Semigroup (Min(Min))

data Expr = Const Double 
            | Sqrt Expr
            | Add Expr Expr
            | Sub Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Pow Expr Expr 
          deriving Eq

instance Show Expr where 
  show e = case e of
          (Const c)       -> show c
          (Sqrt e1)       -> "sqrt(" ++ show e1 ++ ")"
          (Add e1 e2)    -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
          (Sub e1 e2)   -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
          (Mult e1 e2)    -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
          (Div e1 e2)     -> "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
          (Pow e1 e2)     -> "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

data Error  = DivisionByZero | RootOfNegative
            deriving Eq

instance Show Error where 
  show DivisionByZero = "DivisionByZero"
  show RootOfNegative = "RootOfNegative"


eval :: Expr -> Either Error Double 
eval e = case e of
  Const c      -> Right c
  Add e1 e2   -> doOperation (\ x y -> Right $ x + y ) e1 e2
  Sub e1 e2  -> doOperation (\ x y -> Right $ x - y ) e1 e2
  Mult e1 e2   -> doOperation (\ x y -> Right $ x * y ) e1 e2
  Div e1 e2    -> doOperation (\ x y -> if y /= 0 then Right (x/y) else Left DivisionByZero) e1 e2
  Pow e1 e2    -> doOperation (\ x y -> Right $ x ** y ) e1 e2
  Sqrt e1 -> doOperation (\_ v -> if v >= 0 then Right (sqrt v) else Left RootOfNegative) e1 e1
  where doOperation op e1 e2 = case (eval e1, eval e2) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er

cases :: [(Expr, Either Error Double)]
cases = [ 
        (Const 1, Right 1),
        (Pow (Const 2) (Const (-2)), Right 0.25),
        (Pow (Const 5) (Const 2), Right 25),
        (Mult (Const 4) (Const 7), Right 28),
        (Div (Const 26) (Const 13), Right 2),
        (Div (Const 1) (Const 0), Left DivisionByZero),
        (Sqrt (Const (-1)), Left RootOfNegative)
        ] 

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  
