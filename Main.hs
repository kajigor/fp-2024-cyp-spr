module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Semigroup (Min(Min))

data Expr = Const Double 
            | SquareRoot Expr
            | Plus Expr Expr
            | Minus Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Pow Expr Expr 

instance Show Expr where 
  show e = case e of
          (Const c)       -> show c
          (SquareRoot e)  -> printf "sqrt(%s)" (show e)
          (Plus e1 e2)    -> putBrackets $ show e1 ++ "+" ++ show e2
          (Minus e1 e2)   -> putBrackets $ show e1 ++ "-" ++ show e2
          (Mult e1 e2)    -> putBrackets $ show e1 ++ "*" ++ show e2
          (Div e1 e2)     -> putBrackets $ show e1 ++ "/" ++ show e2
          (Pow e1 e2)     -> putBrackets $ show e1 ++ "^" ++ show e2
          where putBrackets s = '(' : s ++ ")"

instance Eq Expr where 
  (==) (Const c1) (Const c2) = c1 == c2
  (==) (SquareRoot c1) (SquareRoot c2) = c1 == c2
  (==) (Plus e1 e2) (Plus e1' e2') = e1 == e1' && e2 == e2'
  (==) (Minus e1 e2) (Minus e1' e2') = e1 == e1' && e2 == e2'
  (==) (Mult e1 e2) (Mult e1' e2') = e1 == e1' && e2 == e2'
  (==) (Div e1 e2) (Div e1' e2') = e1 == e1' && e2 == e2'
  (==) (Pow e1 e2) (Pow e1' e2') = e1 == e1' && e2 == e2'
  (==) _ _ = False

data Error = DivisionByZero Expr Expr | RootOfNegative Expr

instance Show Error where 
  show (DivisionByZero e1 e2) = printf "Error when dividing %s by %s. %s was equal to zero" (show e1) (show e2) (show e2)
  show (RootOfNegative e)     = printf "Error when taking square root. Expression %s was negative" (show e)

instance Eq Error where 
  (==) (DivisionByZero _ _) (DivisionByZero _ _) = True 
  (==) (RootOfNegative _) (RootOfNegative _) = True 
  (==) _ _ = False


eval :: Expr -> Either Error Double 
eval e = case e of
  Const c      ->  Right c
  Plus e1 e2   -> perfOp (\ x y -> Right $ x+y ) e1 e2
  Minus e1 e2  -> perfOp (\ x y -> Right $ x-y ) e1 e2
  Mult e1 e2   -> perfOp (\ x y -> Right $ x*y ) e1 e2
  Div e1 e2    -> perfOp (\ x y -> if y /= 0 then Right (x/y) else Left (DivisionByZero e1 e2)) e1 e2
  Pow e1 e2   -> perfOp (\ x y -> Right $ x ** y ) e1 e2
  SquareRoot e1 -> perfOp (\_ v -> if v >=0 then Right (sqrt v) else Left (RootOfNegative e1)) e1 e1
  where perfOp op e1 e2 = case (eval e1, eval e2) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er

x = Pow (Div (Mult (Minus (Pow (SquareRoot (Const 25)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3)



cases :: [(Expr, Either Error Double)]
cases = [ 
        (Const 777, Right 777),
        (Pow (Div (Mult (Minus (Pow (Plus (SquareRoot(Const 25)) (Const 3)) (Const 2)) (Mult (Const 4) (Const 7))) (Const 2)) (Const 5)) (Const 3), Right ( ((  ( ( sqrt 25 + 3)**2 - 4 * 7) * 2) / 5)**3)),  
        let e1 = Mult (Const 4) (Const 5)
            e2 = Plus (Const 1) (Const (-1)) in
            (Div e1 e2, Left (DivisionByZero e1 e2)),
        let e = Minus (Const 1) (Const 100) in
        (SquareRoot e, Left (RootOfNegative e))
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
  