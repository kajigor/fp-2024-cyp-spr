module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data Expr = Number Double | Sqrt Expr | InfNoFail String (Double -> Double -> Double) Expr Expr | Div Expr Expr | Exp Expr Expr

instance Show Expr where 
  show (Number x) = show x
  show (Sqrt exp) = "sqrt(" ++ show exp ++ ")"
  show (InfNoFail name _ exp1 exp2) = "(" ++ show exp1 ++ name ++ show exp2 ++ ")"
  show (Div exp1 exp2) = "(" ++ show exp1 ++ "/" ++ show exp2 ++ ")"
  show (Exp exp1 exp2) = "(" ++ show exp1 ++ "^" ++ show exp2 ++ ")"

instance Eq Expr where 
  (==) (Number a) (Number b) = a == b
  (==) (Sqrt exp1) (Sqrt exp2) = exp1 == exp2
  (==) (InfNoFail opName1 _ exp1l exp1r) (InfNoFail opName2 _ exp2l exp2r) = (opName1, exp1l, exp1r) == (opName2, exp2l, exp2r)
  (==) (Div exp1l exp1r) (Div exp2l exp2r) = (exp1l, exp1r) == (exp2l, exp2r)
  (==) (Exp e1l e1r) (Exp e2l e2r) = (e1l, e1r) == (e2l, e2r)
  (==) _ _ = False

data Error = DivByZero | NegExponent | NegSqrt

instance Show Error where 
  show DivByZero = "Division by zero"
  show NegExponent = "Trying to use negative exponent"
  show NegSqrt = "Trying to take a square root of a negative number"

-- deriving instance Eq => Eq (Error)   =(

instance Eq Error where 
  (==) DivByZero DivByZero = True
  (==) NegExponent NegExponent = True
  (==) NegSqrt NegSqrt = True
  (==) _ _ = False

eval :: Expr -> Either Error Double 
eval (Number x) = Right x
eval (Sqrt exp1) = if (res1 < 0.0) then Left NegExponent else Right sqrt res1
  where res1 = eval exp1
eval (InfNoFail _ f exp1 exp2) = Right f (eval exp1) (eval exp2)
eval (Div exp1 exp2) = if (res2 == 0.0) then Left DivByZero else Right res1 / res2
  where res1 = eval exp1
        res2 = eval exp2
eval (Exp exp1 exp2) = if (res2 < 0) then Left NegExponent else Right (res1 ^ res2)
  where res1 = eval exp1
        res2 = eval exp2

cases :: [(Expr, Either Error Double)]
cases = [(Number 1.0, Right 1.0)]

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  
