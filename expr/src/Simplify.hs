module Simplify ( simplify ) where 

import Expr ( Expr (..), Op (..) )
import Eval ( func )

simplify :: (Floating a, Eq a) => Expr a -> Expr a
simplify (Sqrt x) = oneSimplify (Sqrt (simplify x))
simplify (BinOp op l r) = oneSimplify (BinOp op (simplify l) (simplify r))
simplify x = x

oneSimplify :: (Floating a, Eq a) => Expr a -> Expr a
oneSimplify (Sqrt (Num a)) = Num (sqrt a)
oneSimplify (BinOp op (Num a) (Num b)) =
  case calc of
    Right x -> Num x
    _ -> BinOp op (Num a) (Num b)
  where calc = func op a b
oneSimplify (BinOp Add (Num 0.0) b) = b
oneSimplify (BinOp Add a (Num 0.0)) = a
oneSimplify (BinOp Sub a (Num 0.0)) = a
oneSimplify (BinOp Mul (Num 1.0) b) = b
oneSimplify (BinOp Mul a (Num 1.0)) = a
oneSimplify (BinOp Mul (Num 0.0) b) = Num 0.0
oneSimplify (BinOp Mul a (Num 0.0)) = Num 0.0
oneSimplify (BinOp Div (Num 0.0) b) = Num 0.0
oneSimplify (BinOp Pow (Num 0.0) b) = Num 0.0
oneSimplify (BinOp Pow a (Num 1.0)) = a
oneSimplify expr = expr
