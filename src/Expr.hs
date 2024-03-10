{-# LANGUAGE InstanceSigs #-}
module Expr (Expr(..)) where
  
import Operations (Operation(..))
import Text.Printf ( printf )

data Expr a = 
  Var String
  | Const a
  | Bin (Expr a) Operation (Expr a)
  | Sqrt (Expr a)
  -- deriving Eq

instance (Eq a, Floating a, Ord a, Show a, Fractional a) => Eq (Expr a) where 
  (==) :: Expr a -> Expr a -> Bool
  Const a == Const b = a == b
  Var a == Var b = a == b
  Sqrt exp1 == Sqrt exp2 = exp1 == exp2
  Bin a1 op1 b1 == Bin a2 op2 b2 = op1 == op2 && a1 == a2 && b1 == b2
  _ == _ = False

instance (Show a) => Show (Expr a) where 
  show :: Expr a -> String
  show (Const a) = show a
  show (Sqrt exp1) = printf " sqrt( %s )" (show exp1)
  show (Bin exp1 oper exp2) = printf "(%s)" (show exp1 ++ show oper ++ show exp2)
  show (Var name) = name


instance (Fractional a, Num a) => Num (Expr a) where
  (+) exp = Bin exp Plus
  (*) exp = Bin exp Mult
  (-) exp = Bin exp Minus
  fromInteger i = Const (fromInteger i)
  negate = Bin (Const 0) Minus