module Expr where 

import qualified Data.Map.Strict as M 

data Op = Add | Sub | Mul | Div | Pow deriving Eq

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

data Expr a
  = Num a
  | Var String
  | Sqrt (Expr a)
  | BinOp Op (Expr a) (Expr a) deriving Eq

instance Show a => Show (Expr a) where
  show (Num x) = show x
  show (Var s) = show s
  show (Sqrt e) = "(sqrt " ++ show e ++ ")"
  show (BinOp op e1 e2) = '(' : show e1 ++ (' ' : show op) ++ " " ++ show e2 ++ ")"

instance Fractional a => Num (Expr a) where
  (+) = BinOp Add
  (*) = BinOp Mul
  negate = BinOp Sub (Num 0.0)  
  abs = error "abs not implemented"
  signum = error "signum not implemented"
  fromInteger i = Num $ fromInteger i 
