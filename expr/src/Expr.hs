module Expr where 

data Expr a = Var String
              | Const a
              | Plus (Expr a) (Expr a)
              | Minus (Expr a) (Expr a)
              | Mult (Expr a) (Expr a)
              | Div (Expr a) (Expr a)
              | Pow (Expr a) (Expr a)
              | Root (Expr a)
              deriving (Eq)


instance (Show a) => Show (Expr a) where
  show (Var ex) = ex
  show (Const ex) = show ex
  show (Plus ex1 ex2) = "(" ++ show ex1 ++ " + " ++ show ex2 ++ ")"
  show (Minus ex1 ex2) = "(" ++ show ex1 ++ " - " ++ show ex2 ++ ")"
  show (Mult ex1 ex2) = "(" ++ show ex1 ++ " * " ++ show ex2 ++ ")"
  show (Div ex1 ex2) = "(" ++ show ex1 ++ " / " ++ show ex2 ++ ")"
  show (Pow ex1 ex2) = "(" ++ show ex1 ++ " ** " ++ show ex2 ++ ")"
  show (Root ex) = "(" ++ "root " ++ show ex ++ ")"

instance (Num a) => Num (Expr a) where
  (+) = Plus
  (*) = Mult
  negate = Minus (Const 0)
  abs expr = undefined -- is possible to define, but only with the evaluation
  signum expr = undefined -- same reason
  fromInteger = Const . fromInteger