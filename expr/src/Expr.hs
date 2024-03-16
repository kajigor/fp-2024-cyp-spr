module Expr where
import Text.Printf (printf)
import qualified Data.Map.Strict as M

data Expr a = Const a 
        | SquareRoot (Expr a)
        | Plus (Expr a) (Expr a)
        | Minus (Expr a) (Expr a)
        | Mult (Expr a) (Expr a)
        | Div (Expr a) (Expr a)
        | Pow (Expr a) (Expr a) 
        | Var String
        deriving Eq

instance Num a => Num (Expr a) where
  (+) = Plus
  (*) = Mult
  negate = Minus (Const 0) 
  fromInteger = Const . fromInteger

instance Show a => Show (Expr a) where 
  show e = case e of
          (Const c)       -> show c
          (SquareRoot e1)  -> printf "sqrt(%s)" (show e1)
          (Plus e1 e2)    -> printf "(%s %s %s)" (show e1) "+" (show e2)
          (Minus e1 e2)   -> printf "(%s %s %s)" (show e1) "-" (show e2)
          (Mult e1 e2)    -> printf "(%s %s %s)" (show e1) "*" (show e2)
          (Div e1 e2)     -> printf "(%s %s %s)" (show e1) "/" (show e2)
          (Pow e1 e2)     -> printf "(%s %s %s)" (show e1) "^" (show e2)
          (Var v)         -> printf "%s" (show v)
