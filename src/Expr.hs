module Expr (Expr (..), BinOperator (..)) where

import Text.Printf (printf)
import qualified Data.Map.Strict as M


data BinOperator = Plus | Minus | Multiply | Divide | Power deriving (Eq)
instance Show BinOperator where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Divide = "/"
  show Power = "^"

data Expr a = Const a | Var String |  SquareRoot (Expr a) | BinExpr BinOperator (Expr a) (Expr a) deriving Eq 

instance (Show a) => Show (Expr a) where 
  show (Const n) = show n 
  show (Var s) = show s
  show (SquareRoot exp) = printf "(square root of %s)"  (show exp)
  show (BinExpr binOp leftExp rightExp) = printf "(%s %s %s)" (show leftExp) (show binOp) (show rightExp)

instance (Num a) => Num (Expr a) where
  (+) = BinExpr Plus
  (*) = BinExpr Multiply
  negate = BinExpr Multiply (Const (-1))
  fromInteger = Const . fromInteger

