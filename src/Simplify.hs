module Simplify (simplify) where
import Expr
import Error

simplify :: (Num a, Ord a) => Expr a -> Expr a
simplify (SquareRoot x) = simplifySquare (SquareRoot (simplify x)) where
    simplifySquare (SquareRoot (Const 0)) = (Const 0)
    simplifySquare (SquareRoot (Const 1)) = (Const 1)
    simplifySquare exp = exp
simplify (BinExpr binOp left right) = simplifyBinExp (BinExpr binOp (simplify left) (simplify right)) where
  simplifyBinExp (BinExpr Plus (Const 0) x) = simplify x
  simplifyBinExp (BinExpr Plus x (Const 0)) = simplify x
  simplifyBinExp (BinExpr Minus x (Const 0)) = simplify x
  simplifyBinExp (BinExpr Multiply (Const 1) x) = simplify x
  simplifyBinExp (BinExpr Multiply x (Const 1)) = simplify x
  simplifyBinExp (BinExpr Multiply (Const 0) _) = Const 0
  simplifyBinExp (BinExpr Multiply _ (Const 0)) = Const 0
  simplifyBinExp (BinExpr Divide x (Const 1)) = simplify x
  simplifyBinExp (BinExpr Power (Const 1) _) = (Const 1)
  simplifyBinExp (BinExpr Power (Const 0) _) = (Const 0)
  simplifyBinExp (BinExpr Power x (Const 1)) = simplify x
  simplifyBinExp (BinExpr Power x (Const 0)) = (Const 1)
  simplifyBinExp exp = exp
simplify x = x
