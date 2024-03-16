module Simplify where

import Expr


simplify :: (Floating a, Ord a) => Expr a -> Expr a

simplify e = if trySimplifyResult /= e then simplify trySimplifyResult else trySimplifyResult where
  trySimplifyResult = trySimplify e

  trySimplify (Plus (Const 0) expr) = trySimplify expr
  trySimplify (Plus expr (Const 0)) = trySimplify expr 

  trySimplify (Mult (Const 0) _) = Const 0
  trySimplify (Mult _ (Const 0)) = Const 0

  trySimplify (Mult (Const 1) expr) = trySimplify expr  
  trySimplify (Mult expr (Const 1)) = trySimplify expr

  trySimplify (Div l (Const 1)) = trySimplify l

  trySimplify (Minus l (Const 0)) = trySimplify l

  trySimplify (Mult expr1 expr2) = Mult (trySimplify expr1) (trySimplify expr2)
  trySimplify (Plus expr1 expr2) = Plus (trySimplify expr1) (trySimplify expr2) 
  trySimplify (Minus expr1 expr2) = if expr1 == expr2 then Const 0 else Minus (trySimplify expr1) (trySimplify expr2)

  trySimplify expr = expr