module Simplify (simplify) where
import Expr (Expr(..))
import Operations (Operation(..))

simplify :: (Eq a, Fractional a, Ord a, Floating a, Show a) => Expr a -> Expr a
simplify expr =
  case expr of
    Const a -> Const a
    Var x -> Var x
    -- Plus - *
    Bin exp Plus (Const 0.0) -> simplify exp
    Bin (Const 0.0) Plus exp -> simplify exp
    Bin exp1 Plus exp2 -> Bin (simplify exp1) Plus (simplify exp2)
    -- Minus - *
    
    Bin exp Minus (Const 0.0) -> simplify exp
    Bin (Const 0.0) Minus (Const a) -> Const ((-1) * a)
    -- Bin exp Minus (Const a) -> if a < 0 then Bin (simplify exp) Plus (Const ((-1) * a)) else Bin (simplify exp) Minus (Const a)
    Bin exp1 Minus exp2 -> if exp1 == exp2 then Const 0 else Bin (simplify exp1) Minus (simplify exp2)
    -- Mult - *
    Bin (Const 1.0) Mult exp -> simplify exp
    Bin exp Mult (Const 1.0) -> simplify exp
    Bin exp Mult (Const 0.0) -> Const 0.0
    Bin (Const 0.0) Mult exp -> Const 0.0
    Bin exp1 Mult exp2 -> Bin (simplify exp1) Mult (simplify exp2)
    -- Div - *
    Bin exp Div (Const 1.0) -> simplify exp
    Bin (Const 0.0) Div (Const a) -> Const 0.0
    Bin exp1 Div exp2 -> Bin (simplify exp1) Div (simplify exp2)
    -- Sqrt - *
    Sqrt (Const 0.0) -> Const 0.0
    Sqrt (Const 1.0) -> Const 1.0
    Sqrt exp -> Sqrt $ simplify exp

    Bin exp1 Pow exp2 -> Bin (simplify exp1) Pow (simplify exp2)