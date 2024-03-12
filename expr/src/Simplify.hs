module Simplify ( simplify ) where 

import Expr ( Expr (..) )
import Error ( Error (..) )


simplify :: (RealFloat a, Ord a, Show a) => Expr a -> Expr a

simplify (Const x) = Const x
simplify (Var x) = Var x

simplify (Plus (Const 0) expr) = simplify expr
simplify (Plus expr (Const 0)) = simplify expr
simplify (Plus (Const a) (Const b)) = if a + b == 0 then Const 0 else Minus (simplify (Const a)) (simplify (Const b))


-- simplify (Minus (Const 0) expr) = simplify expr -- that's wrong
simplify (Minus expr (Const 0)) = simplify expr
simplify (Minus (Const a) (Const b)) = if a - b == 0 then Const 0 else Minus (simplify (Const a)) (simplify (Const b))


simplify (Mult (Const 0) expr) = Const 0
simplify (Mult expr (Const 0)) = Const 0

simplify (Mult (Const 1) expr) = simplify expr
simplify (Mult expr (Const 1)) = simplify expr

simplify (Div expr (Const 1)) = simplify expr
simplify (Div (Const 0) expr) = if simplify expr /= Const 0 then Const 0 else Div (Const 0) (Const 0)

simplify (Root (Const 1)) = Const 1
simplify (Root (Const 0)) = Const 0

simplify (Root expr) = Root $ simplify expr
simplify (Plus expr1 expr2) = Plus (simplify expr1) (simplify expr2) -- I wonder if there is a prettier way
simplify (Minus expr1 expr2) = Minus (simplify expr1) (simplify expr2)
simplify (Mult expr1 expr2) = Mult (simplify expr1) (simplify expr2)
simplify (Div expr1 expr2) = if (simple1 == simple2) && (simple1 /= Const 0) && (simple2 /= Const 0)
                             then Const 1 else Div simple1 simple2
                              where simple1 = simplify expr1
                                    simple2 = simplify expr2
simplify (Pow expr1 expr2) = Pow (simplify expr1) (simplify expr2)