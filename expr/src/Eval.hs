module Eval ( eval ) where 

import Expr ( Expr (..) )
import Error ( Error (..) )

import Data.Maybe (fromJust, isNothing)

andExprError :: (Expr a -> Expr a -> Expr a) -> Either Error (Expr a) -> Either Error (Expr a) -> Either Error (Expr a) -- priority for the errors, operands otherwise
andExprError _ (Left error1) (Left error2) = Left (ComplexError error1 error2)
andExprError _ (Right expr1) (Left error2) = Left error2
andExprError _ (Left error1) (Right expr2) = Left error1
andExprError oper (Right expr1) (Right expr2) = Right (oper expr1 expr2)


resolve :: (RealFloat a, Ord a, Show a) => Either Error (Expr a) -> [(String, a)] -> Either Error (Expr a)
resolve (Left error) _ = Left error

resolve (Right (Const n)) _ = Right (Const n)

resolve (Right (Root (Const n))) _
  | n < 0 = Left (NegativeFromRoot ("(root of " ++ show n ++ ")"))
  | otherwise = Right (Const (sqrt n))

resolve (Right (Plus (Const n1) (Const n2))) _ = Right $ Const (n1 + n2)
resolve (Right (Minus (Const n1) (Const n2))) _ = Right $ Const (n1 - n2)
resolve (Right (Mult (Const n1) (Const n2))) _ = Right $ Const (n1 * n2)
resolve (Right (Div (Const n1) (Const n2))) _
  | n2 == 0 = Left (DivideByZero ("(div of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Right $ Const (n1 / n2)

resolve (Right (Pow (Const n1) (Const n2))) _
  | isNaN (n1 ** n2)= Left (PowNaN ("(pow of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Right (Const (n1 ** n2))

resolve (Right (Var v)) vars
  | isNothing (lookup v vars) = Left (VariableNotApplied ("(variable " ++ v ++ ")"))
  | otherwise = Right $ Const (fromJust $ lookup v vars)

resolve (Right (Root expr)) vars = resolve (Root <$> resolve (Right expr) vars) vars -- propagate error and recursive Root if correct
resolve (Right (Plus expr1 expr2)) vars = resolve (andExprError Plus (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Minus expr1 expr2)) vars = resolve (andExprError Minus (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Mult expr1 expr2)) vars = resolve (andExprError Mult (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Div expr1 expr2)) vars = resolve (andExprError Div (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars
resolve (Right (Pow expr1 expr2)) vars = resolve (andExprError Pow (resolve (Right expr1) vars) (resolve (Right expr2) vars)) vars


exprToNum :: Expr a -> a
exprToNum (Const n) = n

eval :: (RealFloat a, Ord a, Show a) => Expr a -> [(String, a)] -> Either Error a
eval expr vars = exprToNum <$> resolve (Right expr) vars
