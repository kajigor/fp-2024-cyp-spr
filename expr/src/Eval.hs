module Eval where

import Expr
import Error
import qualified Data.Map.Strict as M


eval :: (Floating a, Ord a) => Expr a -> M.Map String a -> Either (Error a) a 
eval e ls = case e of
                Const c      ->  Right c
                Plus e1 e2   -> perfOp (\ x y -> Right $ x+y ) e1 e2
                Minus e1 e2  -> perfOp (\ x y -> Right $ x-y ) e1 e2
                Mult e1 e2   -> perfOp (\ x y -> Right $ x*y ) e1 e2
                Div e1 e2    -> perfOp (\ x y -> if y /= 0 then Right (x/y) else Left (DivisionByZero e1 e2)) e1 e2
                Pow e1 e2   -> perfOp (\ x y -> Right $ x** y ) e1 e2
                SquareRoot e1 -> perfOp (\_ v -> if v >=0 then Right (sqrt v) else Left (RootOfNegative e1)) e1 e1
                Var v         -> case M.lookup v ls of 
                                        Just val -> Right val
                                        Nothing -> Left $ UndefinedVariable v
       where  perfOp op e1 e2 = case (eval e1 ls, eval e2 ls) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er
