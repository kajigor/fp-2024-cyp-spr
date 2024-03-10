module Eval (eval) where
import Error ( Error(..) ) 
import Expr ( Expr(..) )
import Operations (Operation(..))
import qualified Data.Map.Strict as M 

eval :: (Fractional a, Floating a, Ord a, Show a, Eq a) => Expr a -> M.Map String a -> Either Error a 
eval (Const b) _ = Right b
eval (Var name) env = 
  case M.lookup name env of
    Nothing -> Left $ UndefVar name
    Just v -> Right v

eval (Sqrt exp) env = 
  case eval exp env of 
    Left err -> Left err
    Right a -> if a < 0 then Left $ NegativeRoot $ show a else Right $ sqrt a

eval (Bin exp1 op exp2) env
  | op == Plus = resolveOperation exp1 (+) exp2 env
  | op == Minus = resolveOperation exp1 (-) exp2 env
  | op == Mult = resolveOperation exp1 (*) exp2 env
  | op == Div = 
    let ans1 = eval exp1 env
        ans2 = eval exp2 env in
    case (ans1, ans2) of 
      (Left _, _) -> ans1
      (_, Left _) -> ans2
      (Right d1, Right 0.0) -> Left $ DivByZero $ show d1
      _ -> resolveOperation exp1 (/) exp2 env
  | op == Pow = resolveOperation exp1 (**) exp2 env

eval Bin {} _ = Left $ DivByZero "Wrong pattern matching!"

resolveOperation :: (Floating a, Ord a, Show a, Fractional a) => Expr a -> (a -> a -> a) -> Expr a -> M.Map String a -> Either Error a
resolveOperation exp1 op exp2 env =
  let ans1 = eval exp1 env 
      ans2 = eval exp2 env in
  case (ans1, ans2) of
    (Left _, _) -> ans1
    (_, Left _) -> ans2
    (Right d1, Right d2) -> Right $ d1 `op` d2