module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either (isLeft, fromRight)
import Distribution.Simple.Install (install)
import Distribution.TestSuite (Result(Error))

data Eather_ a b = 
  Left_ a |
  Right_ b

instance Functor (Eather_ a) where
  fmap :: (b -> c) -> Eather_ a b -> Eather_ a c
  fmap _ (Left_ a) = Left_ a
  fmap f (Right_ b) = Right_ $ f b
-- Proof why it works:
-- id: 
--     fmap id (Left_ a) = Left_ a
--     fmap id (Right_ b) = Right_ (id b) = Right_ b
-- Composition:
--     fmap (f . g) (Left_ a) = Left_ a
--     fmap (f . g) (Right_ b) = Right (f . g) b

-- first impl.
data ArrowRR a b = ArrowRR (a -> b)

instance Functor (ArrowRR a) where 
  fmap :: (b -> c) -> ArrowRR a b -> ArrowRR a c
  fmap f (ArrowRR t) = ArrowRR (f . t)
-- second impl
newtype ArrowR a b = ArrowR {getArrowR :: a -> b}

instance Functor (ArrowR a) where
  fmap :: (b -> c) -> ArrowR a b -> ArrowR a c
  fmap f (ArrowR t) = ArrowR (f . t)
-- Proof why it works:
-- id:
--     fmap id t = id . t
-- Composition:
--     fmap (f . g) t = f . g . t

data Operation = Plus 
  | Minus 
  | Mult 
  | Div 
  | Pow 

instance Show Operation where 
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "
  show Pow  = " ^ "

instance Eq Operation where 
  Plus == Plus = True
  Minus == Minus = True
  Mult == Mult = True
  Div == Div = True
  Pow == Pow = True
  _ == _ = False

data Expr a = 
  Var String
  | Const a
  | Bin (Expr a) Operation (Expr a)
  | Sqrt (Expr a)
  deriving Eq

instance (Show a) => Show (Expr a) where 
  show :: Expr a -> String
  show (Const a) = show a
  show (Sqrt exp) = printf " sqrt( %s )" (show exp)
  show (Bin exp1 oper exp2) = printf "(%s)" (show exp1 ++ show oper ++ show exp2)
  show (Var name) = name

-- instance (Eq a, Floating a, Ord a, Show a, Fractional a) => Eq (Expr a) where 
--   (==) :: Expr a -> Expr a -> Bool
--   Const a == Const b = a == b
--   Var a == Var b = a == b
--   Sqrt exp1 == Sqrt exp2 = exp1 == exp2
--   Bin a1 op1 b1 == Bin a2 op2 b2 = op1 == op2 && a1 == a2 && b1 == b2
--   a == b = eval a == eval b

data Error = DivByZero [Char]
  | NegativeRoot [Char] 
  | UndefVar [Char]
  deriving Eq


instance Show Error where 
  show (DivByZero msg) = printf "ERROR_DIV_BY_ZERO: %s / 0" msg
  show (NegativeRoot msg) = printf "ERROR_NEGATIVE_ROOT: %s" msg
  show (UndefVar mag) = printf "ERROR_UNDEFINED_VAR: %s" mag

-- instance Eq Error where 
--   DivByZero _ == DivByZero _ = True
--   NegativeRoot _ == NegativeRoot _ = True
--   _ == _ = True

eval :: (Fractional a, Floating a, Ord a, Show a) => Expr a -> [(String, a)] -> Either Error a 
eval (Const b) env = Right b
eval (Var name) env = 
  case [v | (n, v) <- env, n == name] of
    [] -> Left $ UndefVar name
    v:_ -> Right v

eval (Sqrt exp) env = 
  let ans = eval exp env in 
    if isLeft ans then ans else 
      let val = fromRight 1 ans in 
        if val < 0 then Left $ NegativeRoot $ show val 
        else Right $ sqrt val

eval (Bin exp1 op exp2) env
  | op == Plus = resolveOperation exp1 (+) exp2 env
  | op == Minus = resolveOperation exp1 (-) exp2 env
  | op == Mult = resolveOperation exp1 (*) exp2 env
  | op == Div = 
    let ans1 = eval exp1 env
        ans2 = eval exp2 env in
    case (ans1, ans2) of 
      (Right _, Left _) -> ans2
      (Left msg, _) -> ans1
      (Right d1, Right 0.0) -> Left $ DivByZero $ show d1
      _ -> resolveOperation exp1 (/) exp2 env
  | op == Pow = resolveOperation exp1 (**) exp2 env

resolveOperation :: (Floating a, Ord a, Show a, Fractional a) => Expr a -> (a -> a -> a) -> Expr a -> [(String, a)] -> Either Error a
resolveOperation exp1 op exp2 env =
  let ans1 = eval exp1 env 
      ans2 = eval exp2 env in
  case (ans1, ans2) of
    (Right _, Left _) -> ans2
    (Left msg, _) -> ans1
    (Right d1, Right d2) -> Right $ d1 `op` d2


cases :: (Floating a, Ord a, Fractional a, Show a) => [(Expr a, [(String, a)] ,Either Error a)]
cases = [
  (Sqrt $ Const 1.0, [] ,Right 1.0),
  (Bin (Sqrt (Const 9.0)) Plus (Bin (Const 2.0) Pow (Const 7.0)), [], Right 131.0),
  (Sqrt (Const (-1.0)), [] ,Left $ NegativeRoot "-1.0"),
  (Bin (Bin (Const 7.0) Mult (Const 2.0)) Div (Bin (Const 4.0) Minus (Const 4.0)), [], Left $ DivByZero "14.0"),
  (Bin (Const 7.0) Div (Const 2.0), [], Right 3.5),
  (Bin (Bin (Const 7.0) Div (Sqrt (Const 4.0))) Minus (Bin (Sqrt (Sqrt (Sqrt  (Const 256.0)))) Mult (Const 2)), [], Right (-0.5)),
  (Bin (Const 3.0) Plus (Var "x"), [("x", 1.0)], Right 4.0),
  (Bin (Const 3.0) Plus (Var "x"), [("y", 1.0)], Left $ UndefVar "x")
  ]

test :: (Floating a, Ord a, Fractional a, Show a) => Expr a -> [(String, a)] -> Either Error a -> IO () 
test expr env expected = 
    let actual = eval expr env in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (\(expr, env ,expected) -> test expr env expected) cases
  --mapM_ (uncurry test) cases 
  