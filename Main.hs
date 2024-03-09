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
  deriving Eq

instance Show Operation where 
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "
  show Pow  = " ^ "

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

data Error = DivByZero [Char]
  | NegativeRoot [Char] 
  | UndefVar [Char]
  deriving Eq


instance Show Error where 
  show (DivByZero msg) = printf "ERROR_DIV_BY_ZERO: %s / 0" msg
  show (NegativeRoot msg) = printf "ERROR_NEGATIVE_ROOT: %s" msg
  show (UndefVar mag) = printf "ERROR_UNDEFINED_VAR: %s" mag

eval :: (Fractional a, Floating a, Ord a, Show a) => Expr a -> [(String, a)] -> Either Error a 
eval (Const b) env = Right b
eval (Var name) env = 
  case lookup name env of
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

resolveOperation :: (Floating a, Ord a, Show a, Fractional a) => Expr a -> (a -> a -> a) -> Expr a -> [(String, a)] -> Either Error a
resolveOperation exp1 op exp2 env =
  let ans1 = eval exp1 env 
      ans2 = eval exp2 env in
  case (ans1, ans2) of
    (Left _, _) -> ans1
    (_, Left _) -> ans2
    (Right d1, Right d2) -> Right $ d1 `op` d2

simplify :: (Eq a, Fractional a, Ord a) => Expr a -> Expr a
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

   
  
casesSimplify :: (RealFloat a, Ord a, Show a) => [(Expr a, Expr a)]
casesSimplify = [
  (Bin (Const 1.0) Plus (Bin (Const 0.0) Plus (Const 1.0)), Bin (Const 1.0) Plus (Const 1.0)),
  (Bin (Const 1.0) Minus (Bin (Const 0.0) Minus (Const 1.0)), Bin (Const 1.0) Minus (Const (-1.0))),
  (Bin (Const 1.0) Minus (Bin (Const 1.0) Minus (Const 0.0)), Bin (Const 1.0) Minus (Const 1.0)),
  (Sqrt (Const 1.0), Const 1.0),
  (Sqrt (Const 0.0), Const 0.0),
  (Bin (Const 3.0) Mult (Const 0.0), Const 0.0),
  (Bin (Const 0.0) Mult (Sqrt (Const 7.0)), Const 0.0),
  (Bin (Const 0) Div (Var "x"), Bin (Const 0) Div (Var "x")),
  (Bin (Bin (Const 1.0) Plus (Const 7.0)) Mult (Const 1), Bin (Const 1.0) Plus (Const 7.0)),
  (Bin (Var "x") Minus (Var "x"), Const 0.0),
  (Bin (Var "y") Div (Const 1.0), Var "y")
  ]


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
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 
  
testSimplify :: (Show a, Eq a,  Fractional a, Ord a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
  let actual = simplify expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO () 
main = do 
  mapM_ (\(expr, env ,expected) -> test expr env expected) cases
  mapM_ (uncurry testSimplify) casesSimplify
  --mapM_ (uncurry test) cases 
  