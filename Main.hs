module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

-- Part 1

data Either_ a b = Left_ a | Right_ b
instance Functor (Either_ a) where
  fmap f (Left_ x) = Left_ x
  fmap f (Right_ x) = Right_ (f x)

{-
  fmap id = id
  fmap id (Left_ x)  = Left_ x
  fmap id (Right_ x) = Right_ (id x) = Right_ x

  fmap (f . g) = fmap f . fmap g
  fmap (f . g) (Left_ x) = Left_ x
  (fmap f . fmap g) (Left_ x) = fmap f (fmap g (Left_ x)) = fmap f (Left_ x) = Left_ x
  fmap (f . g) (Right_ x) = Right_ ((f . g) x) = Right_ (f (g x))
  (fmap f . fmap g) (Right_ x) = fmap f (fmap g (Right_ x)) = fmap f (Right_ (g x)) = Right_ f (g x)
-}

newtype Arrow_ a b = Arrow_ ((->) a b)
instance Functor (Arrow_ a) where
  fmap f (Arrow_ g) = Arrow_ (f . g)

{-
  fmap id = id
  fmap id (Arrow_ f) = Arrow_ (id . f) == Arrow_ f

  fmap (f . g) = fmap f . fmap g
  fmap (f . g) (Arrow_ h) = Arrow_ ((f . g) . h) = Arrow_ (f . g . h),
  (fmap f . fmap g) (Arrow_ h) = fmap f (Arrow_ (g . h)) = Arrow_ (f . (g . h)) = Arrow_ (f . g . h)

-}

data Expr a = Const a 
            | Sqrt (Expr a)
            | Add (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Pow (Expr a) (Expr a) 
            | Var String
          deriving Eq

instance Show a => Show (Expr a) where
  show e = case e of
          (Const c)    -> show c
          (Sqrt e1)    -> "sqrt(" ++ show e1 ++ ")"
          (Add e1 e2)  -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
          (Sub e1 e2)  -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
          (Mult e1 e2) -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
          (Div e1 e2)  -> "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
          (Pow e1 e2)  -> "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"
          (Var v)      -> show v

data Error  = DivisionByZero | RootOfNegative | UndefinedVar deriving Eq

instance Show Error where
  show DivisionByZero = "DivisionByZero"
  show RootOfNegative = "RootOfNegative"
  show UndefinedVar   = "UndefinedVar"

eval :: (Floating a, Ord a) => Expr a -> [(String, a)] -> Either Error a
eval e memory = case e of
  Const c    -> Right c
  Add e1 e2  -> doOperation (\ x y -> Right $ x + y ) e1 e2
  Sub e1 e2  -> doOperation (\ x y -> Right $ x - y ) e1 e2
  Mult e1 e2 -> doOperation (\ x y -> Right $ x * y ) e1 e2
  Div e1 e2  -> doOperation (\ x y -> if y /= 0 then Right (x/y) else Left DivisionByZero) e1 e2
  Pow e1 e2  -> doOperation (\ x y -> Right $ x ** y ) e1 e2
  Sqrt e1    -> doOperation (\_ v -> if v >= 0 then Right (sqrt v) else Left RootOfNegative) e1 e1
  Var v      -> case lookup v memory of
                  Just val -> Right val
                  Nothing  -> Left UndefinedVar
  where doOperation op e1 e2 = case (eval e1 memory, eval e2 memory) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er

type ExprConstCaseType = Double
cases :: [(Expr Double, [(String, Double)] , Either Error Double)]
cases = [
        (Const 1, [],  Right 1),
        (Var "a", [("a", 1)], Right 1),
        (Add (Var "x") (Var "y"), [("x", 1), ("y", 2)], Right 3),
        (Add (Var "y") (Var "x"), [("x", 10)], Left UndefinedVar),
        (Sub (Var "x") (Var "x"), [("x", 1)], Right 0),
        (Pow (Var "x") (Var "y"), [("x", 3), ("y", 2)], Right 9)
        ]

test :: Expr Double -> [(String, Double)]  -> Either Error Double -> IO ()
test expr var_list expected =
    let actual = eval expr var_list in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (\(expr, expected, vars) -> test expr expected vars) cases
  
