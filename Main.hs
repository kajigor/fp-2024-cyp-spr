{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import Text.Printf (printf)
import Control.Monad (unless)

data Expr a = Var String | Expr a | Sq (Expr a) | (:+) (Expr a) (Expr a) | (:-) (Expr a) (Expr a) | (:*) (Expr a) (Expr a) | (:/) (Expr a) (Expr a) | (:^) (Expr a) (Expr a)
  deriving Eq

instance Show a => Show (Expr a) where
  show (Expr x) = show x
  show (Var x) = show x
  show (Sq x) = "sqrt(" ++ (show x ++ ")")
  show ((:+) x y) = '(':show x ++ (") + (" ++ (show y ++ ")"))
  show ((:-) x y) = '(':show x ++ (") - (" ++ (show y ++ ")"))
  show ((:*) x y) = '(':show x ++ (") * (" ++ (show y ++ ")"))
  show ((:/) x y) = '(':show x ++ (") / (" ++ (show y ++ ")"))
  show ((:^) x y) = '(':show x ++ (") ^ (" ++ (show y ++ ")"))


newtype Error = Error String
  deriving Eq

instance Show Error where
  show (Error x) = "Error: " ++ show x

checkErrors :: (Either Error a, Either Error a) -> Either Error (a, a)
checkErrors (Left x, _) = Left x
checkErrors (_, Left y) = Left y
checkErrors (Right x, Right y) = Right (x, y)

computeOrError :: (Floating a, Ord a) => (a -> a -> a) -> (a -> a -> Bool) -> String -> (Expr a, Expr a) -> [(String, a)]-> Either Error a
computeOrError f predicate errorMsg (expA, expB) li =
  case checkErrors (eval expA li, eval expB li) of
    Left err -> Left err
    Right (x, y) -> if predicate x y then Right (f x y) else Left (Error errorMsg)

alwaysCorrect :: a -> a -> Bool
alwaysCorrect a b = True

substValue :: String -> [(String, a)] -> Either Error a
substValue var vars =
  let checkUniqueVar li =
        case li of
          [] -> Left (Error ("Unknown variable: " ++ var))
          (h:t) -> if null t then Right (snd h) else Left (Error ("Non-unique variable value: " ++ var))
  in checkUniqueVar (filter (\(x, y) -> var == x) vars)

eval :: (Floating a, Ord a) => Expr a -> [(String, a)]-> Either Error a
eval (Var x) = substValue x
eval (Expr x) = const (Right x)
eval (Sq x) = computeOrError (const sqrt) (\val -> (<=) 0) "sqrt from negative taken" (x, x)
eval ((:+) x y) = computeOrError (+) alwaysCorrect "" (x, y)
eval ((:-) x y) = computeOrError (-) alwaysCorrect "" (x, y)
eval ((:*) x y) = computeOrError (*) alwaysCorrect "" (x, y)
eval ((:/) x y) = computeOrError (/) (\a -> (/=) 0) "divided by zero" (x, y)
eval ((:^) x y) = computeOrError (**) (\a b -> (<) 0 a) "power used with non-positive base" (x, y)

cases :: [((Expr Double, [(String, Double)]), Either Error Double)]
cases = [((Expr 1.0, []), Right 1.0) --base
  ,((Sq (Expr 4.0), []), Right 2.0) --Sq
  ,((Expr 1.0 :+ Sq (Expr (-4.0)), []), Left (Error "sqrt from negative taken")) --base Error
  ,(((Expr 2.0 :^ Expr 3.0) :+ ((Expr 2.0 :* Expr 3.0) :- (Expr 4.0 :/ Expr 1.0)), []), Right 10.0) --base operations
  ,(((Expr 1.0 :/ Expr 0.0) :+ Sq (Expr (-1.0)), []), Left (Error "divided by zero")) --multiple errors
  ,((Expr (-1.0) :^ Expr 3.0, []), Left (Error "power used with non-positive base")) --negative base error
  ,((Expr 0.0 :^ Expr (-3.0), []), Left (Error "power used with non-positive base")) --zero base error
  ,((Var "x", [("x", 8.0)]), Right 8.0) --base variable substitution
  ,((Var "x", []), Left (Error "Unknown variable: x")) --variable substitution without value
  ,((Var "x", [("x", 2), ("x", 3)]), Left (Error "Non-unique variable value: x")) --variable substitution with too many values
  ,((Var "x" :+ Sq (Expr (-1.0)), [("x", 8.0)]), Left (Error "sqrt from negative taken"))] --base variable substitution with other error

test :: (Floating a, Ord a, Show a) => (Expr a, [(String, a)]) -> Either Error a -> IO ()
test (expr, variables) expected =
    let actual = eval expr variables in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

--HW04 below

data MyEither a b = MyLeft a | MyRight b

instance Functor (MyEither a) where
  fmap f (MyRight x) = MyRight (f x)

-- fmap id (MyRight x) = MyRight x
-- fmap (f . g) (MyRight x) = MyRight ((f . g) x), 
-- (fmap f . fmap g) (MyRight x) = fmap f (MyRight (g x)) = MyRight ((f . g) x)

-- instance Functor ((->) a) where  
--   fmap f x = f . x
-- fmap id x = id . x = x
-- fmap (f . g) x = (f . g) . x = f . g . x = f . (fmap g x) = (fmap f . fmap g) x

instance Floating a => Num (Expr a) where
  a + b = a :+ b
  a * b = a :* b
  fromInteger val = Expr (fromInteger val)
  negate a = a * (-1)

simplify :: (Floating a, Eq a, Show a) => Expr a -> Expr a
simplify (Expr a) = Expr a
simplify (Var x) = Var x
simplify ((:-) x y) = if x == y then Expr 0.0 else simplify x :- simplify y
simplify (x :+ y) = 
  case (simplify x, simplify y) of
    (Expr 0.0, newY) -> newY 
    (newX, Expr 0.0) -> newX 
    _ -> simplify x :+ simplify y
simplify (x :* y) = 
  case (simplify x, simplify y) of
    (Expr 0.0, newY) -> Expr 0.0 
    (newX, Expr 0.0) -> Expr 0.0
    (Expr 1.0, newY) -> newY 
    (newX, Expr 1.0) -> newX 
    _ -> simplify x :* simplify y
simplify (x :/ y) = 
  case (simplify x, simplify y) of
    (newX, Expr 1.0) -> newX
    _ -> simplify x :/ simplify y  
simplify (x :^ y) = 
  case (simplify x, simplify y) of
    (newX, Expr 1.0) -> newX
    _ -> simplify x :^ simplify y    

simplifyCases :: Floating a => [(Expr a, Expr a)]
simplifyCases = [(Expr 1.0 :+ (Expr 0.0 :* (Expr 2.0 :+ Expr 3.0)) :+ Expr 0.0 :+ (Expr 2.0 :^ Expr 1.0), Expr 1.0 + Expr 2.0) --basic simplify
  ,(Expr 0.0 + (Expr 1.0 * Var "x") + Expr 0.0, Var "x") --simplify with variables
  ,(1 + 0 * Var "x", 1)] --straight simplify

testSimplify :: (Floating a, Eq a, Show a) => Expr a -> Expr a -> IO ()
testSimplify expr expected =
    let actual = simplify expr in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "simplify (%s) should be %s but it was %s" (show expr) (show expected) (show actual)

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  mapM_ (uncurry testSimplify) simplifyCases

