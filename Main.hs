module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either (fromRight, fromLeft)
import Language.Haskell.TH (Exp)

newtype MyEither a b = MyEither (Either a b) deriving (Show)

instance Functor (MyEither err) where
    fmap :: (f -> t) -> MyEither err f -> MyEither err t
    fmap _ (MyEither (Left x)) = MyEither(Left x)
    fmap f (MyEither (Right x)) = MyEither(Right (f x))


newtype MyArrow a b = MyArrow((->) a b)

instance Functor (MyArrow f) where
    fmap :: (a -> b) -> MyArrow f a -> MyArrow f b
    fmap f (MyArrow g)  = MyArrow (f . g)



data Expr a = Const a | Var String | Add (Expr a) (Expr a) | Subtract (Expr a) (Expr a) | Multiply (Expr a) (Expr a) | Divide (Expr a) (Expr a) | Power (Expr a) (Expr a) | Square (Expr a) deriving (Eq)

instance (Show a) => Show (Expr a) where
  show :: Expr a -> String
  show (Const x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multiply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Divide x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Power x y) = show x ++ " ^ " ++ show y
  show (Square x) = "sqrt(" ++ show x ++ ")"
  show (Var name) = name


data Error = DivisorIsZero | SquareRootIsNegative | PowerBaseIsNegative | VariableIsUndefined deriving Eq
instance Show Error where
  show :: Error -> String
  show DivisorIsZero = "Divisor is equal zero"
  show SquareRootIsNegative  = "The root can't be negative"
  show PowerBaseIsNegative  = "Power base can't be negative"
  show VariableIsUndefined = "Variable is undefined"


performOp :: (a -> a -> a) -> Either Error a -> Either Error a -> Either Error a
performOp f a b = case a of
    Right fst -> case b of
      Right snd -> Right (f fst snd)
      err -> err
    err -> err


performOpWithCheck :: (Either Error a -> Either Error a -> Maybe Error) -> (a -> a -> a) -> Either Error a -> Either Error a -> Either Error a
performOpWithCheck pr f a b = case pr a b of
  Nothing -> performOp f a b
  Just err -> Left err

checkDivisorIsZero :: (Eq a, Fractional a) => Either Error a -> Either Error a -> Maybe Error
checkDivisorIsZero _ b = case b of 
  Right 0.0 -> Just DivisorIsZero
  Right _ -> Nothing
  Left err -> Just err


checkFirstArgIsNegative :: (Ord a, Num a) => Error -> Either Error a -> Either Error a -> Maybe Error
checkFirstArgIsNegative err (Right x) _
  | x < 0 = Just err
  | otherwise = Nothing
  
checkFirstArgIsNegative err a _ = Just (fromLeft err a)


getVar :: [(String, a)] -> String -> Either Error a
getVar map name = case filter (\x -> fst x == name) map of
  xs:x -> Right (snd xs)
  _ -> Left VariableIsUndefined


eval :: (Ord a, Floating a) => Expr a -> [(String, a)] -> Either Error a
eval expr map = case expr of
  Const val -> Right val
  Var name -> getVar map name
  Add a b -> performOp (+) (eval a map) (eval b map)
  Subtract a b -> performOp (-) (eval a map) (eval b map)
  Multiply a b -> performOp (*) (eval a map) (eval b map)
  Divide a b -> performOpWithCheck checkDivisorIsZero (/) (eval a map) (eval b map)
  Power a b -> performOpWithCheck (checkFirstArgIsNegative PowerBaseIsNegative) (**) (eval a map) (eval b map)
  Square a -> performOpWithCheck (checkFirstArgIsNegative SquareRootIsNegative) (**) (eval a map) (Right 0.5)


simplifyAdd :: (Eq a, Fractional a) => Expr a -> Expr a
simplifyAdd expr = case expr of
  Add (Const 0.0) x -> x
  Add x (Const 0.0) -> x
  _ -> expr

simplifySubtract :: (Eq a, Fractional a) => Expr a -> Expr a
simplifySubtract expr = case expr of
  Subtract x (Const 0.0) -> x
  _ -> expr


chooseMultiplyByZeroSimplification :: Fractional a => Expr a -> Expr a
chooseMultiplyByZeroSimplification expr = case expr of
  Const _ -> Const 0.0
  Var _ -> Const 0.0
  _ -> expr

simplifyMultiply :: (Eq a, Fractional a) => Expr a -> Expr a
simplifyMultiply expr = case expr of
  Multiply (Const 0.0) x -> chooseMultiplyByZeroSimplification x
  Multiply x (Const 0.0) -> chooseMultiplyByZeroSimplification x
  Multiply (Const 1.0) x -> x
  Multiply x (Const 1.0) -> x
  _ -> expr

simplifyDivide :: (Eq a, Fractional a) => Expr a -> Expr a
simplifyDivide expr = case expr of
  Divide (Const 0.0) (Const x)
    | x /= 0 -> Const 0.0
    | otherwise -> expr 
  Divide x (Const 1.0) -> x
  Divide (Const x) (Const y)
    | x == y && x /= 0.0 -> Const 1.0
    | otherwise -> expr
  _ -> expr



simplifyPower :: (Fractional a, Ord a) => Expr a -> Expr a
simplifyPower expr = case expr of
  Power (Const x) (Const 1.0)
     | x >= 0.0 -> Const x
     | otherwise -> expr
  Power (Const x) (Const 0.0)
     | x >= 0.0 -> Const 1.0
     | otherwise -> expr
  _ -> expr

simplify :: (Ord a, Fractional a) =>  Expr a -> Expr a
simplify expr = case expr of
  Add l r -> simplifyAdd (Add (simplify l) (simplify r))
  Subtract l r -> simplifySubtract (Subtract (simplify l) (simplify r))
  Multiply l r -> simplifyMultiply (Multiply (simplify l) (simplify r))
  Divide l r -> simplifyDivide (Divide (simplify l) (simplify r))
  Power l r -> simplifyPower (Power (simplify l) (simplify r))
  Square l -> Square (simplify l)
  _ -> expr



cases :: Fractional a => [(Expr a, Either Error a)]
cases = [
  (Const 1, Right 1.0),
  (Var "x", Right 1.0),
  (Var "undefined", Left VariableIsUndefined),
  (Add (Const 1) (Const 1), Right 2.0),
  (Subtract (Const 1) (Const 1), Right 0.0),
  (Multiply (Const 2) (Const 3), Right 6.0),
  (Divide (Const 5) (Const 3), Right (5/3)),
  (Divide (Const 5) (Const 0.0), Left DivisorIsZero),
  (Square (Const 9), Right 3.0),
  (Square (Const (-3)), Left SquareRootIsNegative),
  (Power (Const 3) (Const 3), Right 27.0),
  (Power (Const (-3)) (Const 3), Left PowerBaseIsNegative),
  (Add (Multiply (Const 5) (Const 3)) (Divide (Subtract (Const 15) (Const 5)) (Const 5)), Right 17.0),
  (Square (Square (Square (Const 256.0))), Right 2.0),
  (Power (Const 2) (Power (Const 2) (Const 2)), Right 16),
  (Power (Var "y") (Var "z"), Right 8)
  ]

test :: (Show a, Ord a, Floating a) => Expr a -> Either Error a -> IO ()
test expr expected =
    let testMap = [("x", 1), ("y", 2), ("z", 3)]
        actual = eval expr testMap in
    unless (expected == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual)


main :: IO ()
main = do
  mapM_ (uncurry test) cases
  putStrLn "Done"
