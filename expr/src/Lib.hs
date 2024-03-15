{-# LANGUAGE InstanceSigs #-}
module Lib (Expr(..), Error(..), eval, simplify) where
import Data.Either (fromLeft)
import Data.Map.Strict as M ( lookup, Map )

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

instance (Num a) => Num (Expr a) where
  (+) :: Num a => Expr a -> Expr a -> Expr a
  (+) = Add 
  (*) :: Num a => Expr a -> Expr a -> Expr a
  (*) = Multiply
  abs :: Num a => Expr a -> Expr a
  abs = undefined
  signum :: Num a => Expr a -> Expr a
  signum = undefined
  fromInteger :: Num a => Integer -> Expr a
  fromInteger = Const . fromInteger
  negate :: Num a => Expr a -> Expr a
  negate = Multiply (Const (-1))
  

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


getVar :: Ord k => k -> Map k b -> Either Error b
getVar name map = case M.lookup name map of
  Just x -> Right x
  _ -> Left VariableIsUndefined




eval :: (Ord b, Floating b) => Expr b -> Map String b -> Either Error b
eval expr map = case expr of
  Const val -> Right val
  Var name -> getVar name map
  Add a b -> performOp (+) (eval a map) (eval b map)
  Subtract a b -> performOp (-) (eval a map) (eval b map)
  Multiply a b -> performOp (*) (eval a map) (eval b map)
  Divide a b -> performOpWithCheck checkDivisorIsZero (/) (eval a map) (eval b map)
  Power a b -> performOpWithCheck (checkFirstArgIsNegative PowerBaseIsNegative) (**) (eval a map) (eval b map)
  Square a -> performOpWithCheck (checkFirstArgIsNegative SquareRootIsNegative) (**) (eval a map) (Right 0.5)


simplifyAdd :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyAdd (Const 0.0) x = x
simplifyAdd x (Const 0.0) = x
simplifyAdd x y = Add x y

simplifySubtract :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifySubtract (Const x) (Const y) 
    | x == y = Const 0.0
    | y == 0.0 = Const x
    | otherwise = Subtract (Const x) (Const y)
simplifySubtract (Var x) (Var y)
    | x == y = Const 0.0
    | otherwise = Subtract (Var x) (Var y)
simplifySubtract x (Const 0.0) = x
simplifySubtract x y = Subtract x y


chooseMultiplyByZeroSimplification :: Fractional a => Expr a -> Expr a -> Expr a
chooseMultiplyByZeroSimplification initExpr expr = case expr of
  Const _ -> Const 0.0
  Var _ -> Const 0.0
  _ -> initExpr

simplifyMultiply :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyMultiply (Const 0.0) x = chooseMultiplyByZeroSimplification (Multiply (Const 0.0) x) x
simplifyMultiply x (Const 0.0) = chooseMultiplyByZeroSimplification (Multiply x (Const 0.0)) x
simplifyMultiply (Const 1.0) x = x
simplifyMultiply x (Const 1.0) = x
simplifyMultiply x y = Multiply x y

simplifyDivide :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyDivide (Const 0.0) (Const x)
    | x /= 0 = Const 0.0
    | otherwise = Divide (Const 0.0) (Const x)
simplifyDivide x (Const 1.0) = x
simplifyDivide (Const x) (Const y)
    | x == y && x /= 0.0 = Const 1.0
    | otherwise = Divide (Const x) (Const y)
simplifyDivide x y = Divide x y



simplifyPower :: (Fractional a, Ord a) => Expr a -> Expr a -> Expr a
simplifyPower (Const x) (Const 1.0)
    | x >= 0.0 = Const x
    | otherwise = Power (Const x) (Const 1.0)
simplifyPower (Const x) (Const 0.0)
     | x > 0.0 = Const 1.0
     | otherwise = Power (Const x) (Const 0.0)
simplifyPower x y = Power x y

simplify :: (Ord a, Fractional a) =>  Expr a -> Expr a
simplify expr = case expr of
  Add l r -> simplifyAdd (simplify l) (simplify r)
  Subtract l r -> simplifySubtract (simplify l) (simplify r)
  Multiply l r -> simplifyMultiply (simplify l) (simplify r)
  Divide l r -> simplifyDivide (simplify l) (simplify r)
  Power l r -> simplifyPower (simplify l) (simplify r)
  Square l -> Square (simplify l)
  _ -> expr