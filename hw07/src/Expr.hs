{-# LANGUAGE InstanceSigs #-}
module Expr (BinaryOperator(..), UnaryOperator(..), Expr(..), Error(..), eval, simplify) where
import Data.Either (fromLeft)
import Data.Map.Strict as M ( lookup, Map, empty, fromList)
import StateDemo (State, get, execState)

data BinaryOperator = Plus | Minus | Multiply | Divide | Power deriving (Eq)

data UnaryOperator = Square deriving Eq

data Expr a = Const a | Var String | BinOp BinaryOperator (Expr a) (Expr a) | UnOp UnaryOperator (Expr a) deriving (Eq)

instance (Show a) => Show (Expr a) where
  show :: Expr a -> String
  show (Const x) = show x
  show (BinOp Plus x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (BinOp Minus x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (BinOp Multiply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (BinOp Divide x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (BinOp Power x y) = show x ++ " ^ " ++ show y
  show (UnOp Square x) = "sqrt(" ++ show x ++ ")"
  show (Var name) = name

instance (Num a) => Num (Expr a) where
  (+) :: Expr a -> Expr a -> Expr a
  (+) = BinOp Plus
  (*) :: Expr a -> Expr a -> Expr a
  (*) = BinOp Multiply
  abs :: Expr a -> Expr a
  abs = undefined
  signum :: Expr a -> Expr a
  signum = undefined
  fromInteger :: Integer -> Expr a
  fromInteger = Const . fromInteger
  negate :: Expr a -> Expr a
  negate = BinOp Multiply (Const (-1))


data Error = DivisorIsZero | SquareRootIsNegative | PowerBaseIsNegative | VariableIsUndefined deriving Eq
instance Show Error where
  show :: Error -> String
  show DivisorIsZero = "Divisor is equal zero"
  show SquareRootIsNegative  = "The root can't be negative"
  show PowerBaseIsNegative  = "Power base can't be negative"
  show VariableIsUndefined = "Variable is undefined"


performOp :: (a -> a -> a) -> Either Error a -> Either Error a -> Either Error a
performOp f a b = case a of
    Right first -> case b of
      Right second -> Right (f first second)
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

getVar :: Maybe b -> Either Error b
getVar value = case value of
  Just x -> Right x
  _ -> Left VariableIsUndefined


eval :: (Ord b, Floating b) => Expr b -> State (Map String b) (Either Error b)
eval (Const x) = do
  return (Right x)
eval (Var v) = do
  getVar . M.lookup v <$> get
eval (BinOp op x y) = do
  f <- eval x
  s <- eval y
  return ((case op of 
    Plus -> performOp (+) 
    Minus -> performOp (-) 
    Multiply ->  performOp (*) 
    Divide -> performOpWithCheck checkDivisorIsZero (/)
    Power -> performOpWithCheck (checkFirstArgIsNegative PowerBaseIsNegative) (**)) f s)
eval (UnOp op x) = do
  f <- eval x
  return (case op of
    Square -> performOpWithCheck (checkFirstArgIsNegative SquareRootIsNegative) (**) f (Right 0.5)
    )


simplifyAdd :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyAdd (Const 0.0) x = x
simplifyAdd x (Const 0.0) = x
simplifyAdd x y = BinOp Plus x y

simplifySubtract :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifySubtract (Const x) (Const y) 
    | x == y = Const 0.0
    | y == 0.0 = Const x
    | otherwise = BinOp Minus (Const x) (Const y)
simplifySubtract (Var x) (Var y)
    | x == y = Const 0.0
    | otherwise = BinOp Minus (Var x) (Var y)
simplifySubtract x (Const 0.0) = x
simplifySubtract x y = BinOp Minus x y


chooseMultiplyByZeroSimplification :: Fractional a => Expr a -> Expr a -> Expr a
chooseMultiplyByZeroSimplification initExpr expr = case expr of
  Const _ -> Const 0.0
  Var _ -> Const 0.0
  _ -> initExpr

simplifyMultiply :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyMultiply (Const 0.0) x = chooseMultiplyByZeroSimplification (BinOp Multiply (Const 0.0) x) x
simplifyMultiply x (Const 0.0) = chooseMultiplyByZeroSimplification (BinOp Multiply x (Const 0.0)) x
simplifyMultiply (Const 1.0) x = x
simplifyMultiply x (Const 1.0) = x
simplifyMultiply x y = BinOp Multiply x y

simplifyDivide :: (Eq a, Fractional a) => Expr a -> Expr a -> Expr a
simplifyDivide (Const 0.0) (Const x)
    | x /= 0 = Const 0.0
    | otherwise = BinOp Divide (Const 0.0) (Const x)
simplifyDivide x (Const 1.0) = x
simplifyDivide (Const x) (Const y)
    | x == y && x /= 0.0 = Const 1.0
    | otherwise = BinOp Divide (Const x) (Const y)
simplifyDivide x y = BinOp Divide x y


simplifyPower :: (Fractional a, Ord a) => Expr a -> Expr a -> Expr a
simplifyPower (Const x) (Const 1.0)
    | x >= 0.0 = Const x
    | otherwise = BinOp Power (Const x) (Const 1.0)
simplifyPower (Const x) (Const 0.0)
     | x > 0.0 = Const 1.0
     | otherwise = BinOp Power (Const x) (Const 0.0)
simplifyPower x y = BinOp Power x y

simplify :: (Ord a, Fractional a) =>  Expr a -> Expr a
simplify expr = case expr of
  BinOp Plus l r -> simplifyAdd (simplify l) (simplify r)
  BinOp Minus l r -> simplifySubtract (simplify l) (simplify r)
  BinOp Multiply l r -> simplifyMultiply (simplify l) (simplify r)
  BinOp Divide l r -> simplifyDivide (simplify l) (simplify r)
  BinOp Power l r -> simplifyPower (simplify l) (simplify r)
  UnOp Square l -> UnOp Square (simplify l)
  _ -> expr