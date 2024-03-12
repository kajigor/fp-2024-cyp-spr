module Expr where

import Text.Printf (printf)
import Control.Monad (unless)
import qualified Data.Map.Strict as M


data BinOperator = Plus | Minus | Multiply | Divide | Power deriving (Eq)
instance Show BinOperator where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Divide = "/"
  show Power = "^"

data Expr a = Const a | Var String |  SquareRoot (Expr a) | BinExpr BinOperator (Expr a) (Expr a) deriving Eq 

instance (Show a) => Show (Expr a) where 
  show (Const n) = show n 
  show (Var s) = show s
  show (SquareRoot exp) = printf "(square root of %s)"  (show exp)
  show (BinExpr binOp leftExp rightExp) = printf "(%s %s %s)" (show leftExp) (show binOp) (show rightExp)

instance (Num a) => Num (Expr a) where
  (+) = BinExpr Plus
  (*) = BinExpr Multiply
  negate = BinExpr Multiply (Const (-1))
  fromInteger = Const . fromInteger


data Error = DivisionByZero | SquareRootOfNegative | NoSuchVariable deriving Eq 

instance Show Error where 
  show (DivisionByZero) = printf "Division by zero"
  show (SquareRootOfNegative) = printf "Square root of negative number"
  show (NoSuchVariable) = printf "No such variable"


eval :: (Floating a, Ord a) => Expr a -> M.Map String a -> Either Error a 
eval (Const n) _ = Right n
eval (Var s) xs = evalVarValue s xs
eval (SquareRoot exp) xs = evalSquareRootExp (eval exp xs)
eval (BinExpr binOp exp1 exp2) xs = evalBinExp binOp (eval exp1 xs) (eval exp2 xs) 

evalVarValue :: String -> M.Map String a -> Either Error a 
evalVarValue s map = case M.lookup s map of
  Nothing -> Left NoSuchVariable
  Just a -> Right a 

evalSquareRootExp :: (Floating a, Ord a) =>  Either Error a -> Either Error a
evalSquareRootExp (Right n) = if n < 0 then Left SquareRootOfNegative else Right (sqrt n)
evalSquareRootExp err = err

evalBinExp ::  (Floating a, Ord a) => BinOperator -> Either Error a ->  Either Error a -> Either Error a
evalBinExp _ (Left err) _ = Left err
evalBinExp _ _ (Left err) = Left err
evalBinExp Plus (Right n1) (Right n2) = Right (n1 + n2)
evalBinExp Minus (Right n1) (Right n2) = Right (n1 - n2)
evalBinExp Multiply (Right n1) (Right n2) = Right (n1 * n2)
evalBinExp Power (Right n1) (Right n2) = Right (n1 ** n2)
evalBinExp Divide (Right n1) (Right n2) = evalDivideExp n1 n2

evalDivideExp :: (Floating a, Ord a) => a -> a -> Either Error a
evalDivideExp _ 0 = Left DivisionByZero
evalDivideExp n1 n2 = Right (n1 / n2)

simplify :: (Num a, Ord a) => Expr a -> Expr a
simplify (SquareRoot x) = simplifySquare (SquareRoot (simplify x)) where
    simplifySquare (SquareRoot (Const 0)) = (Const 0)
    simplifySquare (SquareRoot (Const 1)) = (Const 1)
    simplifySquare exp = exp
simplify (BinExpr binOp left right) = simplifyBinExp (BinExpr binOp (simplify left) (simplify right)) where
  simplifyBinExp (BinExpr Plus (Const 0) x) = simplify x
  simplifyBinExp (BinExpr Plus x (Const 0)) = simplify x
  simplifyBinExp (BinExpr Minus x (Const 0)) = simplify x
  simplifyBinExp (BinExpr Multiply (Const 1) x) = simplify x
  simplifyBinExp (BinExpr Multiply x (Const 1)) = simplify x
  simplifyBinExp (BinExpr Multiply (Const 0) _) = Const 0
  simplifyBinExp (BinExpr Multiply _ (Const 0)) = Const 0
  simplifyBinExp (BinExpr Divide x (Const 1)) = simplify x
  simplifyBinExp (BinExpr Power (Const 1) _) = (Const 1)
  simplifyBinExp (BinExpr Power (Const 0) _) = (Const 0)
  simplifyBinExp (BinExpr Power x (Const 1)) = simplify x
  simplifyBinExp (BinExpr Power x (Const 0)) = (Const 1)
  simplifyBinExp exp = exp
simplify x = x

