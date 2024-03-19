{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Expr where

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

instance Floating a => Num (Expr a) where
  a + b = a :+ b
  a * b = a :* b
  fromInteger val = Expr (fromInteger val)
  negate a = a * (-1)