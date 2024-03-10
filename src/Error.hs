{-# LANGUAGE InstanceSigs #-}
module Error (Error(..)) where
import Text.Printf (printf)

data Error = DivByZero [Char]
  | NegativeRoot [Char] 
  | UndefVar [Char]
  deriving Eq


instance Show Error where 
  show :: Error -> String
  show (DivByZero msg) = printf "ERROR_DIV_BY_ZERO: %s / 0" msg
  show (NegativeRoot msg) = printf "ERROR_NEGATIVE_ROOT: %s" msg
  show (UndefVar mag) = printf "ERROR_UNDEFINED_VAR: %s" mag