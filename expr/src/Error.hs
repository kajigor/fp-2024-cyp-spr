module Error where

import Expr
import Text.Printf (printf)

data Error a = DivisionByZero (Expr a) (Expr a) | RootOfNegative (Expr a) | UndefinedVariable String | InvalidMapping [(String, a)] deriving Eq

instance Show a => Show (Error a) where 
  show (DivisionByZero e1 e2) = printf "Error when dividing %s by %s. Denominator was equal to zero" (show e1) (show e2)
  show (RootOfNegative e)     = printf "Error when taking square root. Expression %s was negative" (show e)
  show (UndefinedVariable v)  = printf "Undefined variable %s" v
  show (InvalidMapping ls)    = printf "Incorrect mapping of variables. The following variables have multiple values: %s" (show ls)
