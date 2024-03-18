module Error where
import Expr (Expr (..))

import Text.Printf (printf)

data Error = DivisionByZero | SquareRootOfNegative | NoSuchVariable deriving Eq 

instance Show Error where 
  show (DivisionByZero) = printf "Division by zero"
  show (SquareRootOfNegative) = printf "Square root of negative number"
  show (NoSuchVariable) = printf "No such variable"
