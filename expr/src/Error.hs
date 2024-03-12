module Error where 

import Expr ( Expr (..) ) 

data Error = DivideByZero String
            | NegativeFromRoot String
            | PowNaN String
            | VariableNotApplied String
            | ComplexError Error Error
            deriving (Eq)

instance Show Error where
  show (DivideByZero msg) = "DivideByZero error " ++ show msg
  show (NegativeFromRoot msg) = "NegativeFromRoot error " ++ show msg
  show (PowNaN msg) = "PowNaN error " ++ show msg
  show (VariableNotApplied msg) = "VariableNotApplied error " ++ show msg
  show (ComplexError error1 error2) = "ComplexErrorOf " ++ show error1 ++ " and " ++ show error2