module Error where 

data Error = DivideByZero | SquareRootOfNegative | IncorrectVariableName
  deriving (Eq, Show)