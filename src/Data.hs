module Data where

-- Type for unary operator
data Operator1 = Sqrt | Neg deriving (Eq)

instance Show Operator1 where
  show Sqrt = "√"
  show Neg = "-"


-- Type for binary operator
data Operator2 = Div | Mul | Plus | Min | In deriving (Eq)

instance Show Operator2 where
  show Div = "/"
  show Mul = "*"
  show Plus = "+"
  show Min = "-"
  show In = "^"


-- Type for expression
data Expr a = Arg a | Var String | Marg Operator1 (Expr a) | CE (Expr a) Operator2 (Expr a)

instance Show a => Show (Expr a) where
  show (Arg value) = show value
  show (Var variable) = show variable
  show (Marg op value)
    | op == Neg = "(" ++ (show op) ++ (show value) ++ ")"
    | op == Sqrt = (show op) ++ "(" ++ (show value) ++ ")"
  show (CE expr1 op expr2) = (show expr1) ++ (show op) ++ (show expr2)

instance Eq a => Eq (Expr a) where
  Arg value1 == Arg value2 = value1 == value2
  Var variable1 == Var variable2 = variable1 == variable2
  Marg op1 value1 == Marg op2 value2 =
   op1 == op2 && value1 == value2
  CE expr11 op1 expr12 == CE expr21 op2 expr22 =
   expr11 == expr21 && op1 == op2 && expr12 == expr22
  _ == _ = False

instance Num a => Num (Expr a) where
  expr1 + expr2 = CE expr1 Plus expr2
  expr1 * expr2 = CE expr1 Mul expr2
  fromInteger value = Arg (fromInteger value)
  negate expr = Marg Neg expr


data Error a =
  OutOfPossibleValuesError Operator1 a
  | ZeroDivisionError a
  | IncorrectDegreeOfValue a
  | VariableDoesNotExist String

instance Show a => Show (Error a) where
  show (OutOfPossibleValuesError op value) = "OutOfPossibleValuesError: operator "
   ++ (show op) ++ " can not handle expression " ++ (show value)
  show (ZeroDivisionError chislitel) = "ZeroDivisionError: numerator "
   ++ (show chislitel) ++ " can not be divided by 0"
  show (IncorrectDegreeOfValue value) = "IncorrectDegreeOfValue: value 0 in "
   ++ (show value) ++ " degree does not exist"
  show (VariableDoesNotExist variable) = "VariableDoesNotExist: variable "
   ++ variable ++ " is not defined"

instance Eq a => Eq (Error a) where
  OutOfPossibleValuesError op1 value1 == OutOfPossibleValuesError op2 value2 =
   op1 == op2 && value1 == value2
  ZeroDivisionError chislitel1 == ZeroDivisionError chislitel2 =
   chislitel1 == chislitel2
  IncorrectDegreeOfValue value1 == IncorrectDegreeOfValue value2 = value1 == value2
  VariableDoesNotExist var1 == VariableDoesNotExist var2 = var1 == var2
  _ == _ = False
