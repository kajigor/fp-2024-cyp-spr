module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data BinOp = Plus | Minus | Mult | Div | Exp deriving (Eq)
instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show Exp   = "^"

data FunOpUn = Sqrt deriving (Eq)
instance Show FunOpUn where
  show Sqrt = "sqrt"

data Expr = Number Double | Binary BinOp Expr Expr | UnaryFun FunOpUn Expr deriving (Eq)
instance Show Expr where
  show (Number x) = show x
  show (Binary op lhs rhs) = '(' : show lhs ++ show op ++ show rhs ++ ")"
  show (UnaryFun fun arg) = show fun ++ "(" ++ show arg ++ ")"

data Error = DivByZero | NegExponent | NegSqrt deriving (Eq)

instance Show Error where 
  show DivByZero = "Division by zero"
  show NegExponent = "Trying to exponentiate negative value"
  show NegSqrt = "Trying to take a square root of a negative number"

getFun :: BinOp -> (Double -> Double -> Double)
getFun Plus   = (+)
getFun Minus  = (-)
getFun Mult   = (*)
getFun Div    = (/)
getFun Exp    = (**)

evalBinOp :: BinOp -> Either Error Double -> Either Error Double -> Either Error Double
evalBinOp _ (Left e) _ = Left e
evalBinOp _ _ (Left e) = Left e
evalBinOp op@Div (Right lhs) (Right rhs)  = if (rhs == 0) then Left DivByZero else Right $ getFun op lhs rhs
evalBinOp op@Exp (Right lhs) (Right rhs)  = if (lhs < 0)  then Left NegExponent else Right $ getFun op lhs rhs
evalBinOp op (Right lhs) (Right rhs)      = Right $ getFun op lhs rhs

evalFunUn :: FunOpUn -> Either Error Double -> Either Error Double
evalFunUn _ (Left e) = Left e
evalFunUn Sqrt (Right arg) = if (arg < 0) then Left NegSqrt else Right $ sqrt arg

eval :: Expr -> Either Error Double 
eval (Number x) = Right x
eval (Binary op lhs rhs) = evalBinOp op (eval lhs) (eval rhs)
eval (UnaryFun fun arg)  = evalFunUn fun (eval arg)

cases :: [(Expr, Either Error Double)]
cases = [(Number 1.0, Right 1.0),
         (Number 0.0, Right 0.0),
         (Number 3, Right 3.0),
         (Number 1e300, Right 1e300),
         (Binary Plus (Number 2) (Number 5), Right 7.0),
         (Binary Minus (Number 5) (Number 7), Right (-2.0)),
         (Binary Mult (Number 7.0) (Number 3), Right 21.0),
         (Binary Div (Number 27) (Number 3), Right 9.0),
         (Binary Div (Number 3) (Number 2), Right 1.5),
         (Binary Div (Number 4) (Number 0), Left DivByZero),
         (Binary Exp (Number 2) (Number 4), Right 16.0),
         (Binary Exp (Number (-2.7)) (Number 1.5), Left NegExponent),
         (UnaryFun Sqrt (Number 81.0), Right 9.0),
         (UnaryFun Sqrt (Number (-1.0)), Left NegSqrt),
         (Binary Plus (Number 2) (Binary Mult (Number 2) (Number 2)), Right 6.0),
         (Binary Div (Number 4) (Binary Minus (Number 2) (Number 2)), Left DivByZero)]

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  putStrLn "Done"
  
