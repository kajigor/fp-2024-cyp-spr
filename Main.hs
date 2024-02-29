module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data BinOperator = Plus | Minus | Multiply | Divide | Power deriving (Eq)
instance Show BinOperator where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Divide = "/"
  show Power = "^"

data Expr = Number Double | SquareRoot Expr | BinExpr BinOperator Expr Expr deriving Eq

instance Show Expr where 
  show (Number n) = show n 
  show (SquareRoot exp) = printf "(square root of %s)"  (show exp)
  show (BinExpr binOp leftExp rightExp) = printf "(%s %s %s)" (show leftExp) (show binOp) (show rightExp)
   

data Error = DivisionByZero | SquareRootOfNegative deriving Eq

instance Show Error where 
  show (DivisionByZero) = printf "Division by zero"
  show (SquareRootOfNegative) = printf "Square root of negative number"


eval :: Expr -> Either Error Double 
eval (Number n) = Right n
eval (SquareRoot exp) = evalSquareRootExp (eval exp)
eval (BinExpr binOp exp1 exp2) = evalBinExp binOp (eval exp1) (eval exp2) 

evalSquareRootExp :: Either Error Double -> Either Error Double
evalSquareRootExp (Left error) = Left error
evalSquareRootExp (Right n) = if n < 0 then Left SquareRootOfNegative else Right (sqrt n)

evalBinExp :: BinOperator -> Either Error Double ->  Either Error Double -> Either Error Double
evalBinExp _ (Left err) _ = Left err
evalBinExp _ _ (Left err) = Left err
evalBinExp Plus (Right n1) (Right n2) = Right (n1 + n2)
evalBinExp Minus (Right n1) (Right n2) = Right (n1 - n2)
evalBinExp Multiply (Right n1) (Right n2) = Right (n1 * n2)
evalBinExp Power (Right n1) (Right n2) = Right (n1 ** n2)
evalBinExp Divide (Right n1) (Right n2) = evalDivideExp n1 n2

evalDivideExp :: Double ->  Double -> Either Error Double
evalDivideExp _ 0 = Left DivisionByZero
evalDivideExp n1 n2 = Right (n1 / n2)


cases :: [(Expr, Either Error Double)]
cases = [(Number 12, Right 12), 
  (SquareRoot (Number 4), Right 2), 
  (BinExpr Plus (Number 30) (Number 20), Right 50),
  (BinExpr Minus (Number 100) (Number 115.5), Right (-15.5)),
  (BinExpr Multiply (Number 50) (Number 7.5), Right 375),
  (BinExpr Power (Number 2) (Number 4), Right 16),
  (BinExpr Divide (Number 21) (Number 2), Right 10.5),
  (BinExpr Divide (Number 37) (Number 0), Left DivisionByZero),
  (SquareRoot (Number (-24)), Left SquareRootOfNegative),
  (BinExpr Multiply (BinExpr Minus (Number 12) (Number 4)) (SquareRoot (BinExpr Power (Number 20) (Number 2))), Right 160),
  (BinExpr Plus (BinExpr Multiply (Number 12) (Number 4)) (BinExpr Power (Number 2) (Number 5)), Right 80),
  (BinExpr Divide (BinExpr Multiply (Number 150) (Number 50)) (BinExpr Power (Number 0) (Number 2)), Left DivisionByZero),
  (BinExpr Plus (SquareRoot (Number (-345))) (BinExpr Power (Number 100) (Number 4)), Left SquareRootOfNegative)
  ] 


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

  