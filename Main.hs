module Main where 

import Text.Printf (printf)
import Control.Monad (unless)


data Expr = Num Double
          | Sqrt Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          deriving (Eq)


instance Show Expr where 
  show (Num x) = show x
  show (Sqrt expr) = "sqrt(" ++ show expr ++ ")"
  show (Add expr1 expr2) = "(" ++ show expr1 ++ " + " ++ show expr2 ++ ")"
  show (Sub expr1 expr2) = "(" ++ show expr1 ++ " - " ++ show expr2 ++ ")"
  show (Mul expr1 expr2) = "(" ++ show expr1 ++ " * " ++ show expr2 ++ ")"
  show (Div expr1 expr2) = "(" ++ show expr1 ++ " / " ++ show expr2 ++ ")"
  show (Pow expr1 expr2) = "(" ++ show expr1 ++ " ^ " ++ show expr2 ++ ")"


data Error = NegativeSquareRoot 
          | DivisionByZero 
          | ZeroToZeroPower
          deriving (Eq, Show)


eval :: Expr -> Either Error Double 
eval (Num x) = Right x
eval (Sqrt expr) = case eval expr of
                     Left err -> Left err
                     Right val -> if val >= 0 then Right (sqrt val) else Left NegativeSquareRoot
eval (Add expr1 expr2) = binaryOperation (+) expr1 expr2
eval (Sub expr1 expr2) = binaryOperation (-) expr1 expr2
eval (Mul expr1 expr2) = binaryOperation (*) expr1 expr2
eval (Div expr1 expr2) = case eval expr2 of
                           Left err -> Left err
                           Right val2 -> if val2 /= 0 then binaryOperation (/) expr1 expr2 else Left DivisionByZero
eval (Pow expr1 expr2) = case (eval expr1, eval expr2) of
                           (Right 0, Right 0) -> Left ZeroToZeroPower
                           (Right val1, Right val2) -> Right (val1 ** val2)
                           (Left err, _) -> Left err
                           (_, Left err) -> Left err


binaryOperation :: (Double -> Double -> Double) -> Expr -> Expr -> Either Error Double
binaryOperation op expr1 expr2 = case (eval expr1, eval expr2) of
                                    (Right val1, Right val2) -> Right (val1 `op` val2)
                                    (Left err, _) -> Left err
                                    (_, Left err) -> Left err


cases :: [(Expr, Either Error Double)]
cases = [ (Num 566, Right 566)
        , (Sqrt (Num 4), Right 2)
        , (Sqrt (Num (-1)), Left NegativeSquareRoot)
        , (Add (Num 2) (Num 3), Right 5)
        , (Sub (Num 7) (Num 2), Right 5)
        , (Mul (Num 2) (Num 3), Right 6)
        , (Div (Num 10) (Num 2), Right 5)
        , (Div (Num 5) (Num 0), Left DivisionByZero)
        , (Div (Num (-10)) (Num (-1)), Right 10)
        , (Pow (Num 2) (Num 3), Right 8)
        , (Pow (Num 0) (Num 0), Left ZeroToZeroPower)
        , (Pow (Num 2) (Num (-1)), Right 0.5)
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
  