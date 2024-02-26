module Main where 

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either (isRight, fromRight)
import Data.Text.Internal.Read (IParser(P))

data Expr = Num Double
            | Plus Expr Expr
            | Minus Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Pow Expr Expr
            | Root Expr
            deriving (Eq)


instance Show Expr where 
  show (Num ex) = show ex
  show (Plus ex1 ex2) = "(" ++ show ex1 ++ " + " ++ show ex2 ++ ")"
  show (Minus ex1 ex2) = "(" ++ show ex1 ++ " - " ++ show ex2 ++ ")"
  show (Mult ex1 ex2) = "(" ++ show ex1 ++ " * " ++ show ex2 ++ ")"
  show (Div ex1 ex2) = "(" ++ show ex1 ++ " / " ++ show ex2 ++ ")"
  show (Pow ex1 ex2) = "(" ++ show ex1 ++ " ** " ++ show ex2 ++ ")" 
  show (Root ex) = "(" ++ "root " ++ show ex ++ ")"

data Error = DivideByZero String
            | NegativeFromRoot String 
            | PowNaN String 
            | ComplexError Error Error
            deriving (Eq)

instance Show Error where 
  show (DivideByZero msg) = "DivideByZero error " ++ show msg
  show (NegativeFromRoot msg) = "NegativeFromRoot error " ++ show msg
  show (PowNaN msg) = "PowNaN error " ++ show msg
  show (ComplexError error1 error2) = "ComplexErrorOf " ++ show error1 ++ " and " ++ show error2

andExprError :: (Expr -> Expr -> Expr) -> Either Error Expr -> Either Error Expr -> Either Error Expr -- priority for the errors, operands otherwise
andExprError _ (Left error1) (Left error2) = Left (ComplexError error1 error2)
andExprError _ (Right expr1) (Left error2) = Left error2
andExprError _ (Left error1) (Right expr2) = Left error1
andExprError oper (Right expr1) (Right expr2) = Right (oper expr1 expr2)


resolve :: Either Error Expr -> Either Error Expr
resolve (Left error) = Left error

resolve (Right (Num n)) = Right (Num n)

resolve (Right (Root (Num n)))
  | n < 0 = Left (NegativeFromRoot ("(root of " ++ show n ++ ")"))
  | otherwise = Right (Num (sqrt n))

resolve (Right (Plus (Num n1) (Num n2))) = Right $ Num (n1 + n2)
resolve (Right (Minus (Num n1) (Num n2))) = Right $ Num (n1 - n2)
resolve (Right (Mult (Num n1) (Num n2))) = Right $ Num (n1 * n2)
resolve (Right (Div (Num n1) (Num n2))) 
  | n2 == 0 = Left (DivideByZero ("(div of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Right $ Num (n1 / n2)

resolve (Right (Pow (Num n1) (Num n2)))
  | isNaN (n1 ** n2)= Left (PowNaN ("(pow of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Right (Num (n1 ** n2))

resolve (Right (Root expr)) = resolve $ either Left (Right . Root) (resolve (Right expr)) -- propagate error and recursive Root if correct
resolve (Right (Plus expr1 expr2)) = resolve $ andExprError Plus (resolve (Right expr1)) (resolve (Right expr2))
resolve (Right (Minus expr1 expr2)) = resolve $ andExprError Minus (resolve (Right expr1)) (resolve (Right expr2))
resolve (Right (Mult expr1 expr2)) = resolve $ andExprError Mult (resolve (Right expr1)) (resolve (Right expr2))
resolve (Right (Div expr1 expr2)) = resolve $ andExprError Div (resolve (Right expr1)) (resolve (Right expr2))
resolve (Right (Pow expr1 expr2)) = resolve $ andExprError Pow (resolve (Right expr1)) (resolve (Right expr2))


exprToNum :: Expr -> Double
exprToNum (Num n) = n

eval :: Expr -> Either Error Double
eval expr = either Left (Right . exprToNum) (resolve (Right expr)) 

cases :: [(Expr, Either Error Double)]
cases = [
    (Root $ Num 4, Right 2.0),
    (Plus (Root (Num 9)) (Minus (Num 2) (Num 7)), Right (-2.0)),
    (Div (Pow (Num 9) (Num 3)) (Pow (Num 4) (Num 2)), Right 45.5625),
    (Mult (Div (Num 3) (Num 150)) (Div (Num 200) (Num 10)),Right 0.4),
    -- (1 - 1^3/3!) * 6
    (Mult (Num 6) (Minus (Num 1) (Div (Pow (Num 1) (Num 3)) (Mult (Num 2) (Num 3))))
                      , Right 5.0),


    (Root (Num (-1)), Left $ NegativeFromRoot "(root of -1.0)"),
    (Div (Mult (Num 7.0) (Num 2.0)) (Minus (Num 4.0) (Num 4.0)), Left $ DivideByZero "(div of 14.0 and 0.0)"),
    (Div (Pow (Num (-1.0)) (Num 0.5)) (Root (Num 4.0)), Left $ PowNaN "(pow of -1.0 and 0.5)"),
    (Plus (Root (Num (-100500))) (Div (Num 100) (Num 0)), Left $ ComplexError (NegativeFromRoot "(root of -100500.0)") (DivideByZero "(div of 100.0 and 0.0)"))

  ]

test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s\n" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  