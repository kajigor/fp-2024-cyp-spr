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


instance Show Expr where 
  show (Num ex) = show ex
  show (Plus ex1 ex2) = "(" ++ show ex1 ++ " + " ++ show ex2 ++ ")"
  show (Minus ex1 ex2) = "(" ++ show ex1 ++ " - " ++ show ex2 ++ ")"
  show (Mult ex1 ex2) = "(" ++ show ex1 ++ " * " ++ show ex2 ++ ")"
  show (Div ex1 ex2) = "(" ++ show ex1 ++ " / " ++ show ex2 ++ ")"
  show (Pow ex1 ex2) = "(" ++ show ex1 ++ " ** " ++ show ex2 ++ ")" 
  show (Root ex) = "(" ++ "root " ++ show ex ++ ")"


instance Eq Expr where 
  (==) (Num ex1) (Num ex2) = ex1 == ex2
  (==) (Root ex1) (Root ex2) = ex1 == ex2
  (==) (Plus a1 a2) (Plus b1 b2) = a1 == b1 && a2 == b2 
  (==) (Minus a1 a2) (Minus b1 b2) = a1 == b1 && a2 == b2 
  (==) (Mult a1 a2) (Mult b1 b2) = a1 == b1 && a2 == b2 
  (==) (Div a1 a2) (Div b1 b2) = a1 == b1 && a2 == b2
  (==) (Pow a1 a2) (Pow b1 b2) = a1 == b1 && a2 == b2 
  (==) _ _ = False

data Error = DivideByZero String
            | NegativeFromRoot String 
            | PowNaN String 
            | ComplexError Error Error

instance Show Error where 
  show (DivideByZero msg) = "DivideByZero error " ++ show msg
  show (NegativeFromRoot msg) = "NegativeFromRoot error " ++ show msg
  show (PowNaN msg) = "PowNaN error " ++ show msg
  show (ComplexError error1 error2) = "ComplexErrorOf " ++ show error1 ++ " and " ++ show error2
  

instance Eq Error where 
  (==) (DivideByZero msg1) (DivideByZero msg2) = msg1 == msg2
  (==) (NegativeFromRoot msg1) (NegativeFromRoot msg2) = msg1 == msg2 
  (==) (PowNaN msg1) (PowNaN msg2) = msg1 == msg2 
  (==) _ _ = False

andExprError :: (Expr -> Expr -> Expr) -> Either Expr Error -> Either Expr Error -> Either Expr Error -- priority for the errors, operands otherwise
andExprError _ (Right error1) (Right error2) = Right (ComplexError error1 error2)
andExprError _ (Left expr1) (Right error2) = Right error2
andExprError _ (Right error1) (Left expr2) = Right error1
andExprError oper (Left expr1) (Left expr2) = Left (oper expr1 expr2)


resolve :: Either Expr Error -> Either Expr Error
resolve (Right error) = Right error

resolve (Left (Num n)) = Left (Num n)

resolve (Left (Root (Num n)))
  | n < 0 = Right (NegativeFromRoot ("(root of " ++ show n ++ ")"))
  | otherwise = Left (Num (sqrt n))

resolve (Left (Plus (Num n1) (Num n2))) = Left $ Num (n1 + n2)
resolve (Left (Minus (Num n1) (Num n2))) = Left $ Num (n1 - n2)
resolve (Left (Mult (Num n1) (Num n2))) = Left $ Num (n1 * n2)
resolve (Left (Div (Num n1) (Num n2))) 
  | n2 == 0 = Right (DivideByZero ("(div of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Left $ Num (n1 / n2)

resolve (Left (Pow (Num n1) (Num n2)))
  | isNaN (n1 ** n2)= Right (PowNaN ("(pow of " ++ show n1 ++ " and " ++ show n2 ++ ")"))
  | otherwise = Left (Num (n1 ** n2))

resolve (Left (Root expr)) = resolve $ either (Left . Root) Right (resolve (Left expr)) -- propagate error and recursive Root if correct
resolve (Left (Plus expr1 expr2)) = resolve $ andExprError Plus (resolve (Left expr1)) (resolve (Left expr2))
resolve (Left (Minus expr1 expr2)) = resolve $ andExprError Minus (resolve (Left expr1)) (resolve (Left expr2))
resolve (Left (Mult expr1 expr2)) = resolve $ andExprError Mult (resolve (Left expr1)) (resolve (Left expr2))
resolve (Left (Div expr1 expr2)) = resolve $ andExprError Div (resolve (Left expr1)) (resolve (Left expr2))
resolve (Left (Pow expr1 expr2)) = resolve $ andExprError Pow (resolve (Left expr1)) (resolve (Left expr2))


exprToNum :: Expr -> Double
exprToNum (Num n) = n

eval :: Expr -> Either Error Double
eval expr = either (Right . exprToNum) Left (resolve (Left expr)) 

cases :: [(Expr, Either Error Double)]
cases = [
    (Root $ Num 1, Right 1.0),
    (Plus (Root (Num 9)) (Minus (Num 2) (Num 7)), Right (-2.0)),
    (Root (Num (-1)), Left $ NegativeFromRoot "(root of -1.0)"),
    (Div (Mult (Num 7.0) (Num 2.0)) (Minus (Num 4.0) (Num 4.0)), Left $ DivideByZero "(div of 14.0 and 0.0)"),
    (Div (Pow (Num (-1.0)) (Num 0.5)) (Root (Num 4.0)), Left $ PowNaN "(pow of -1.0 and 0.5)")
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
  