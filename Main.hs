module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.Either (fromRight, fromLeft)
import Language.Haskell.TH (Exp)

data Expr = Const Double | Add Expr Expr | Subtract Expr Expr | Multiply Expr Expr | Divide Expr Expr | Power Expr Expr | Square Expr

instance Show Expr where
  show :: Expr -> String
  show (Const x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Subtract x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Multiply x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Divide x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Power x y) = "(" ++ show x ++ ")" ++  " ^ " ++ "(" ++ show y ++ ")"
  show (Square x) = "sqrt(" ++ show x ++ ")"


instance Eq Expr where
  (==) :: Expr -> Expr -> Bool
  (==) a b =
     let fst = eval a
         snd = eval b
     in fst == snd 


data Error = DivisorIsZero | SquareRootIsNegative | PowerBaseIsNegative deriving Eq
instance Show Error where
  show :: Error -> String
  show DivisorIsZero = "Divisor is equal zero"
  show SquareRootIsNegative  = "The root can't be negative"
  show PowerBaseIsNegative  = "Power base can't be negative"


performOp :: (Double -> Double -> Double) -> Either Error Double -> Either Error Double -> Either Error Double
performOp f a b = case a of
    Right fst -> case b of
      Right snd -> Right (f fst snd)
      err -> err
    err -> err


performOpWithCheck :: (Either Error Double -> Either Error Double -> Maybe Error) -> (Double -> Double -> Double) -> Either Error Double -> Either Error Double -> Either Error Double
performOpWithCheck pr f a b = case pr a b of
  Nothing -> performOp f a b
  Just err -> Left err

checkDivisorIsZero :: Either Error Double -> Either Error Double -> Maybe Error
checkDivisorIsZero _ b = case b of 
  Right 0.0 -> Just DivisorIsZero
  Right _ -> Nothing
  Left err -> Just err


checkBaseIsNegative :: Error -> Either Error Double -> Either Error Double -> Maybe Error
checkBaseIsNegative err (Right x) _
  | x < 0 = Just err
  | otherwise = Nothing
  
checkBaseIsNegative err a _ = Just (fromLeft err a)


eval :: Expr -> Either Error Double
eval expr = case expr of
  Const val -> Right val
  Add a b -> performOp (+) (eval a) (eval b)
  Subtract a b -> performOp (-) (eval a) (eval b)
  Multiply a b -> performOp (*) (eval a) (eval b)
  Divide a b -> performOpWithCheck checkDivisorIsZero (/) (eval a) (eval b)
  Power a b -> performOpWithCheck (checkBaseIsNegative PowerBaseIsNegative) (**) (eval a) (eval b)
  Square a -> performOpWithCheck (checkBaseIsNegative SquareRootIsNegative) (**) (eval a) (Right 0.5)

cases :: [(Expr, Either Error Double)]
cases = [
  (Const 1, Right 1.0),
  (Add (Const 1) (Const 1), Right 2.0),
  (Subtract (Const 1) (Const 1), Right 0.0),
  (Multiply (Const 2) (Const 3), Right 6.0),
  (Divide (Const 5) (Const 3), Right (5/3)),
  (Divide (Const 5) (Const 0.0), Left DivisorIsZero),
  (Square (Const 9), Right 3.0),
  (Square (Const (-3)), Left SquareRootIsNegative),
  (Power (Const 3) (Const 3), Right 27.0),
  (Power (Const (-3)) (Const 3), Left PowerBaseIsNegative),
  (Add (Multiply (Const 5) (Const 3)) (Divide (Subtract (Const 15) (Const 5)) (Const 5)), Right 17.0),
  (Square (Square (Square (Const 256.0))), Right 2.0),
  (Power (Const 2) (Power (Const 2) (Const 2)), Right 16)
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
  putStrLn "Done"
