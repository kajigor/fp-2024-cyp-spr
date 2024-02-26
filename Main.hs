module Main where

import Text.Printf (printf)
import Control.Monad (unless)

data Expr = Expr Double | Sq Expr | (:+) Expr Expr | (:-) Expr Expr | (:*) Expr Expr | (:/) Expr Expr | (:^) Expr Expr
  deriving Eq

instance Show Expr where
  show (Expr x) = show x
  show (Sq x) = ("sqrt(" ++ (show x ++ ")"))
  show ((:+) x y) = '(':show x ++ (") + (" ++ (show y ++ ")"))
  show ((:-) x y) = '(':show x ++ (") - (" ++ (show y ++ ")"))
  show ((:*) x y) = '(':show x ++ (") * (" ++ (show y ++ ")"))
  show ((:/) x y) = '(':show x ++ (") / (" ++ (show y ++ ")"))
  show ((:^) x y) = '(':show x ++ (") ^ (" ++ (show y ++ ")"))


data Error = Error String
  deriving Eq

instance Show Error where
  show (Error x) = "Error: " ++ show x

checkErrors :: (Either Error Double, Either Error Double) -> Either Error (Double, Double)
checkErrors ((Left x), _) = Left x
checkErrors (_, (Left y)) = Left y
checkErrors (Right x, Right y) = Right (x, y)

computeOrError :: (Double -> Double -> Double) -> (Double -> Double -> Bool) -> String -> (Expr, Expr) -> Either Error Double
computeOrError f predicate errorMsg (expA, expB) =
  case checkErrors (eval expA, eval expB) of
    Left err -> Left err
    Right (x, y) -> if predicate x y then Right (f x y) else Left (Error errorMsg)

alwaysCorrect :: Double -> Double -> Bool
alwaysCorrect = \a b -> True

eval :: Expr -> Either Error Double
eval (Expr x) = Right x
eval (Sq x) = computeOrError (\val -> sqrt) (\val -> (<=) 0) "sqrt from negative taken" (x, x)
eval ((:+) x y) = computeOrError (+) (alwaysCorrect) "" (x, y)
eval ((:-) x y) = computeOrError (-) (alwaysCorrect) "" (x, y)
eval ((:*) x y) = computeOrError (*) (alwaysCorrect) "" (x, y)
eval ((:/) x y) = computeOrError (/) (\a -> (/=) 0) "divided by zero" (x, y)
eval ((:^) x y) = computeOrError (**) (\a b -> (<) 0 a) "power used with non-positive base" (x, y)

cases :: [(Expr, Either Error Double)]
cases = [(Expr 1.0, Right 1.0) --base
  ,(Sq (Expr 4.0), Right 2.0) --Sq
  ,(Expr 1.0 :+ Sq (Expr (-4.0)), Left (Error "sqrt from negative taken")) --base Error
  ,((Expr 2.0 :^ Expr 3.0) :+ ((Expr 2.0 :* Expr 3.0) :- (Expr 4.0 :/ Expr 1.0)), Right 10.0) --base operations
  ,((Expr 1.0 :/ Expr 0.0) :+ Sq (Expr (-1.0)), Left (Error "divided by zero")) --multiple errors
  ,(Expr (-1.0) :^ Expr 3.0, Left (Error "power used with non-positive base")) --negative base error
  ,(Expr (0.0) :^ Expr (-3.0), Left (Error "power used with non-positive base"))] --zero base error

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

