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

eval :: Expr -> Either Error Double
eval (Expr x) = Right x
eval (Sq x) =
  case eval x of
    Left err -> Left err
    Right exp -> if (exp >= 0) then Right (sqrt exp) else Left (Error "sqrt from negative taken")
eval ((:+) x y) =
  case checkErrors (eval x, eval y) of
    Left err -> Left err
    Right (xEval, yEval) -> Right (xEval + yEval)
eval ((:-) x y) =
  case checkErrors (eval x, eval y) of
    Left err -> Left err
    Right (xEval, yEval) -> Right (xEval - yEval)
eval ((:*) x y) =
  case checkErrors (eval x, eval y) of
    Left err -> Left err
    Right (xEval, yEval) -> Right (xEval * yEval)
eval ((:/) x y) =
  case checkErrors (eval x, eval y) of
    Left err -> Left err
    Right (xEval, yEval) -> if (yEval /= 0) then Right (xEval / yEval) else Left (Error "divided by zero")
eval ((:^) x y) =
  case checkErrors (eval x, eval y) of
    Left err -> Left err
    Right (xEval, yEval) -> if (yEval > 0) then Right (xEval ** yEval) else Left (Error "power used with non-positive base")


cases :: [(Expr, Either Error Double)]
cases = []

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

