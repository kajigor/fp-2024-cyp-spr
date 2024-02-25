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
    Right (xEval, yEval) -> if (xEval > 0) then Right (xEval ** yEval) else Left (Error "power used with non-positive base")


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

