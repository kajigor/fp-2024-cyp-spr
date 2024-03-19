module Eval where

import Expr
import qualified Data.Map.Strict as Map


checkErrors :: (Either Error a, Either Error a) -> Either Error (a, a)
checkErrors (Left x, _) = Left x
checkErrors (_, Left y) = Left y
checkErrors (Right x, Right y) = Right (x, y)

computeOrError :: (Floating a, Ord a) => (a -> a -> a) -> (a -> a -> Bool) -> String -> (Expr a, Expr a) -> Map.Map String a -> Either Error a
computeOrError f predicate errorMsg (expA, expB) li =
  case checkErrors (eval expA li, eval expB li) of
    Left err -> Left err
    Right (x, y) -> if predicate x y then Right (f x y) else Left (Error errorMsg)

alwaysCorrect :: a -> a -> Bool
alwaysCorrect _ _ = True

substValue :: String -> Map.Map String a -> Either Error a
substValue var vars =
  case Map.lookup var vars of
    Nothing -> Left (Error "Unknown variable: x")
    (Just x) -> Right x

eval :: (Floating a, Ord a) => Expr a -> Map.Map String a-> Either Error a
eval (Var x) = substValue x
eval (Expr x) = const (Right x)
eval (Sq x) = computeOrError (const sqrt) (\_ -> (<=) 0) "sqrt from negative taken" (x, x)
eval ((:+) x y) = computeOrError (+) alwaysCorrect "" (x, y)
eval ((:-) x y) = computeOrError (-) alwaysCorrect "" (x, y)
eval ((:*) x y) = computeOrError (*) alwaysCorrect "" (x, y)
eval ((:/) x y) = computeOrError (/) (\_ -> (/=) 0) "divided by zero" (x, y)
eval ((:^) x y) = computeOrError (**) (\a _ -> (<) 0 a) "power used with non-positive base" (x, y)
