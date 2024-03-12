module Eval where

import Expr

checkErrors :: (Either Error a, Either Error a) -> Either Error (a, a)
checkErrors (Left x, _) = Left x
checkErrors (_, Left y) = Left y
checkErrors (Right x, Right y) = Right (x, y)

computeOrError :: (Floating a, Ord a) => (a -> a -> a) -> (a -> a -> Bool) -> String -> (Expr a, Expr a) -> [(String, a)]-> Either Error a
computeOrError f predicate errorMsg (expA, expB) li =
  case checkErrors (eval expA li, eval expB li) of
    Left err -> Left err
    Right (x, y) -> if predicate x y then Right (f x y) else Left (Error errorMsg)

alwaysCorrect :: a -> a -> Bool
alwaysCorrect _ _ = True

substValue :: String -> [(String, a)] -> Either Error a
substValue var vars =
  let checkUniqueVar li =
        case li of
          [] -> Left (Error ("Unknown variable: " ++ var))
          (h:t) -> if null t then Right (snd h) else Left (Error ("Non-unique variable value: " ++ var))
  in checkUniqueVar (filter (\(x, _) -> var == x) vars)

eval :: (Floating a, Ord a) => Expr a -> [(String, a)]-> Either Error a
eval (Var x) = substValue x
eval (Expr x) = const (Right x)
eval (Sq x) = computeOrError (const sqrt) (\_ -> (<=) 0) "sqrt from negative taken" (x, x)
eval ((:+) x y) = computeOrError (+) alwaysCorrect "" (x, y)
eval ((:-) x y) = computeOrError (-) alwaysCorrect "" (x, y)
eval ((:*) x y) = computeOrError (*) alwaysCorrect "" (x, y)
eval ((:/) x y) = computeOrError (/) (\_ -> (/=) 0) "divided by zero" (x, y)
eval ((:^) x y) = computeOrError (**) (\a _ -> (<) 0 a) "power used with non-positive base" (x, y)
