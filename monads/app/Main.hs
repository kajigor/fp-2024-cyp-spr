module Main (main) where

import List
import IO (helloUser, helloUser')

flatten Nil = Nil
flatten (Cons Nil ls) = flatten ls
flatten (Cons (Cons x xs) vs) = Cons x (flatten (Cons xs vs))

ls1 = Cons 1 (Cons 2 Nil) 
ls2 = Cons 3 (Cons 4 Nil)

f x
  | x > 0 = Cons x (f (x-1))
  | otherwise = Nil

main :: IO ()
main = do
  putStrLn $ show $ pure (*) <*> Cons 1 (Cons 2 Nil) <*> (Cons 3 (Cons 4 Nil))
  putStrLn $ show $ Cons 1 (Cons 4 (Cons 3 Nil)) >>= f >>= f