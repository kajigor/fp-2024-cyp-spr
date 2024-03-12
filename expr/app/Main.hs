module Main (main) where

import Expr

main :: IO ()
main = do
  mapM_ (uncurry test) cases
  mapM_ (uncurry testSimplify) simplifyCases
