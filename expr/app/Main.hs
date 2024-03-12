module Main ( main ) where

import Expr ( Expr (..) )
import Simplify ( simplify )
import Text.Printf ( printf )

printSimplified :: Expr Double -> IO () 
printSimplified expr = do 
  putStrLn $ printf "Expr:\t\t%s\nSimplified:\t%s\n\n" (show expr) (show $ simplify expr)

main :: IO ()
main = do 
  printSimplified $ 0
  printSimplified $ 0 * 1 
  printSimplified $ 1 * 0 
  printSimplified $ 0 - 1
  printSimplified $ 1 - 0
  printSimplified $ 1 - 1  
  printSimplified $ 0 * 1 + 2 * 3 - (Var "v") * 0 - 2 * 3 