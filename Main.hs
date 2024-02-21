module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data Expr

instance Show Expr where 
  show = undefined 

instance Eq Expr where 
  (==) = undefined 

data Error 

instance Show Error where 
  show = undefined

instance Eq Error where 
  (==) = undefined 

eval :: Expr -> Either Error Double 
eval = undefined 

cases :: [(Expr, Either Error Double)]
cases = undefined 

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
  