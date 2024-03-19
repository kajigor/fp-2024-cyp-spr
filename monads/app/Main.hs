module Main (main) where

import IO (helloUser, helloUser', myGetLine)

main :: IO ()
main = do 
  helloUser
  helloUser'
  return ()


