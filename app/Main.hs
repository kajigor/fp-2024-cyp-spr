module Main (main) where

import Lib
import Logger (factLog)

main :: IO ()
main = putStr (show (factLog 5))
