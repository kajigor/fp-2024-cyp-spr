{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.List (sort)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (pivot:rest) = smaller ++ [pivot] ++ bigger
    where smaller = quickSort [x | x <- rest, x <= pivot]
          bigger = quickSort [x | x <- rest, x > pivot]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr (\x -> (++) (f x)) []

positions :: (a -> Bool) -> [a] -> [Int]
positions predicate xs = map snd (filter (predicate . fst) (zip xs [0..]))

{- another implementation of positions:
positions :: (a -> Bool) -> [a] -> [Int]
positions p xs = foldr check [] [0..length xs - 1]
    where check i acc = if p (xs !! i) then i : acc else acc
-}

main = do
  runTests
  putStrLn "Done"

runTests = do
    runTestMap
    runTestConcatMap
    runTestQuickSort
    runTestPositions
  where
    describeFailure :: (Show a, Show b) => String -> String -> a -> b -> b -> IO ()
    describeFailure functionName errorMsg input exp actual =
      putStrLn $
      printf "Test for a function %s has failed:\n  %s\n  Input: %s\n  Expected: %s\n  But got: %s\n"
             functionName
             errorMsg
             (show input)
             (show exp)
             (show actual)

    runTestMap = do
        test "+2" (+2) [0..10]
        test "*3" (*3) ([] :: [Int])
        test "show" show [1.1, 2.2, 3.3, 4.4, 5.5]
        test "even . abs" (even . abs) [0, -1, 2, -3, 4, -5, 6]
      where
        test fRepr f xs =
          let act = map' f xs in
          let exp = map f xs in
          unless (act == exp) $ describeFailure "map'" (printf "map' (%s)" fRepr) xs exp act

    runTestConcatMap = do
        test ":[]" (:[]) ([] :: [Int])
        test ":[]" (:[]) [0..10]
        test "words" words ["a a a a", "b b b", "c"]
        test "replicate n n" (\n -> replicate n n) [0..10]
      where
        test fRepr f xs =
          let act = concatMap' f xs in
          let exp = concatMap f xs in
          unless (act == exp) $ describeFailure "concatMap'" (printf "concatMap' (%s)" fRepr) xs exp act

    runTestQuickSort = do 
        test [] 
        test [10, 9 .. 0] 
        test [ if even x then negate x else x | x <- [0..10] ]
      where 
        test xs = 
          let act = quickSort xs in 
          let exp = sort xs in 
          unless (act == exp) $ describeFailure "quickSort" "" xs exp act

    runTestPositions = do 
        test "even" even [] [] 
        test "even" even [0..10] [0,2..10] 
        test "==0" (==0) [0,1,0,0,1,1,0] [0,2,3,6]
        test "\\xs -> length xs `mod` 3 == 2" ((==2) . (`mod` 3) . length) [replicate x x | x <- [0..10]] [2,5..10]
      where  
        test fRepr f xs exp = 
          let act = positions f xs in 
          unless (act == exp) $ describeFailure "positions" (printf "positions (%s)" $ show xs) xs exp act
