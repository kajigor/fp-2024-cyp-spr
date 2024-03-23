{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.List (sort)

-- 1. Implement (not so quick) `quicksort :: [Int] -> [Int]`.
--   * Naive recursive implementation is fine, don't try to make it run in `O(n*(log n))`

quickSort :: [Int] -> [Int]
quickSort [] = []  -- Base case: Empty list, return an empty list
quickSort (x:xs) = lessThanX ++ [x] ++ greaterThanX
  where
    lessThanX = quickSort [y | y <- xs, y <= x]
    greaterThanX = quickSort [y | y <- xs, y > x]

-- 2. Implement `map' :: (a -> b) -> [a] -> [b]` using a fold.
--   * It should behave exactly like the `map` from `Prelude`.

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- 3. Implement `concatMap' :: (a -> [b]) -> [a] -> [b]` which is equivalent to concatenating all the lists produced by applying the function to every element of the input list.
--   * `concatMap words ["a a a a", "b b b", "c"] == ["a","a","a","a","b","b","b","c"]`

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = concat [f x | x <- xs]

--4. Implement `positions :: (a -> Bool) -> [a] -> [Int]` which finds all indices of elements of the input list for which the predicate holds.
--   * `positions (==0) [0,1,0,0,1,1,0] == [0, 2, 3, 6]`


positions :: (a -> Bool) -> [a] -> [Int]
positions f xs = [i | (i, x) <- zip [0..] xs, f x]

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
