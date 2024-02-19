{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Main where

import Text.Printf (printf)
import Control.Monad (unless)
import Data.List (sort)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort xs =
  let anchor = head xs
      left = filter (< anchor) xs
      mid = filter (== anchor) xs
      right = filter (> anchor) xs
   in quickSort left ++ mid ++ quickSort right

map' :: (a -> b) -> [a] -> [b]
map' transform = foldr ((:) . transform) []

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' transform = foldr ((++) . transform) []

concatMapRe :: (a -> [b]) -> [a] -> [b]
concatMapRe _ [] = []
concatMapRe transform (x:xs) = transform x ++ concatMapRe transform xs

positions :: (a -> Bool) -> [a] -> [Int]
positions predicate xs = map fst $ filter (predicate . snd) $ zip [0..] xs

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
