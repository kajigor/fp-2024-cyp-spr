# HW02
## Deadline: 23:59 20.02.2024

1. Implement (not so quick) `quicksort :: [Int] -> [Int]`.
   * Naive recursive implementation is fine, don't try to make it run in `O(n*(log n))`

2. Implement `map' :: (a -> b) -> [a] -> [b]` using a fold. 
   * It should behave exactly like the `map` from `Prelude`. 

3. Implement `concatMap' :: (a -> [b]) -> [a] -> [b]` which is equivalent to concatenating all the lists produced by applying the function to every element of the input list. 
   * `concatMap words ["a a a a", "b b b", "c"] == ["a","a","a","a","b","b","b","c"]`

4. Implement `positions :: (a -> Bool) -> [a] -> [Int]` which finds all indices of elements of the input list for which the predicate holds.
   * `positions (==0) [0,1,0,0,1,1,0] == [0, 2, 3, 6]`

If you have enough energy, pick any of the functions and implement it in as many ways as you can come up with. 

Make sure that running main results in a single word `Done`. 