module Tree where 

import Text.Read (readMaybe)

data Tree a 
  = E 
  | N (Tree a) a (Tree a) 
  deriving (Show, Eq, Read)

insert :: Ord a => a -> Tree a -> Tree a 
insert elem E = N E elem E
insert elem (N x el y)
  | el > elem = N (insert elem x) el y
  | el < elem = N x el (insert elem y)
  | otherwise = N x el y  

data ParseResult
  = IntList [Int]
  | IntTree (Tree Int)

readTree x = case readMaybe @(Tree Int) x of
        Nothing -> Left "failed"
        Just tr -> Right (IntTree tr)

parse :: String -> Either String ParseResult 
parse x = 
    case readMaybe @[Int] x of
        Nothing -> readTree x
        Just res -> Right (IntList res)

makeTree :: ParseResult -> Tree Int 
makeTree (IntTree x) = x
makeTree (IntList xs) = foldr insert E xs 

main :: IO () 
main = do 
  str <- getLine 
  let input = parse str
  case input of 
    Right pr -> print $ makeTree pr
    Left err -> putStrLn err  