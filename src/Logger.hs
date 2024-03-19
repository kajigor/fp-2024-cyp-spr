module Logger where

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws (I proved Monad with 3 rules, without <*> -> no need to comment it)
instance Functor (Logger l) where
  fmap f (Logger list value) = Logger list (f value)


-- Implement the instance and prove the laws (I proved Monad with 3 rules, without <*> -> no need to comment it)
instance Applicative (Logger l) where
  pure value = Logger [] value
  (Logger logs1 function) <*> (Logger logs2 item) = Logger (logs1 ++ logs2) (function item)


-- Implement the instance and prove the laws
instance Monad (Logger l) where
  Logger logs result >>= f = Logger logs id <*> (f result)


-- Writes a single log message.
-- Can be easily bound together with other logging computations.
writeLog :: l -> Logger l ()
writeLog l = Logger [l] ()

-- Logs every intermediate result
-- ghci> factLog 5
-- Logger [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120)] 120
factLog :: Int -> Logger (Int, Int) Int
factLog n
  | n <= 0 = do
      let res = 1
      writeLog (n, res)
      return res
  | otherwise = do
      prev <- factLog (n - 1)
      let res = n * prev
      writeLog (n, res)
      return res


-- Left identity:
-- return a >>= k = k a

-- return x >>= f
-- Logger _ x >>= f
-- f x


-- Right identity:
-- m >>= return = m

-- Logger _ a >>= return
-- return a
-- m a


-- Associativity:
-- m >>= (\x -> k x >>= h) = (m >>= k) >>= g

-- Logger _ a >>= (\x -> k x >>= h) = k a >>= h
-- (Logger _ a >>= k) >>= h === k a >>= h
