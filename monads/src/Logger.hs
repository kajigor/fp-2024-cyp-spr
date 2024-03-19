module Logger where

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where
  fmap :: (a -> b) -> Logger l a -> Logger l b
  fmap f (Logger l a) = Logger l (f a)

-- - 1. fmap id = id
-- fmap id (Logger l x) = Logger l (id x) = id (Logger l x)

-- 2. fmap (f . g) = fmap f . fmap g
-- fmap (f . g) (Logger l x) = Logger l (f (g x)) = fmap f Logger l (g x) = (fmap f . fmap g) (Logger l x)


-- Implement the instance and prove the laws
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure = Logger []
  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (<*>) (Logger l1 f) (Logger l2 a) = Logger (l1 ++ l2) (f a)

-- 1. pure id <*> v = v
-- pure id <*> (Logger l x) = (Logger [] id) <*> (Logger l x) = (Logger l x)

-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> u <*> v <*> w = (Logger [] (.)) <*> (Logger l1 x) <*> (Logger l2 y) <*> (Logger l3 z) = 
--     (Logger l1 ((.) x)) <*> (Logger l2 y) <*> (Logger l3 z) = 
--     (Logger (l1 ++ l2) (x . y)) <*> (Logger l3 z) = 
--     (Logger (l1 ++ l2 ++ l3) (x . y . z)) =
--     u <*> (v <*> w) = (Logger x l1) <*> (Logger (l2 ++ l3) (y . z)) = 
--     (Logger x l1) <*> ((Logger l2 y) <*> (Logger l3 z))

-- 3. pure f <*> pure x = pure (f x) 
-- pure f <*> pure g = (Logger [] f) <*> (Logger [] g) = Logger []] (f g) = pure (f g)

-- 4. u <*> pure x = pure ($ y) <*> u
-- pure ($ y) <*> (Logger l x) = Logger l (x y) = (Logger l x) <*> pure y


-- Implement the instance and prove the laws
instance Monad (Logger l) where
  (>>=) :: Logger l a -> (a -> Logger l b) -> Logger l b
  (>>=) (Logger l a) f = Logger l id <*> f a

-- 1. return a >>= k = k a
-- return a >>= k = Logger [] a >>= k = k a

-- 2. m >>= return = m
-- m >>= return = (Logger l x) >>= return = Logger l x

-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- m >>= (\x -> k x >>= h) =
-- = (Logger l a) >>= (\x -> k x >>= h) = Logger l id <*> (k a >>= h) =
-- (Logger l id <*> k a) >>= h = (Logger l a >>= k) >>= h = (m >>= k) >>= h


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

