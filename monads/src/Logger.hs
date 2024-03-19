module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
    fmap :: (a -> b) -> Logger l a -> Logger l b
    fmap f (Logger l a) = Logger l (f a)

{-
  1) fmap id == id
  fmap f (Logger l a) == Logger l (id a) == Logger l a
  
  2) fmap (f . g)  ==  fmap f . fmap g
  fmap (f . g) (Logger l a) == Logger l ((f . g) a) ==  Logger l (f (g a)) ==
  == fmap f (Logger l (g a)) == (fmap f . fmap g) (Logger l a)
-}

-- Implement the instance and prove the laws
instance Applicative (Logger l) where 
    pure :: a -> Logger l a
    pure = Logger [] 

    (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
    (<*>) (Logger lgf f) (Logger lg a) = Logger (lgf ++ lg) (f a)

{-
  1) pure id <*> v = v
  pure id <*> (Logger l a) == Logger [] id <*> (Logger l a) == Logger (l ++ []) (id a) == Logger l a

  2) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  pure (.) <*> u <*> v <*> w == pure (.) <*> (Logger l1 a1) <*> (Logger l2 a2) <*> (Logger l3 a3) ==
  == Logger (l1 + l2) (a1 . a2) <*> Logger l3 a3 == Logger (l1 + l2 + l3) (a1 . a2 . a3) ==
  == Logger l1 a1 <*> Logger (l2 + l3) (a2 . a3) == Logger l1 a1 <*> (Logger l2 a2 <*> Logger l3 a3) 

  3) pure f <*> pure x = pure (f x)
  pure f <*> pure x == Logger [] f <*> Logger [] x == Logger [] (f x) == pure (f x)

  4) u <*> pure y = pure ($ y) <*> u
  u <*> pure y == u <*> Logger [] y == Logger [] (u y) == Logger [] ($ y u) ==
  ==  Logger [] ($ y) <*> u == pure ($ y) <*> u
-}

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
    (>>=) :: Logger l a -> (a -> Logger l b) -> Logger l b
    (>>=) (Logger l a) f = appendLog l (f a)
        where appendLog :: [l] -> Logger l a -> Logger l a
              appendLog l1 (Logger l2 a) = Logger (l1 ++ l2) a

{-
  1) return a >>= k = k a
  return a >>= k == Logger [] a >>= k == k a 

  2) m >>= return = m
  Logger lm m >>= return == appendLog lm (return m) == Logger lm m  

  3) m >>= (\x -> k x >>= h) = (m >>= k) >>= h
  Logger lm m >>= (\x -> k x >>= h) == Logger (lm ++ lk) (k m) >>= h ==
  == (Logger lm m >>= Logger lk k) >> h
-}

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

          