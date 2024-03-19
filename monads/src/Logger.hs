module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
    fmap f (Logger a b) = Logger a (f b)
    --fmap id (Logger a b) = Logger a (id b) = Logger a b
    --fmap (f . g) (Logger a b) = Logger a ((f . g) b) = Logger a (f (g b)) = fmap f (Logger a (g b)) = fmap f . fmap g (Logger a b)

-- Implement the instance and prove the laws
instance Applicative (Logger l) where
    pure = Logger []
    Logger la a <*> Logger lb b = Logger (la ++ lb) (a b)
    --pure id <*> v = Logger [] id <*> Logger lv av = Logger ([] ++ lv) (id av) = Logger lv av = v
    --pure (.) <*> u <*> v <*> w = Logger [] (.) <*> Logger lu au <*> Logger lv av <*> Logger lw aw = 
    --  Logger ([] ++ lu) ((.) au) <*> Logger lv av <*> Logger lw aw = Logger (lu ++ lv ++ lw) ((au . av) aw) = 
    --  Logger (lu ++ lv ++ lw) (au (av aw)) = Logger lu au <*> (Logger (lv ++ lw) (av aw)) = u <*> v <*> w
    --pure f <*> pure x = Logger [] f <*> Logger [] x = Logger [] (f x) = pure (f x)
    --u <*> pure y = Logger lu au <*> Logger [] y = Logger lu (($ y) au) = Logger [] ($ y) <*> Logger lu au = pure ($ y) <*> u 

-- Implement the instance and prove the laws
instance Monad (Logger l) where
    Logger l x >>= f = Logger l id <*> f x
    --pure x >>= f = Logger [] x >>= f = Logger [] id <*> f x = f x
    --m >>= pure = Logger lm am >>= pure = Logger lm id <*> Logger [] am = Logger lm am = m
    --m >>= f >>= g = Logger lm am >>= f >>= g = Logger lm id <*> f am >>= g,
    --  m >>= (\x -> f x >>= g) = Logger lm am >>= (\x -> f x >>= g) = Logger lm id <*> (f am >>= g), which is the same as before.


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

          