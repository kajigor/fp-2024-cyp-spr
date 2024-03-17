{-# LANGUAGE InstanceSigs #-}
module Logger where

data Logger l a = Logger [l] a deriving (Show, Eq)

{-
Proof:
1) Identity: fmap id x === id x
fmap id (Logger log x) === Logger log (id x) === Logger log x  === id (Logger log x)
2) Composition: fmap (f . g) x === fmap f . fmap g 
fmap (f . g) (Logger log x) === Logger log ((f . g) x) 
(fmap f . fmap g) (Logger log x) === fmap f (Logger log (g x)) === Logger log ((f . g) x)
-}
instance Functor (Logger l) where
  fmap :: (a -> b) -> Logger l a -> Logger l b
  fmap f (Logger log val) = Logger log (f val)

{-
Proof:
1) Identity: pure id <*> v === Logger [] id <*> Logger log val === Logger log (id val) === Logger log val === id (Logger log val)
2) Homomorphism: pure f <*> pure x === Logger [] f <*> Logger [] val === Logger [] (f val) === pure (f val)
3) Interchange: u <*> pure y === pure ($ y) <*> u
u <*> pure y === Logger log f <*> Logger [] y === Logger log (f y)
pure ($ y) <*> u === Logger [] ($ y) <*> Logger log f === Logger log ($ y f) === Logger log (f y)
4) Composition: pure (.) <*> u <*> v <*> w === u <*> (v <*> w)
pure (.) <*> Logger log f <*> v <*> w === Logger log (f .) <*> Logger log' g <*> w === Logger (log + log') (f . g) <*> Logger log'' val === Logger (log + log' + log'') ((f . g) val)
u <*> (Logger log' g <*> Logger log'' val) === Logger log f <*> Logger (log' + log'') (f val) === Logger (log + log' + log'') ((f . g) val)
-}
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure = Logger []
  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (<*>) (Logger log f) (Logger log' a) = Logger (log ++ log') (f a) 

{-
1) Left identity: return x >>= k === Logger [] x >>= k === Logger log' newVal === k x (k x = Logger log' newVal)
2) Right identity: m >>= return === Logger log f >>= return === Logger (log + []) f === Logger log f === m
3) Associativity: m >>= (\x -> k x >>= h) === (m >>= k) >>= h
m >>= (\x -> f x >>= h) >>= Logger log val >>= (\x -> f x >>= h) === Logger (log + log') (f val) >>= h === Logger (log + log' + log'') ((f . g) val) ???? is it possible to write associative? I guess we can only ignore loggers
(m >>= k) >>= h === (Logger log val >>= k) >>= h === (log + log') (g val) >>= h === Logger (log + log' + log'') ((f . g) val)
-}
instance Monad (Logger l) where
  return :: a -> Logger l a
  return = pure
  (>>=) :: Logger l a -> (a -> Logger l b) -> Logger l b
  (>>=) (Logger log val) f = case f val of
    Logger log' newVal -> Logger (log ++ log') newVal

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

