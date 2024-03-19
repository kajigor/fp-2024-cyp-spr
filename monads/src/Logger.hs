module Logger where 

data Logger l a = 
    Logger [l] a 
    deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
    fmap :: (a -> b) -> Logger l a -> Logger l b
    fmap f (Logger l a) = Logger l (f a)
-- Proof why it works:
-- id: 
--     fmap id (Logger l a) === Logger l (id a) === Looger l a
-- Composition:
--     fmap (f . g) (Logger l a) === Logger l ((f . g) a) === 
--
-- Implement the instance and prove the laws
instance Applicative (Logger l) where 
    pure :: a -> Logger l a
    pure = Logger [] 

    (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
    (<*>) (Logger lgf f) (Logger lg a) = Logger (lgf ++ lg) (f a)
-- Prove of app law:
-- I'm looking on not trivial cases!
-- 1) (pure id) <*> (Logger l a) === (Logger [] id) <*> (Logger l a) === Logger l a - true
--
-- 2) (pure) (.) <*> u <*> v <*> w === Logger [] (.) <*> Logger l1 a <*> Logger l2 b <*> w ===
-- Logger l1 (. a) <*> Logger l2 b <*> w === Logger (l1++l2) (a . b) <*> Logger l3 c === 
-- Logger (l1 ++ l2 ++ l3)((a . b) c) =?=
-- u <*> (v <*> w) === u <*> (Logger (l2 ++ l3) (b c)) === Logger (l1 ++ l2 ++ l3) ((a . b) c)
--
-- 3) (pure f) <*> (pure b) === (Logger [] f) <*> (Logger [] b) === Logger [] (f b)
--
-- 4) u <*> pure y === Logger l a <*> Logger [] y === Logger l (a y) =?= 
-- pure ($ y) <*> u === Logger [] ($ y) <*> Logger l a === Logger l ($ y a) === 
-- Logger l (a y) - true
--
concatLogKeepSecond :: [l]  -> Logger l a -> Logger l a
concatLogKeepSecond l1 (Logger l2 a) = Logger (l1 ++ l2) a

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
    return :: a -> Logger l a
    return = pure

    (>>=) :: Logger l a -> (a -> Logger l b) -> Logger l b
    (>>=) (Logger l a) f = concatLogKeepSecond l (f a)
-- Prove of monade law:
-- 1) return b >>= k === Logger [] b >>= k === k b 
-- 2) m >>= return === Logger l b >>= return === concatLogKeepSecond l ( return b) ===
-- Logger l b - true
-- 3) m >>= (\x -> k x >>= h) === Logger l1 a >>= (\x -> k x >>= h) ===
-- Logger (l1 ++ l_k) (k a) >>= h === Logger (l1 ++ l_k ++ l_h) ((h . k) a) =?=
-- m >>= k >>= h === Logger l a >>= k >>= h ===
-- Logger (l ++ l_k) (k a) >>= h === Logger (l ++ l_k ++ l_h) ((h . k) a) - true
--

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

          