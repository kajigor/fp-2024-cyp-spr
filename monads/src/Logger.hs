module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
    fmap f (Logger xs x) = Logger xs (f x)


-- Proof:
-- Law 1. fmap id (Logger l a) = Logger l (id a) = Logger l a

-- Law 2. fmap (f . g) (Logger l a) = Logger l (f(g a))
--        fmap f (fmap g (Logger l a)) = fmap f (Logger l g(a)) = Logger l f(g(a))
    

-- Implement the instance and prove the laws
instance Applicative (Logger l) where
    pure a = Logger [] a
    (Logger l a) <*> (Logger l2 b) = Logger (l ++ l2) (a b)

-- Proof:
-- Law 1. (v = Logger l1 b): pure id <*> v = Logger [] id <*> Logger l1 b = Logger l1 (id b) =  Logger l1 b

-- Law 2. pure (.) <*> u <*> v <*> w = (Logger [] (.)) <*> (Logger l1 x) <*> (Logger l2 y) <*> (Logger l3 z) = 
--        (Logger l1 ((.) x)) <*> (Logger l2 y) <*> (Logger l3 z) = (Logger (l1 ++ l2) (x . y)) <*> (Logger l3 z) = 
--        (Logger (l1 ++ l2 ++ l3) (x . y . z)) =

--        u <*> (v <*> w) = (Logger l1 x) <*> (Logger (l2 ++ l3) (y . z)) = Logger (l1 ++ l2 ++ l3) (x . y .z)


-- Law 3. pure f <*> pure x = Logger [] f <*> Logger [] x = Logger [] (f x) = pure (f x)

-- Law 4. (u = Logger l1 b): u <*> pure y = Logger l1 b <*> Logger [] y = Logger l1 (b y)
--        pure ($ y) <*> u = Logger [] ($ y) <*> Logger l1 b = Logger l1 (b y)

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
    (Logger l a) >>= f = Logger l id <*> f a

-- Proof:
-- Law 1. return a >>= h = Log [] a >> h = Logger [] id <*> h a = h a

-- Law 2. (m = Logger l a): m >>= return = Logger l a >>= return = Logger l id <*> return a = Logger l id <*> Logger [] a Logger l a

-- Law 3.  m >>= (\x -> k x >>= h) = (Logger l a) >>= (\x -> k x >>= h) = Logger l id <*> (k a >>= h) =
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

          