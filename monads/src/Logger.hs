module Logger where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where 
    fmap f (Logger l a) = Logger l (f a)

{-
proof : obviously 
-}

-- Implement the instance and prove the laws
instance Applicative (Logger l) where 
    pure v = Logger [] v
    (<*>) (Logger l1 f) (Logger l2 v) = Logger (l1 ++ l2) (f v)

{-  proof:
    Identity: pure id <*> Logger l v = Logger [] id <*> Logger l v = Logger l (id v) = Logger l v
    Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    1) pure (.) <*> Logger l1 v1 <*> Logger l2 v2 <*> Logger l3 v3 = Logger [] (.) <*> Logger l1 v1 <*> Logger l2 v2 <*> Logger l3 v3 = Logger l1 ((.) v1) <*> Logger l2 v2 <*> Logger l3 v3 = Logger (l1 ++ l2) (v1 . v2) <*> Logger l3 v3 = Logger (l1 ++ l2 ++ l3) (v1 $ v2 v3)
    2) Logger l1 v1 <*> (Logger l2 v2 <*> Logger l3 v3) = Logger l1 v1 <*> Logger (l2 ++ l3) (v2 v3) = Logger (l1 ++ l2 ++ l3) (v1 $ v2 v3)
    Homomorphism: 
    1) pure f <*> pure x = pure Logger [] f <*> pure Logger [] x = Logger [] (f x)
    2) pure (f x) = Logger [] (f x)
    Interchange: u <*> pure y = pure ($ y) <*> u
    1) Logger l1 f <*> pure v = Logger l1 f <*> Logger [] v = Logger l1 (f v)
    2) pure (\f -> f x) <*> Logger l v = Logger [] (\f -> f x) <*> Logger l v = Logger l (f v)
-}

-- Implement the instance and prove the laws
instance Monad (Logger l) where 
    return f = Logger [] f

    (>>=) (Logger l x) f = let Logger log val = f x in Logger (l++log) val
{- Proof:
    1) return f >>= k = Logger [] f >>= k = f k, because left log empty
    2) Logger l x >>= return = let Logger ([], x) = return x in Logger (l++[]) () x = Logger l x
    3) m        >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
    context :   let Logger lk kv = k v1, 
                let Logger lh hv = h kv
    3.1) Logger l1 v1 >>= (\x -> k x >>=h ) = let Logger (a, b) = (\x -> k x >>=h ) v1 in Logger(l1++a, v)
            (\x -> k x >>=h ) v1 = k v1 >>= h = Logger lk kv >>= h = Logger (lk++lh) hv => Logger(l1++a, v) = Logger (l1++lk++lh) hv
    3.2) Logger l1 v1 >>= k >>=h = Logger (l1++lk) kv >>= h = Logger (l1++lk++lh) hv

-}

{- Also we should prove m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))) (and we can dont prove applicative laws explicitly, but i forgot about it)
    1) Logger l1 v1 <*> Logger l2 v2 = Logger (l1++l2) (v1 v2)
    2) Logger l1 v1 >>= (\x1 -> Logger l2 v2 >>= (\x2 -> return (x1 x2) ) ) 
        2.1) (\x2 -> return (v1 x2) ) v2 = return (v1 v2) = Logger [] (v1 v2)
        2.2) Logger l2 v2 >>= (\x2 -> return (v1 x2) ) = Logger l2 (v1 v2)
        2.3) (\x1 -> Logger l2 v2 >>= (\x2 -> return (x1 x2) ) ) v1 = Logger l2 v2 >>= (\x2 -> return (v1 x2) ) = Logger l2 (v1 v2)
        2.4) Logger l1 v1 >>= (\x1 -> Logger l2 v2 >>= (\x2 -> return (x1 x2) ) ) = Logger (l1++l2) (v1 v2)
 
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

          