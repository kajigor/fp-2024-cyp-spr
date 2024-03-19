module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap f (MyLeft x) = MyLeft x
  fmap f (MyRight x) = MyRight (f x)

{-
  1) fmap id == id
  fmap id (MyLeft x) == (MyLeft x)
  fmap id (MyRight x) == MyRight (id x) == MyRight x  
  
  2) fmap (f . g)  ==  fmap f . fmap g
  fmap (f . g) (MyLeft x) == MyLeft x == fmap f (MyLeft x) == fmap f (fmap g (MyLeft x)) == (fmap f . fmap g) (MyLeft x) 
  fmap (f . g) (MyRight x) == MyRight (f.g) x == fmap f (MyRight g x) == fmap f (fmap g (MyRight x)) == (fmap f . fmap g) (MyRight x)
-}

-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure :: b -> MyEither a b
  pure = MyRight
  (<*>) :: MyEither a (b1 -> b2) -> MyEither a b1 -> MyEither a b2
  (MyLeft e) <*> _ = MyLeft e
  (MyRight f) <*> x =  f <$> x

{-
  1) pure id <*> v = v
  pure id <*> (MyLeft x) = (MyRight . id) <*> (MyLeft x) = id <$> (MyLeft x) = (MyLeft x)
  pure id <*> (MyRight x) = (MyRight . id) <*> (MyRight x) = id <$> (MyRight x) = (MyRight x)

  2) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  pure (.) <*> (MyLeft e) <*> v <*> w = (MyLeft e) = (MyLeft e) <*> _ = (MyLeft e) <*> (v <*> w)
  pure (.) <*> (MyRight u) <*> (MyLeft e) <*> w = u <$> (MyLeft e) = u <$> ((MyLeft e) <*> w) = (MyRight u) <*> ((MyLeft e) <*> w)
  pure (.) <*> (MyRight u) <*> (MyRight v) <*> w = (u <$> v) <$> w = u <$> (v <$> w) = (MyRight u) <*> ((MyRight v) <*> w)

  3) pure f <*> pure x = pure (f x)
  pure f <*> pure x = (MyRight f) <*> (MyRight x) = f <$> (MyRight x) = MyRight (f x) = pure (f x)
  
  4) u <*> pure y = pure ($ y) <*> u
    (MyLeft e) <*> pure y = (MyLeft e) = ($ y) <$> (MyLeft e) = MyRight ($ y) <*> (MyLeft e) = pure ($ y) <*> (MyLeft e)
    (MyRight u) <*> pure y = (MyRight u) <*> (MyRight y) = MyRight (u y) = ($ y) <$> (MyRight u) = MyRight ($ y) <*> (MyRight u) 
-}

-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  (>>=) :: MyEither a b1 -> (b1 -> MyEither a b2) -> MyEither a b2
  MyLeft l >>= _ = MyLeft l
  MyRight r >>= k = k r
  return :: b -> MyEither a b 
  return = pure

{-
  1) return a >>= k = k a
  return a >>= k == MyRight a >>= k == k a 

  2) m >>= return = m
  MyLeft f >>= return == MyLeft f
  MyRight f >>= return == return f == MyRight f
  
  3) m >>= (\x -> k x >>= h) = (m >>= k) >>= h
  MyLeft m >>= (\x -> k x >>= h) == MyLeft m == MyLeft m >>= k == (MyLeft m >>= k) >>= h
  MyRight m >>= (\x -> k x >>= h) == (k m) >>= h == (MyRight m >>= k) >>= h
-}