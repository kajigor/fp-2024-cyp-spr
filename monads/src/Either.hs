module Either where

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap :: (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  fmap f (MyRight x) = MyRight $ f x
  fmap _ (MyLeft x) = MyLeft x

-- 1. fmap id = id
-- fmap id (MyLeft x) = MyLeft x = id (MyLeft x)
-- fmap id (MyRight x) = MyRight $ id x = MyRight x = id (MyRight x)

-- 2. fmap (f . g) = fmap f . fmap g
-- fmap (f . g) (MyLeft x) = MyLeft x = fmap f MyLeft x = fmap f (fmap g MyLeft x)
-- fmap (f . g) (MyRight x) = MyRight (f . g) x = fmap f (MyRight g x) = fmap f (fmap g MyRight x) = (fmap f . fmap g) (MyRight x)


-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure :: a2 -> MyEither a1 a2
  pure = MyRight
  (<*>) :: MyEither a1 (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  (<*>) (MyRight f) x = fmap f x
  -- (<*>) _ (MyLeft x) = MyLeft x
  (<*>) (MyLeft a) _ = MyLeft a

-- 1. pure id <*> v = v
-- pure id <*> x = (MyRight id) x = fmap id x = id x = x

-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> u <*> v <*> w = MyRight (.) <*> u <*> v <*> w = (fmap (.) u) <*> v <*> w = 
-- [u = (MyLeft x)] = MyLeft x <*> v <*> w = MyLeft x = (MyLeft x) <*> (v <*> w) = u <*> (v <*> w) 
-- [u = (MyRight x)] = (MyRight $ (.) x) <*> v <*> w = fmap ((.) x) v <*> w = 
-- [v = (MyLeft y)] = fmap ((.) x) (MyLeft y) <*> w = MyLeft y <*> w = MyLeft y = MyRight x <*> MyLeft y = MyRight x <*> (MyLeft y <*> w)
-- [v = (MyRight y)] = fmap ((.) x) (MyRight y) <*> w = MyRight (x . y) <*> w = fmap (x . y) w = (fmap x) . (fmap y) w = (fmap x) ((MyRight y) <*> w) = MyRight x <*> (MyRight y <*> w) = u <*> (v <*> w)

-- 3. pure f <*> pure x = pure (f x) 
-- pure f <*> pure g = MyRight f <*> MyRight x = MyRight (f x) = pure (f x)

-- 4. u <*> pure x = pure ($ y) <*> u
-- pure ($ y) <*> u = fmap ($ y) u
-- [u = MyLeft a] = MyLeft a = MyLeft a <*> pure x = u <*> pure x
-- [u = MyRight a] = MyRight (a $ x) = MyRight a <*> MyRight x = u <*> pure x


-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  (>>=) :: MyEither a1 a2 -> (a2 -> MyEither a1 b) -> MyEither a1 b
  (>>=) (MyLeft x) _ = MyLeft x
  (>>=) (MyRight x) f = f x

-- 1. return a >>= k = k a
-- return a >>= k = MyRight >>= k = ka

-- 2. m >>= return = m
-- m >>= return =
-- [m = MyLeft x] = MyLeft x = m
-- [m = MyRight x] = return x = MyRight x = m 

-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- m >>= (\x -> k x >>= h) =
-- [m = MyLeft a] = (MyLeft a) >>= (\x -> k x >>= h) = MyLeft a = ((MyLeft a) >>= k) >>= h
-- [m = MyRight a] = (MyRight a) >>= (\x -> k x >>= h) = k a >>= h = ((MyRight a) >>= k) >>= h