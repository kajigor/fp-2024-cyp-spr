module Either where

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- Implement the instance and prove the laws (I proved Monad with 3 rules, without <*> -> no need to comment it)
instance Functor (MyEither a) where
  fmap function (MyLeft x) = MyLeft x
  fmap function (MyRight x) = MyRight (function x)


-- Implement the instance and prove the laws (I proved Monad with 3 rules, without <*> -> no need to comment it)
instance Applicative (MyEither a) where
  pure = MyRight
  MyLeft error <*> _ = MyLeft error
  MyRight function <*> cont = fmap function cont


-- Implement the instance and prove the laws
instance Monad (MyEither a) where
--  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  MyRight value >>= f = f value
  MyLeft value >>= f = MyLeft value

-- Left identity:
-- return a >>= k = k a

-- return x >>= f
-- MyRight x >>= f
-- f x


-- Right identity:
-- m >>= return = m

-- MyRight a >>= return
-- return a
-- m a


-- Associativity:
-- m >>= (\x -> k x >>= h) = (m >>= k) >>= g

-- MyRight a >>= (\x -> k x >>= h) = k a >>= h
-- (MyRight a >>= k) >>= h === k a >>= h

-- MyLeft a >>= (\x -> k x >>= h) = k a >>= h
-- (MyLeft a >>= k) >>= h === k a >>= h
