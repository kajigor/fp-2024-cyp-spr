module Either where

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap function (MyLeft x) = MyLeft x
  fmap function (MyRight x) = MyRight (function x)


-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure = MyRight
  MyLeft error <*> _ = MyLeft error
  MyRight function <*> cont = fmap function cont


-- Implement the instance and prove the laws
instance Monad (MyEither a) where
--  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  MyRight value >>= f = f value
  MyLeft value >>= f = MyLeft value
