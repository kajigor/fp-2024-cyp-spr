{-# LANGUAGE InstanceSigs #-}
module Either where

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)


{-
IDENTITY: fmap id x === id x
fmap id (MyLeft l) = id $ MyLeft l = MyLeft l
fmap id (MyRight r) = MyRight (id r) = MyRight r

COMPOSITION: fmap (f . g) x === (fmap f . fmap g) x
fmap (f . g) (MyLeft l) = MyLeft l = fmap f (MyLeft l) = fmap f (fmap g (MyLeft l))
fmap (f . g) (MyRight r) = MyRight ((f . g) r) = fmap f (MyRight (g r)) = fmap f (fmap g (MyRight r))
-}

instance Functor (MyEither a) where
  fmap :: (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  fmap _ (MyLeft l) = MyLeft l
  fmap f (MyRight r) = MyRight (f r)


instance Applicative (MyEither a) where
  pure :: a2 -> MyEither a1 a2
  pure = MyRight
  (<*>) :: MyEither a1 (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  (<*>) (MyLeft l) _ = MyLeft l
  (<*>) (MyRight r) f = fmap r f


{-
LEFT IDENTITY: return a >>= f === f a
return a >>= f = MyRight a >>= f = f a

RIGHT IDENTITY: m >>= return === m
MyLeft l >>= return = MyLeft l >>= return = MyLeft l
MyRight r >>= return = MyRight r >>= MyRight = MyRight r

ASSOCIATIVITY: f >>= (\x -> g x >>= h) = (f >>= g) >>= h
MyLeft l >>= (\x -> g x >>= h) = MyLeft l = (MyLeft l >>= g) >>= h
MyRight r >>= (\x -> g x >>= h) = g r >>= h = (MyRight r >>= g) >>= h
-}
instance Monad (MyEither a) where
  (>>=) :: MyEither a1 a2 -> (a2 -> MyEither a1 b) -> MyEither a1 b
  (>>=) (MyLeft l) _ = MyLeft l
  (>>=) (MyRight r) f = f r

