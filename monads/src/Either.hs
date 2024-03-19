module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where 
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap _ (MyLeft a) = MyLeft a
  fmap f (MyRight b) = MyRight (f b)
-- Proof why it works:
-- id: 
--     fmap id (MyLeft a) = MyLeft a
--     fmap id (MyRight b) = MyRight (id b) = MyRight b
-- Composition:
--     fmap (f . g) (MyLeft a) = MyLeft a
--     fmap (f . g) (MyRight b) = MyRight (f . g) b
--
-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure :: b -> MyEither a b
  pure = MyRight
  (<*>) :: MyEither a (b -> c) -> MyEither a b -> MyEither a c
  (<*>) (MyRight g) (MyRight a) = MyRight $ g a  
  (<*>) (MyLeft a) _ = MyLeft a
  (<*>) _ (MyLeft a) = MyLeft a
-- Prove of app law:
-- I'm looking on not trivial cases!
-- 1) (pure id) <*> (MyRight b) === (MyRight id) <*> (MyRight b) === MyRight b - true
-- (pure id) <*> (MyLeft a) === (Right id) <*> (MyLeft a) === MyLeft a - true
--
-- 2) (pure) (.) <*> u <*> v <*> w === MyRight (.) <*> MyRight a <*> MyRight b <*> w ===
-- MyRight (. a) <*> v <*> w === MyRight (a . b) <*> w === MyRight ((a . b) w) =?=
-- u <*> (v <*> w) === u <*> (MyRight (b w)) === MyRight ((a . b) w)  - true
--
-- 3) (pure f) <*> (pure b) === (MyRight f) <*> (MyRight b) === MyRight (f b)
--
-- 4) u <*> pure y === MyRight a <*> pure y === MyRight (a y) =?= 
-- pure ($ y) <*> u === MyRight ($ y) <*> MyRight a === MyRight ($ y a) === MyRight (a y) - true

-- Implement the instance and prove the laws
instance Monad (MyEither a) where 
  return :: b -> MyEither a b
  return = pure

  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  (>>=) (MyRight b) f =  f b
  (>>=) (MyLeft a) _ = MyLeft a
-- Prove of monade law:
-- 1) return b >>= k === MyRight b >>= k === k b 
-- 2) m >>= return === Eather a b >>= return === Eather a b === m
-- 3) m >>= (\x -> k x >>= h) === Eather a b >>= (\x -> k x >>= h) ===
-- k (Eather a b) >>= h === (h . k) (Eather a b) =?= m >>= k >>= h === Eather a b >>= k >>= h ===
-- k (Eather a b) >>= h === (h . k) (Eather a b) - true