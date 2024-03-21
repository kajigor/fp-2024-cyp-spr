module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)


concatList Nil list = list
concatList (Cons head1 tail1) list2 = Cons head1 (concatList tail1 list2)


-- Implement the instance and prove the laws (I proved Monad with 3 rules, without <*> -> no need to comment it)
instance Functor List where
  fmap function Nil = Nil
  fmap function (Cons head tail) = Cons (function head) (fmap function tail)


---- Implement the instance and prove the laws (I proved Monad with 3 rules, without <*> -> no need to comment it)
instance Applicative List where
  pure el = Cons el Nil
  Nil <*> _ = Nil
  Cons headFunction tailFunction <*> Cons headItem tailItem = Cons (headFunction headItem) (tailFunction <*> tailItem)
  _ <*> _ = Nil


-- Implement the instance and prove the laws
instance Monad List where
  Nil >>= handler = Nil
  Cons head tail >>= f = concatList (f head) (tail >>= f)


-- Left identity:
-- return a >>= k = k a

-- return x >>= f
-- List x >>= f
-- Nil >>= f = Nil
-- Cons h t = f h


-- Right identity:
-- m >>= return = m

-- List a >>= return
-- return a
-- m a


-- Associativity:
-- m >>= (\x -> k x >>= h) = (m >>= k) >>= g

-- Nil >>= (\x -> k x >>= h) = Nil >>= h
-- (Nil >>= k) >>= h === Nil >>= h

-- Cons a tail >>= (\x -> k x >>= h) = k a tail >>= h
-- (Cons a tail >>= k) >>= h === k a tail >>= h
