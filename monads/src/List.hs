module List where 
import Language.Haskell.TH (Con)

data List a 
  = Nil |
  Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
-- Proof why it works:
-- id: 
--     fmap id Nil = Nil
--     fmap id (Cons x xs) = Cons (id x) (fmap id xs) = Cons x xs
-- Composition:
--     fmap (f . g) Nil = Nil
--     fmap (f . g) (Cons x xs) = Cons ((f . g) x) (fmap (f . g) xs) = Cons ((f . g) x) ((f.g) xs)
-- here i mean than function (f . g) will be applyed to each element of xs.

concatList :: List a -> List a -> List a
concatList Nil xs = xs
concatList xs Nil = xs
concatList (Cons x Nil) ys = Cons x ys
concatList (Cons x xs) ys = Cons x (concatList xs ys)

-- Implement the instance and prove the laws
instance Applicative List where 
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (b -> c) -> List b -> List c
  (<*>) (Cons f fs) xs = concatList (fmap f xs) (fs <*> xs)
  (<*>) Nil _ = Nil
-- Prove of app law:
--
-- 1) (pure id) <*> (Cons x xs) === (Cons id Nil) <*> (Cons x xs) === Cons x xs
-- (pure id) <*> Nil === (Cons id Nil) <*> Nil === Nil
-- 
-- 2) pure (.) <*> u <*> v <*> w === (Cons (.) Nil) <*> (Cons x xs) <*> (Cons y ys) <*> w ===
-- Cons (. x) (. xs) <*> (Cons y ys) <*> w === Cons (x . y) (xs .each ys) <*> w ===
-- Cons (x . y) (xs .each ys) <$> w =?=  Cons x xs <*> (Cons y ys <*> w) === 
-- Cons x xs <*> (Cons y ys <$> w) === Cons (x . y) (xs .each ys) <$> w - true
-- 
-- 3) (pure f) <*> (pure b) === (Cons f fs) <*> (Cons y ys) === Cons (f x) (fs "each" ys) 
--
-- 4) u <*> pure y === Cons x xs <*> Cons y Nil === Cons (x y) xs =?=
-- pure ($ y) <*> u === Cons ($ y) <*> Cons x xs === Cons ($ y x) <*> xs === Cons (x y) xs
-- 
-- Implement the instance and prove the laws
instance Monad List where
  return :: a -> List a
  return = pure

  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) (Cons x xs) f = concatList (f x) (xs >>= f) 
  (>>=) Nil _ = Nil
-- Prove of monade law:
-- 
-- 1) return b >>= k === Cons b Nil >>= k === concatList (k b) (Nil >>= k) === k b - true
--
-- 2) m >>= return === Cons x xs >>= return === ConcatList (return x) (xs >>= return) ===
-- ConcatList (Cons x Nil) (xs >>= return) === m
--
-- 3)  m >>= (\x -> k x >>= h) === Cons x xs >>= (\x -> k x >>= h) === 
-- concatList (k x >>= h) (xs >>= (\x -> k x >>= h)) =?= 
-- m >>= k >>= h === Cons x xs >>= k >>= h === concatList (k x) (xs >>= k) >>= h ===
-- Cons (k x) (k each xs) >>= h === concatList (h (k x)) (k each xs >>= h)
-- if think carefully, we can see that this two lists are equale.
--
