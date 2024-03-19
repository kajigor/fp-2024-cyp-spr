{-# LANGUAGE InstanceSigs #-}
module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)


{-
IDENTITY: fmap id x === id x
fmap id Nil = Nil
fmap id (Cons x xs) = Cons (id x) (fmap id xs) = Cons x (fmap id xs) = Cons x xs

COMPOSITION: fmap (f . g) x === (fmap f . fmap g) x
fmap (f . g) Nil = Nil
fmap (f . g) (Cons x xs) = Cons ((f . g) x) (fmap (f . g) xs) = Cons (f (g x)) (fmap f (fmap g xs)) = fmap f (Cons (g x) (fmap g xs)) = fmap f (fmap g (Cons x xs))
-}
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

concatList :: List b -> List b -> List b
concatList Nil ys = ys
concatList xs Nil = xs
concatList (Cons x xs) ys = Cons x (concatList xs ys)

-- Implement the instance and prove the laws
instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) x = concatList (fmap f x) (fs <*> x)


{-
LEFT IDENTITY: return a >>= f === f a
return a >>= f = Cons a Nil >>= f = f a

RIGHT IDENTITY: m >>= return === m
Nil >>= return = Nil
Cons x xs >>= return = return x `concatList` (xs >>= return) = Cons x Nil `concatList` (xs >>= return) = Cons x Nil `concatList` xs = Cons x xs

ASSOCIATIVITY: f >>= (\x -> g x >>= h) = (f >>= g) >>= h
Nil >>= (\x -> g x >>= h) = Nil = (Nil >>= g) >>= h
Cons x xs >>= (\x -> g x >>= h) = g x `concatList` (xs >>= (\x -> g x >>= h)) = g x `concatList` ((xs >>= g) >>= h) = (Cons x xs >>= g) >>= h
-}
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = concatList (f x) (xs >>= f)
