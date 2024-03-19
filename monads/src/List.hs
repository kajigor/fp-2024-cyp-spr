module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- 1. fmap id = id
-- Base, prove for Nil:
-- fmap id Nil = Nil
-- Induction step: for list size i, if is true for all lists of size < i
-- fmap id (Cons a as) = Cons (id a) (fmap id as) = Cons a as

-- 2. fmap (f . g) = fmap f . fmap g
-- Base, prove for Nil:
-- fmap (f . g) Nil = Nil = (fmap f . fmap g) Nil
-- Induction step: for list size i, if is true for all lists of size < i
-- fmap (f . g) (Cons a as) = Cons ((f . g) a) (fmap (f . g) as) = Const (f (g a)) (fmap f (fmap g as)) = fmap f (Cons (g a) (fmap g as)) = fmap f . fmap g (Const a as)

myConcat :: List a -> List a -> List a
myConcat Nil ys = ys
myConcat (Cons x xs) ys = Cons x (myConcat xs ys)

-- Implement the instance and prove the laws
instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) (Cons f fs) xs = myConcat (fmap f xs) (fs <*> xs)
  (<*>) _ _ = Nil

-- 1. pure id <*> v = v
-- Base, prove for Nil:
-- pure id <*> Nil = Nil
-- Induction step: for list size i, if is true for all lists of size < i
-- pure id <*> (Cons x xs) = Cons id Nil <*> Cons x xs = myConcat xs (Nil <*> xs) = xs

-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- u = Nil or v = Nil or w = Nil => true
-- pure (.) <*> u <*> v <*> w = (Cons (.) Nil) <*> (Cons x xs) <*> (Cons y ys) <*> (Cons z zs) = 
-- Cons ((.) x) (pure (.) <*> xs) <*> (Cons y ys) <*> (Cons z zs) = 
-- Cons (x . y) (pure (.) <*> xs <*> ys) <*> (Cons z zs) = 
-- Cons (x . y . z) (pure (.) <*> xs <*> ys <*> zs) = Cons (x . y . z) (xs <*> (ys <*> zs)) = 
-- (Cons x xs) <*> (Cons (y . z) (ys <*> zs)) = (Cons x xs) <*> ((Cons y ys) <*> (Cons z zs)) = u <*> (v <*> w)

-- 3. pure f <*> pure x = pure (f x) 
-- pure f <*> pure x = Cons (f x) Nil = pure (f x)

-- 4. u <*> pure y = pure ($ y) <*> u
-- Base, prove for Nil:
-- Nil <*> pure y = Nil = pure ($ y) <*> Nil
-- Induction step: for list size i, if is true for all lists of size < i
-- pure ($ y) <*> Cons x xs = Cons (x y) (pure ($ y) <*> xs) = Cons (x y) (xs <*> pure y) = Cons x xs <*> pure y


-- Implement the instance and prove the laws
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = myConcat (f x) (xs >>= f)

-- 1. return a >>= k = k a
-- return a >>= k = Cons a Nil >>= k = k a 

-- 2. m >>= return = m
-- m >>= return = 
-- [m = Nil] = Nil >>= return = Nil = m (Induction step)
-- [m = (Cons x xs)] = (Cons x xs) >> return = myConcat (return x) (xs >>= return) = Cons x xs

-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- m >>= (\x -> k x >>= h) = 
-- Base:
-- [m = Nil] = Nil = (Nil >>= k) >>= h
-- Induction step:
-- [m = (Cons y ys)] = (Cons y ys) >>= (\x -> k x >>= h) = 
--   myConcat (k y >>= h) (ys >>= (\x -> k x >>= h)) = 
--   myConcat (k y >>= h) ((ys >>= k) >>= h) = 
--   (myConcat (k y) (ys >>= k)) >>= h = 
--   ((Cons y ys) >>= k) >>= h = (m >>= k) >>= h