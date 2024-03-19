module List where 

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap f (Cons a as) = Cons (f a) (fmap f as)
  fmap _ n = n

-- Proof:
-- Law 1. Induction:
--          Base: fmap id Nil = Nil
--          Step: (if true for size < k, then true for size k): fmap id (Cons a as) = Cons (id a) (fmap id as) = Cons a as

-- Law 2. Induction:
--          Base: fmap (f . g) Nil = Nil
--            fmap f (fmap g Nil) = f Nil = Nil
--          Step (if true for size < k, then true for size k):
--            fmap (f . g) (Cons a as) = Cons f(g(a)) (fmap f(g(as)))
--            fmap f (fmap g (Cons a as)) = fmap f (Cons g(a)) (fmap g(as)) = Cons f(g(a)) (fmap f(g(a)))

concat :: List a -> List a -> List a
concat Nil y = y
concat (Cons y ys) z = Cons y (concat ys z)


-- Implement the instance and prove the laws
instance Applicative List where
  pure a = Cons a Nil
  Cons a as <*> xs = concat (fmap a xs) (as <*> xs)
  a <*> b = Nil


-- Proof:
-- Law 1. pure id <*> v = Cons id Nil <*> v = concat (fmap id v) (Nill <*> v) = fmap id v = v

-- Law 2. u = Nil or v = Nil or w = Nil - ok
--        pure (.) <*> u <*> v <*> w = (Cons (.) Nill) <*> (Cons x xs) <*> (Cons y ys) <*> (Cons z zs) = 
--        Cons ((.) x) (pure (.) <*> xs) <*> (Cons y ys) <*> (Cons z zs) = 
--        Cons (x . y) (pure (.) <*> xs <*> ys) <*> (Cons z zs) = 
--        Cons (x . y . z) (pure (.) <*> xs <*> ys <*> zs) = Cons (x . y . z) (xs <*> (ys <*> zs)) = 
--        (Cons x xs) <*> (Cons (y . z) (ys <*> zs)) = (Cons x xs) <*> ((Cons y ys) <*> (Cons z zs)) = u <*> (v <*> w)

-- Law 3. pure f <*> pure x = Cons f Nill <*> Cons x Nil = concat (fmap f (Cons x Nil)) (Nil <*> (Cons x Nil)) = Const (f x)

-- Law 4. Nill <*> pure y = Nill = pure ($ y) <*> Nil
--        pure ($ y) <*> Cons x xs = Cons (x y) (pure ($ y) <*> xs) = Cons (x y) (xs <*> pure y) = Cons x xs <*> pure y


-- Implement the instance and prove the laws
instance Monad List where
  Cons a as >>= f = concat (f a) (as >>= f)
  n >>= _ = n

-- Proof:
-- Law 1. return a >>= f = Cons a Nil >>= f = f a

-- Law 2. m >>= return = 
--        (if m == Nil) = Nil >>= return = Nil = m 
--        (if m = (Cons x xs)) = (Cons x xs) >> return = myConcat (return x) (xs >>= return) = Cons x xs

-- law 3. 
--  1. m = Nil: = Nil = (Nil >>= k) >>= h

--  2. m = (Cons y ys): = (Cons y ys) >>= (\x -> k x >>= h) = 
--    myConcat (k y >>= h) (ys >>= (\x -> k x >>= h)) = 
--    myConcat (k y >>= h) ((ys >>= k) >>= h) = 
--    (myConcat (k y) (ys >>= k)) >>= h = 
--    ((Cons y ys) >>= k) >>= h = (m >>= k) >>= h

