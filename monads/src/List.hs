module List where 

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{-
  1) fmap id == id
  fmap id Nil == Nil
  fmap id (Cons x xs) == Cons (id x) (fmap id xs) == Cons x (fmap id xs) == Cons x xs

  Proof by induction (given fmap id xs == xs)

  2) fmap (f . g)  ==  fmap f . fmap g
  fmap (f . g) Nil == Nil == fmap f Nil == fmap f (fmap g Nil) == (fmap f . fmap g) Nil
  fmap (f . g) (Cons x xs) == Cons ((f . g) x) (fmap (f . g) xs) ==
    == Cons ((f (g x)) (fmap (f . g) xs) == Cons ((f (g x)) ((fmap f . fmap g) xs)) ==
    == fmap f (Cons (g x) (fmap g xs)) == (fmap f) . (fmap g) (Cons x xs)


  Proof by induction (given fmap (f . g) xs == (fmap f . fmap g) xs)
-}

concatList :: List a -> List a -> List a
concatList xs Nil = xs
concatList Nil ys = ys
concatList (Cons x xs) ys = Cons x (concatList xs ys)

-- Implement the instance and prove the laws
instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (b -> c) -> List b -> List c
  (<*>) (Cons f fs) xs = concatList (fmap f xs) (fs <*> xs)
  (<*>) Nil _ = Nil

{-
  1) pure id <*> l = l
  pure id <*> Nil == (Cons id Nil) <*> Nil == Nil
  pure id <*> (Cons x xs) == concatList (fmap id (Cons x xs)) (Nil <*> (Cons x xs)) ==
    == (fmap id (Cons x xs)) == (Cons x xs)

  2) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  trivial for Nil's
  
  (Cons (.) Nil) <*> (Cons u us) <*> (Cons v vs) <*> (Cons w ws) ==
  Cons (. u) (. us) <*> (Cons v vs) <*> (Cons w ws) == Cons (u . v) (us <*> vs) <*> (Cons w ws) ==
  Cons (u . v . w) (us <*> vs <*> ws) == Cons u us <*> (Cons v vs <*> (Cons w ws))
  
  using the induction ( pure (.) <*> us <*> vs <*> ws = us <*> (vs <*> ws))

  3) pure f <*> pure x = pure (f x)
  (Cons f Nil) <*> (Cons x Nil) == f <$> (Cons x Nil) == Cons (f x) Nil == pure (f x) 
  
  4) u <*> pure y = pure ($ y) <*> u
  Cons u us <*> Cons y Nil == concatList (u <$> y) (us <*> y) == pure ($ y) <*> u
  (Ccan be seen for one application, the `us` follow by induciton)
-}


-- Implement the instance and prove the laws
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) (Cons x xs) f = concatList (f x) (xs >>= f) 
  (>>=) Nil _ = Nil

{-
  1) return a >>= k = k a
  return a >>= k == Cons a Nill >>= k == k a 
  
  2) m >>= return = m
  Nil >>= return == Nil
  Cons x xs >>= return == ConcatList (return x) (xs >>= return) ==
  == ConcatList (Cons x Nil) (xs >>= return) == ConcatList (Cons x Nil) xs == Cons x xs 

  3) m >>= (\x -> k x >>= h) = (m >>= k) >>= h
  Nil >>= (\x -> k x >>= h) == (k Nil >>= h) == (Nil >>= k) >>= h
  Cons m ms >>= (\x -> k x >>= h) == ConcatList (Cons (k m) >>= h) ((\x -> k x >>= h) ms) ==
  == ConcatList (Cons (k m) >>= h) ((ms >>= k) >>= h) == ((Cons m ms) >>= k) >>= h
-}