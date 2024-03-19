module List where

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)
  --fmap id Nil = Nil
  --fmap id (Cons x y) = Cons (id x) (fmap id y) = Cons x y
  --fmap (f . g) Nil = Nil = fmap f Nil = fmap f (fmap g Nil) = fmap f . fmap g Nil
  --fmap (f . g) (Cons x y) = Cons ((f . g) x) (fmap (f . g) y) = Cons (f (g x)) (fmap f (fmap g y)) = fmap f (Cons (g x) (fmap g y)) = fmap f . fmap g (Cons x y)

--concatMap for our List
conList Nil b = b
conList (Cons a n) b = Cons a (conList n b) 
  
-- Implement the instance and prove the laws
instance Applicative List where
  pure x = Cons x Nil
  Cons a la <*> x = conList (fmap a x) (la <*> x)
  _ <*> _ = Nil
  --pure id <*> v = Cons id Nil <*> v,
  --  v = Nil => Cons id Nil <*> Nil = Nil,
  --  Cons id Nil <*> x = conList (fmap id x) (Nil <*> x) = conList x Nil = x
  --pure (.) <*> u <*> v <*> w ?= u <*> (v <*> w)
  --  as formulas are becoming too complicated, let's say it works this way: we apply functions for second list from head to tail and concatenate results.
  --  On the LHS we have composition on elemnts of u, then apply <*> to v and w, 
  --    which is taking the List of (u_i . v_j) w_k (we iterate through k inside iteration through j inside iteration through i),
  --  On the RHS we have u_i (v_j w_k) (we iterate through k inside iteration through j inside iteration through i),
  --  as (u_i . v_j) w_k = u_i (v_j w_k) LHS = RHS
  --pure f <*> pure x = Cons f Nil <*> Cons x Nil = conList (fmap f (Cons x Nil)) (Nil <*> (Cons x Nil)) = conList (f x) Nil = Cons (f x) Nil = pure (f x)
  --u <*> pure y,
  --  if u = Nil the Nil <*> pure y = Nil = pure ($ y) <*> Nil
  --  Cons ua ul <*> Cons y Nil = conList (fmap ua (Cons y Nil)) (ul <*> x) = conList (ua y) (pure ($ y) <*> ul) = Cons (($ y) ua) (pure ($ y) <*> ul) = pure ($ y) <*> u

-- Implement the instance and prove the laws
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _ = Nil
  Cons a la >>= f = conList (f a) (la >>= f)
  --pure x >>= f = Cons x Nil >>= f = conList (f x) Nil = f x
  --m >>= pure,
  --  if m = Nil then Nil >>= pure = Nil,
  --  m = Cons a la, m >>= f = conList (pure a) (la >>= pure) = Cons a (la >>= pure) = Cons a la = m
  --m >>= f is the same as concatMap f m for usual lists. 
  --  so for m >>= f >>= g is a flattened list of (f_i x_j) >>= g (we iterate through j inside iteration through i)
  --  on the other hand, m >>= (\x -> f x >>= g) is a list of (\x -> f x >>= g) $ x_i or flattened list of f_i x_j >>= g (we iterate through j inside iteration through i) 