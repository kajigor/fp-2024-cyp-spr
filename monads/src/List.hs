{-# LANGUAGE InstanceSigs #-}
module List where 

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

{-
Proof:
1) Identity: fmap id x === id x
Case 1: x = Nil
  fmap id Nil = Nil === id Nil
Case 2: x = Cons h t
  fmap id (Cons h t) = Cons (id h) (fmap id t) === Cons h (fmap id t) === id (Cons h t)
2) Composition: fmap (f . g) x === fmap f . fmap g 
Case 1: x = Nil
  fmap (f . g) Nil = Nil
  fmap f . (fmap g Nil) = fmap f Nil = Nil
Case 2: x = Cons h t
  fmap (f . g) (Cons h t) = Cons ((f . g) h) (fmap (f . g) t)
  fmap f . fmap g (Cons h t) === fmap f (Cons (g h) t') === Cons ((f . g) h) (fmap f t') === Cons ((f . g) h) (fmap (f . g) t) 
  where t' = fmap g t
-}
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)



listConcat :: List a -> List a -> List a
listConcat Nil x = x
listConcat (Cons h Nil) x = Cons h x
listConcat (Cons h t) x = Cons h (listConcat t x) 


{-
Proof:
1) Identity: pure id <*> v === Cons id Nil <*> v === id v === v
2) Homomorphism: pure f <*> pure x === Cons f Nil <*> Cons x Nil === Cons (f x) Nil === pure (f x)
3) Interchange: u <*> pure y === pure ($ y) <*> u
Case 1: u = Nil
  Nil <*> pure y = Nil
  pure ($ y) <*> Nil === (Cons ($ y) Nil) <*> Nil === Nil
Case 2: u = Cons h t
  Cons h t <*> pure y === Cons h t <*> Cons y Nil = Cons (h y) (t <*> (Cons y Nil))
  pure ($ y) <*> Cons h t === (Cons ($ y) Nil) <*> Cons h t = Cons (h y) ((Cons ($ y) Nil) <*> t) === Cons (h y) (t <*> (Cons y Nil))
4) Composition: pure (.) <*> u <*> v <*> w === u <*> (v <*> w)
Case 1: u = Nil
  pure (.) <*> Nil <*> v <*> w = Nil <*> v <*> w === Nil
  Nil <*> (v <*> w) = Nil
Case 2: u = Cons h t
  pure (.) <*> Cons h t <*> v <*> w === Cons (.) Nil <*> Cons h t <*> v <*> w === fmap (.) (Cons h t) <*> v <*> w ===
  listConcat ((. h) <$> v) (fmap . t <*> v) <*> w === [(f . g) | g <- u, f <- v] <*> w === [(f . g) val | g <- u, f <- v, val <- w]
  Cons h t <*> (v <*> w) === Cons h t <*> [f val | f <- v, val <- w] === [(f . g) val | g <- u, f <- v, val <- w]
  where [] is our List a
-}
instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) Nil _ = Nil
  (<*>) (Cons h t) x = listConcat (h <$> x) (t <*> x)
 

{-
1) Left identity: return x >>= k === Cons x Nil >>= k = listConcat (k x) (Nil) === Cons (k x) Nil
2) Right identity: m >>= return === m 
Case 1: m = Nil
  Nil >>= return === Nil
Case 2: m = Cons h t
  Cons h t >>= return === listConcat (return h) (t >>= return) === listConcat (Cons h Nil) (t >>= return) === Cons h t 
3) Associativity: m >>= (\x -> k x >>= h) === (m >>= k) >>= h
Case 1: m = Nil
  Nil >>= (\x -> k x >>= h) === Nil
  (Nil >>= k) >>= h === Nil >>= h === Nil
Case 2: m = Cons h t
  Cons h t >>= (\x -> k x >>= w) === listConcat (k h) (t >>= k) >>= w
  ((Cons h t) >>= k) >>= w === listConcat (k h) (t >>= k) >>= w
-}
instance Monad List where
  return :: a -> List a
  return = pure
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Nil _ = Nil
  (>>=) (Cons h t) f = listConcat (f h) (t >>= f)
