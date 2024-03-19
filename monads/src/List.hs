module List(List(..)) where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr f init Nil = init
listFoldr f val (Cons a as) = f a $ listFoldr f val as

listMap :: (a -> b) -> List a -> List b
listMap f = listFoldr (Cons . f) Nil

listConcat :: List a -> List a -> List a
listConcat xs ys = listFoldr Cons ys xs

listFlatMap :: (a -> List b) -> List a -> List b
listFlatMap f = listFoldr (listConcat . f) Nil



-- Implement the instance and prove the laws
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{-

-- all proves will be conducted by "induction"
fmap id Nil = Nil
fmap id $ Cons x xs = Cons (id x) (fmap id xs) = Cons x (fmap id xs)
-- in reality we proved that == will never return false here

fmap (f . g) Nil = Nil
fmap f (fmap g Nil) = fmap f Nil = Nil

fmap (f . g) $ Cons a as = Cons (f $ g a) $ fmap (f . g) as
fmap f (fmap g $ Cons a as) = fmap f (Cons (g a) $ fmap g as) = Cons (f $ g a) (fmap f (fmap g as))
-}


-- Implement the instance and prove the laws
instance Applicative List where
  pure x = Cons x Nil
  (<*>) fls xs = listFlatMap (`listMap` xs) fls

{-
pure id <*> [v_1, ..., v_i, ...] = [id] v = [id v_1, ..., id v_i, ...] = [v_1, ..., v_i, ...] = v

pure f <*> pure x = [f] <*> [x] = [f x] = pure $ f x

[f_1, ..., f_n] <*> pure y = [f_1 y, ..., f_n y, ...]
pure ($ y) <*> [f_1, ..., f_n, ...] = [\x -> x y] <*> [f_1, ..., f_n, ...] = [f_1 y, ..., f_n y, ...]

pure (.) <*> u <*> v <*> w = [..., \x -> u_i (v_j x), ...] <*> w = [..., u_i (v_j w_k), ...]
u <*> (v <*> w) = [..., u_i (v_j w_k), ...]


-}

-- Implement the instance and prove the laws
instance Monad List where
  (>>=) Nil func = Nil
  (>>=) as func = listFlatMap func as

{-
return a >>= h = [a] >>= h = h a --only one value flatmapped

[a_1, a_2, ..., a_n, ...] >>= return
after application of return [[a_1], [a_2], ..., [a_n], ...]
after flattening [a_1, ..., a_n, ...]

[a_1, a_2, ..., a_n, ...] >>= f >>= g
[a_1, a_2, ..., a_n, ...] >>= (\x -> g x >>= h)

Assume f a_i = [b_i_1, ..., b_i_{m_i}].
Then both will return flatten [h b_1_1, h b_1_2, ..., h b_1_{m_1}, ..., h b_n_1, ..., h b_n_1, ..., h b_n_{m_n}, ...].
-}

