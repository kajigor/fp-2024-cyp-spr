module Either where

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap f (MyRight x) = MyRight (f x)
  fmap _ (MyLeft y) = MyLeft y
  --fmap id (MyRight x) = MyRight (id x) = MyRight x
  --fmap id (MyLeft y) = MyLeft y
  --fmap (f . g) (MyRight x) = MyRight ((f . g) x) = MyRight (f (g x)) = fmap f (MyRight (g x)) = fmap f (fmap g (MyRight x)) = fmap f . fmap g (MyRight x)
  --fmap (f . g) (MyLeft y) = MyLeft y = fmap f (MyLeft y) = fmap f (fmap g (MyLeft y)) = fmap f . fmap g (MyLeft y)

-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure = MyRight
  MyLeft x <*> _ = MyLeft x
  _ <*> MyLeft x = MyLeft x
  (MyRight f) <*> (MyRight g) = MyRight (f g)
  --pure id <*> v = MyRight id <*> v,
  --  MyRight id <*> (MyLeft y) = MyLeft y, MyRight id <*> (MyRight x) = MyRight (id x) = MyRIght x
  --pure f <*> pure x = MyRight f <*> MyRight x = MyRight (f x) = pure (f x)
  --f <*> pure x = pure ($ x) <*> f,
  --  MyLeft fl <*> MyRight x = MyLeft fl = MyRight ($ x) <*> MyLeft fl = pure ($ x) <*> MyLeft fl,
  --  MyRight fr <*> MyRight x = MyRight (fr x) = MyRight (($ x) fr) = MyRight ($ x) <*> MyRight fr = pure ($ x) <*> MyRight fr
  --pure (.) <*> u <*> v <*> w = MyRight (.) <*> u <*> v <*> w,
  --  u is MyLeft => MyRight (.) <*> u <*> v <*> w = u = u <*> (v <*> w)
  --  MyRight (.) <*> (MyRight ur) <*> v <*> w = MyRight ((.) ur) <*> v <*> w,
  --    v is MyLeft => MyRight ((.) ur) <*> v <*> w = v = MyRight ur <*> v = MyRight ur <*> (v <*> w),
  --    MyRight ((.) ur) <*> MyRight vr <*> w = MyRight (ur . vr) <*> w = MyRight ur <*> (MyRight vr <*> w)  


-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  MyLeft x >>= _ = MyLeft x
  MyRight x >>= f = f x
  --pure x >>= f = MyRight x >>= f = f x
  --m >>= return,
  --  m = MyLeft ml, MyLeft ml >>= return = MyLeft ml
  --  m = MyRight mr, MyRight mr >>= return = return mr = MyRight mr
  --(m >>= f) >>= g,
  --  m = MyLeft ml, (MyLeft ml >>= f) >>= g = MyLeft ml = MyLeft ml >>= (\x -> f x >>= g)
  --  m = MyRight mr, (MyRight mr >>= f) >>= g = f mr >>= g,
  --  MyRight mr >>= (\x f x >>= g) = f mr >>= g
