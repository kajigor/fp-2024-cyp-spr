module Either where

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap _ (MyLeft left) = MyLeft left
  fmap func (MyRight right) = MyRight $ func right

{-
fmap id (MyLeft x) = MyLeft x
fmap id (MyRight x) = MyRight $ id x = MyRight $ x

fmap (f . g) (MyLeft x) = MyLeft x
fmap f (fmap g $ MyLeft x) = fmap f $ MyLeft x = MyLeft x
fmap (f . g) $ MyRight x = MyRight $ (f . g) x
fmap f (fmap g $ MyRight x) = fmap f (MyRight $ g x) = MyRight $ f (g x)
-}


-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure = MyRight
  (<*>) (MyRight func) (MyRight right) = MyRight $ func right
  (<*>) (MyLeft left) _ = MyLeft left
  (<*>) _ (MyLeft left) = MyLeft left

{-
pure id <*> MyLeft x = MyLeft x -- as pure generates MyRight
pure id <*> MyRight x = MyRight id <*> MyRight x = MyRight $ id x = MyRight x

pure f <*> pure x = pure $ f x -- first pattern of <*>

MyLeft left <*> pure x = MyLeft left
MyRight right <*> pure x = MyRight $ right x

pure ($ x) <*> MyLeft left = MyLeft left 
pure ($ x) <*> MyRight right = MyRight $ right x


pure (.) <*> MyRight x <*> MyRight y <*> MyRight z = MyRight $ ((.) x y) z = MyRight $ x (y z)
MyRight x <*> (MyRight y <*> MyRight z) = MyRight $ x (y z)

if some of the values is not MyRight then the leftmost MyLeft will be taken in both cases 

-}

-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  (>>=) (MyLeft left) _ = MyLeft left
  (>>=) (MyRight right) func = func right

{-
return a >>= h = h a -- second pattern

(MyLeft left) >>= return = MyLeft left
(MyRight right) >>= return = return right = MyRight right

((MyLeft left) >>= g) >>= h = MyLeft left = MyLeft left >>= (\x -> g x >>= h)

((MyRight right) >>= g) >>= h = g right >>= h
MyRight right >>= (\x -> g x >>= h) = g right >>= h
-}


