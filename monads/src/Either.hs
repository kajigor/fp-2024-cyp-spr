module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap f (MyRight a) = MyRight (f a)
  fmap _ (MyLeft l) = MyLeft l

{- proofs: 
  id - obviouse

  1)
  (fmap f . fmap g) (Right a) = fmap f (fmap g (Right a)) =fmap f (Right $ g a) = Right $ f. g $ a 

  (fmap f . fmap g) (Left a) = fmap f (fmap g (Left a)) = fmap f (Left a) = Left a

  2)
  fmap (f . g) (Right a) = Right $ f. g $ a

  fmap (f .g ) (Left a) = Left a

-}

-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure v = MyRight v

  (<*>) (MyLeft e) _ = MyLeft e
  (<*>) (MyRight f) (MyLeft e) = MyLeft e
  (<*>) (MyRight f) (MyRight a) = MyRight $ f a

{-  proof:

  !Identity:

  pure id <*> v = MyRight id <*> v = v

  !Composition:

  1) case u, v, w = Right u', Right u', Right w'

  pure (.) <*> u <*> v <*> w = MyRight (.) u' <*> v <*> w = MyRight u' . v' <*> w = MyRight u' $ v' w' = u <*> MyRight v' w' = u <*> (v <*> w)

  2) case u = Left ' 
          v, w = Right '

  pure (.) <*> u <*> v <*> w = Left u' =  u <*> (Any) = u <*> (v <*> w)

  3) case v = Left ' 
          u, w = Right '

  u <*> (v <*> w) = u <*> Left v' = Left v' = Right Any <*> v <*> Any = pure (.) <*> u <*> v <*> w

  4) case w = Left ' 
          u, v = Right '
    pure (.) <*> u <*> v <*> w = (pure (.) <*> u <*> v) <*> w = Right (...) <*> Left w' = Left w' = Right Any <*> w = v <*> w  = Right Any <*> (v <*> w) = u <*> (v <*> w)

  !Homomorphism: 

  pure f <*> pure g = Rigth f <*> Right g = Right f g = pure (f g)

  !Interchange: u <*> pure x = pure (\f -> f x) <*> u
  u <*> pure x = u <*> Right x = case u of Left l -> Left l | Right f -> Right f x
  pure (\f -> f x) <*> u = Right (\f -> f x) <*> u = case u of Left l -> Left l | Right f -> Right f x

-}

-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  return v = MyRight v

  (>>=) (MyLeft l) _ = MyLeft l
  (>>=) (MyRight v) f = f v

{- proof:
  1)  return a >>= k = (MyRight v) >>= k = k a
  2)  Left l >>= return = Left l 
      Right v >>= return = return v = Right v
  3)  m >>= (\x -> k x>>= v) = (m >>= k) >>= v 

      MyLeft l >>= (\x -> k x>>= v) = MyLeft l = MyLeft l >>= k = (MyLeft l >>= k) >>= v
      MyRight r >>= (\x -> k x>>= v) = k r >>= v = ((MyRight r) >>= k) >>= v
-}