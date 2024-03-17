module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Laws were proved here: https://github.com/kajigor/fp-2024-cyp-spr/pull/63#discussion_r1512226116
instance Functor (MyEither err) where
    fmap :: (f -> t) -> MyEither err f -> MyEither err t
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight x) = MyRight (f x)

{-
Proof:
1) Identity: pure id <*> v === MyRight id <*> v === id v === v
2) Homomorphism: pure f <*> pure x === MyRight f <*> MyRight x === f <$> x === MyRight (f x) === pure (f x)
3) Interchange: u <*> pure y === pure ($ y) <*> u
Case 1: u == MyLeft x
  MyLeft x <*> pure y === MyLeft x
  pure ($ y) <*> MyLeft x === MyRight ($ y) <$> MyLeft x === MyLeft x
Case 2: u == MyRight x
  MyRight x <*> pure y === MyRight x <$> MyRight y === MyRight (x y)
  pure ($ y) <*> u === MyRight ($ y) <$> MyRight x === MyRight ($ y x) == MyRight (x y)
4) Composition: pure (.) <*> u <*> v <*> w === u <*> (v <*> w)
Case 1: u == MyLeft x
  pure (.) <*> MyLeft x <*> v <*> w === MyLeft x <*> _ === MyLeft x
  MyLeft x <*> (_) === MyLeft x
Case 2: u == MyRight x
  pure (.) <*> MyRight x <*> v <*> w === MyRight (. x) <*> v <*> w
  Subcase 2.1: v == MyLeft y
    MyRight (. x) <*> MyLeft y <*> w === MyLeft y
    MyRight x <*> (MyLeft y <*> w) === MyRight x <*> MyLeft y === MyLeft y
  Subcase 2.2 v == MyRight y
    MyRight (. x) <*> MyRight y <*> w === MyRight (x (.) y) <*> w === (x (.) y) <$> w
    MyRight x <*> (MyRight y <*> w) === MyRight x <*> y <$> w === x <$> y <$> w === (x (.) y) <$> w
-}
instance Applicative (MyEither a) where
  pure :: b -> MyEither a b
  pure = MyRight
  (<*>) :: MyEither a (b -> c) -> MyEither a b -> MyEither a c
  (<*>) (MyLeft err) _ = MyLeft err
  (<*>) (MyRight f) x = f <$> x

{-
Proof:
1) Left identity: return x >>= k === MyRight x >>= k === k x
2) Right identity: k >>= return 
Case 1: MyLeft err >>= return === MyLeft err
Case 2: MyRight val >>= return === MyRight val >>= \x -> return MyRight x === MyRight val
3) Associativity: m >>= (\x -> k x >>= h) =?= (m >>= k) >>= h
Case 1: MyLeft err >>= (\x -> k x >>= h) === MyLeft err >>= h === (MyLeft err >>= k) >>= h === MyLeft err >>= h === MyLeft err
Case 2: MyRight val >>= (\x -> k x >>= h) === k val >>= h === (MyRight val >>= k) >>= h === k val >>= h
-}
instance Monad (MyEither a) where
  return :: b -> MyEither a b 
  return = pure
  
  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  (>>=) (MyLeft err) _ = MyLeft err
  (>>=) (MyRight val) f = f val
