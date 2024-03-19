module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a) where
  fmap f (MyRight x) = MyRight (f x)
  fmap _ y = y

-- Proof:
-- Law 1. fmap id (Right x) = Right (id x) = Right x
--
--        fmap id (Left x) = Left x
--
-- Law 2. fmap (f . g) (Right x) = Right (f(g x))
--        fmap f (fmap g (Right x)) = fmap f (Right (g x)) = Right (f(g x))
--   
--        fmap (f . g) (Left x) = Left x
--        fmap f (fmap g (Left x)) = fmap f (Left x) = Left x     


-- Implement the instance and prove the laws
instance Applicative (MyEither a) where
  pure = MyRight
  (Right f) <*> x = f <$> x
  x <*> _ = x

-- Proof:
-- Identity law:
-- pure id <*> v = Right id <*> v = id <$> v = v
-- 
-- Composition law:
-- pure (.) <*> u <*> v <*> w = Right (.) <*> u <*> v <*> w = (fmap (.) u) <*> v <*> w = 
-- u = Left x: = Left x <*> v <*> w = Left x = (Left x) <*> (v <*> w) = u <*> (v <*> w) 
-- u = Right x: = (Right $ (.) x) <*> v <*> w = fmap ((.) x) v <*> w = 
-- v = Left y: = fmap ((.) x) (Left y) <*> w = Left y <*> w = Left y = Right x <*> Left y = Right x <*> (Left y <*> w)
-- v = Right y: = fmap ((.) x) (Right y) <*> w = Right (x . y) <*> w = fmap (x . y) w = (fmap x) . (fmap y) w = (fmap x) ((Right y) <*> w) = Right x <*> (Right y <*> w) = u <*> (v <*> w)


-- Homomorphism law:
-- pure f <*> pure x = Right f <*> Right x = f <$> x = Right (f x) = pure f x

-- Interchange law:
-- 1. u = Left x: Left x <*> Right y = Left x; Right ($ y) <*> Left x = ($ y) <$> Left x = Left x
-- 2. u = Right x: Right x <*> Right y = x <$> Right y = Right x $ y; Right ($ y) <*> Right x = ($ y) <$> Right x = Right x $ y



-- Implement the instance and prove the laws
instance Monad (MyEither a) where
  (MyRight x) >>= f = f x
  x >>= _ = x

-- Proof:
-- Left identity law:
-- return a >>= h = Right a >>= h = h a

-- Right identity law:
-- 1. m = Left x: Left x >>= return = Left x
-- 2. m = Right x: Right x >>= return = return x = Right x

-- Associativity law:
-- 1. m = Left x: (Left x >>= g) >>= h = Left x = Left x >>= (\x -> g x >>= h)
-- 2. m = Right x: (Right x >> g) >>= h = g x >>= h = Right x >>= (\x -> g x >>= h)

