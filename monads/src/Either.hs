module Either where 

data MyEither a b 
  = MyLeft a 
  | MyRight b 
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (MyEither a)

-- Implement the instance and prove the laws
instance Applicative (MyEither a) 

-- Implement the instance and prove the laws
instance Monad (MyEither a)
