module List where 

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List 

-- Implement the instance and prove the laws
instance Applicative List

-- Implement the instance and prove the laws
instance Monad List
