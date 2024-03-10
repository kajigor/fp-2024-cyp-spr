module Functors () where

data Eather_ a b = 
  Left_ a |
  Right_ b

instance Functor (Eather_ a) where
  fmap f (Right_ b) = Right_ $ f b
  fmap _ (Left_ b) = Left_ b


-- first impl.
data ArrowRR a b = ArrowRR (a -> b)

instance Functor (ArrowRR a) where 
  fmap f (ArrowRR t) = ArrowRR (f . t)
-- second impl
newtype ArrowR a b = ArrowR {getArrowR :: a -> b}

instance Functor (ArrowR a) where
  fmap f (ArrowR t) = ArrowR (f . t)
