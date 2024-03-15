{-# LANGUAGE InstanceSigs #-}
module Functor() where

newtype MyEither a b = MyEither (Either a b) deriving (Show)

instance Functor (MyEither err) where
    fmap :: (f -> t) -> MyEither err f -> MyEither err t
    fmap _ (MyEither (Left x)) = MyEither(Left x)
    fmap f (MyEither (Right x)) = MyEither(Right (f x))


newtype MyArrow a b = MyArrow((->) a b)

instance Functor (MyArrow f) where
    fmap :: (a -> b) -> MyArrow f a -> MyArrow f b
    fmap f (MyArrow g)  = MyArrow (f . g)