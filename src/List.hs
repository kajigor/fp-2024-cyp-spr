module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)


concatList Nil list = list
concatList (Cons head1 tail1) list2 = Cons head1 (concatList tail1 list2)


-- Implement the instance and prove the laws
instance Functor List where
  fmap function Nil = Nil
  fmap function (Cons head tail) = Cons (function head) (fmap function tail)


---- Implement the instance and prove the laws
instance Applicative List where
  pure el = Cons el Nil
  Nil <*> _ = Nil
  Cons headFunction tailFunction <*> Cons headItem tailItem = Cons (headFunction headItem) (tailFunction <*> tailItem)
  _ <*> _ = Nil


-- Implement the instance and prove the laws
instance Monad List where
  Nil >>= handler = Nil
  Cons head tail >>= f = concatList (f head) (tail >>= f)
