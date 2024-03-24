module Proofs () where


-- 1. HW04: (1 point) What is the `Functor` instance of `Either`? Implement the instance and prove that the functor laws hold.
newtype MyEither a b = MyEither (Either a b)

instance Functor (MyEither a) where
    fmap f (MyEither (Right x)) = MyEither (Right (f x))
    fmap _ (MyEither (Left x)) = MyEither (Left x)

{-
Proof: we need to check 2 conditions:
1. fmap id == id
2. fmap (f . g) == fmap f . fmap g
Let's check the first one: fmap id == id
  fmap f (Right x) = Right (f x) <=> fmap id (Right x) = Right (id x) <=> fmap (Right x) = Right x
  fmap f (Left x) = Left x

Let's check the second one:
  fmap (f . g) (Right x) == Right ((f . g) x) = Right (f (g(x))
  fmap (f . g) (Left x) = Left x
  (fmap f . fmap g) (Right x) = fmap f (fmap g (Right x))
                              = fmap f (Right (g x))
                              = Right (f (g x))
 (fmap f . fmap g) (Left x) = fmap f (fmap g (Left x))
                            = fmap f (Left x)
                            = Left x
what was needed to prove that the functor laws hold
-}


{-
HW04: 2. (1 point) What is the `Functor` instance of an arrow type? Hint: consider the type `a -> b` in its prefix notation: `(->) a b`.
Implement the instance and prove that the functor laws hold.
-}
newtype MyFunction a b = MyFunction (a -> b)
instance Functor (MyFunction a) where
    fmap f (MyFunction g) = MyFunction (f . g)

{-
 Proof:
 fmap id g = (\x -> id (g x))
          = (\x -> g x)
          = g
 fmap (f . g) h = (\x -> (f . g) (h x))
               = (\x -> f (g (h x)))
               = fmap f (g . h)
               = (fmap f) . (g . h)
 So fmap (f . g) = fmap f . fmap g holds
-}
