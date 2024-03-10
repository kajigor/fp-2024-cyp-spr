### Either
```
newtype MyEither a b = MyEither (Either a b)

instance Functor (MyEither a) where
  fmap :: (a2 -> b) -> MyEither a1 a2 -> MyEither a1 b
  fmap f (MyEither (Right x)) = MyEither (Right (f x))
  fmap _ (MyEither (Left x))  = MyEither (Left x)
```
1. Identity
    fmap id (MyEither (Right x)) = MyEither (Right (id x)) = MyEither (Right x)
    fmap id (MyEither (Left x)) = MyEither (Left x)
2. Composition
    fmap f (fmap g MyEther (Right x)) = fmap f (MyEither (Right (g x))) = MyEither (Right (f (g x))) = fmap (f . g) (Right x)
    fmap f (fmap g MyEther (Left x)) = fmap f (MyEither (Left x)) = MyEither (Left x) = fmap (f . g) (Left x)

### Arrow
```
newtype MyArrow a b = MyArrow ((->) a b)

instance Functor (MyArrow a) where
  fmap :: (a2 -> b) -> MyArrow a1 a2 -> MyArrow a1 b
  fmap f (MyArrow g) = MyArrow (f . g)
```
1. Identity
    fmap id (MyArrow g) = MyArrow (id . g) = MyArrow g
2. Composition
    fmap f (fmap g (MyArrow h)) = fmap f (MyArrow (g . h)) = MyArrow (f . (g . h)) = MyArrow ((f . g) . h) = fmap (f . g) (MyArrow h)
