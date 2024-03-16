module List where 

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

{-  proof
    [ID]  1) fmap id Nil = Nil = id Nil
          2) fmap id (Cons a ls) = Const (id a (fmap id ls)) = Cons a ls (? by by induction hypothesis for shorter list?) = id (Cons a ls)

    [COMPOSITION]
    By induction of list lenght
    Base case lenght 0 -> (fmap f . fmap g) Nil = fmap f (fmap g Nil) = fmap f Nil = Nil = fmap (f . g ) Nil
    Transition: 
    (fmap f . fmap g) (Const a ls) = fmap f (fmap g (Const a ls) ) = fmap f (Cons (g a) (fmap g ls )) = Cons (f $ g a) (fmap f (fmap g ls)) =(by inductive hypothesis) = Cons (f $ g a ) (fmap (f .g ) ls) = fmap (f .g) Cons (a ls)
-}

-- Implement the instance and prove the laws
instance Applicative List where 
  pure v = Cons v Nil
  (<*>) fs xs = flatten $ reverseList $foldLeft (\lss f -> Cons (fmap f xs) lss) Nil fs 
                where flatten Nil = Nil
                      flatten (Cons Nil ls) = flatten ls
                      flatten (Cons (Cons x xs) vs) = Cons x (flatten (Cons xs vs))

                      foldLeft f acc Nil = acc
                      foldLeft f acc (Cons a ls) = foldLeft f (f acc a) ls 

                      reverseList = foldLeft (flip Cons) Nil

-- Implement the instance and prove the laws
instance Monad List
