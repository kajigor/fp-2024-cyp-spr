module List where 

data List a 
  = Nil 
  | Cons a (List a)
  deriving (Show, Eq)

flattern Nil = Nil
flattern (Cons Nil ls) = flattern ls
flattern (Cons (Cons x xs) vs) = Cons x (flattern (Cons xs vs))

foldLeft f acc Nil = acc
foldLeft f acc (Cons a ls) = foldLeft f (f acc a) ls 

reverseList = foldLeft (flip Cons) Nil

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
  (<*>) fs xs = flattern $ reverseList $foldLeft (\lss f -> Cons (fmap f xs) lss) Nil fs 

-- Implement the instance and prove the laws
instance Monad List where
  return v = Cons v Nil
  (>>=) xs k = flattern $ fmap k xs

{-
 ProofFixed:
  1)return a >>=k = Cons a Nil >>=k = flatern $ Cons (k a) Nil = k a
  2)  [ai] >>= return = flattern $ [[ai]] = [ai]
  3)  m        >>= (\x -> k x >>= h)  =?  (m >>= k) >>= h
      3.1) Transforming m >>= (\x -> k x >>= h) 
      3.1.1) m >>= (\x -> k x >>= h) = m >>= (\x -> flattern (fmap h (k x))) = flattern $ fmap (\x -> flattern (fmap h (k x))) m
      3.1.2)  Consider fmap (\x -> flattern (fmap h (k x))) m !
              fmap (\x -> flattern (fmap h (k x)) m) = [fmap ( (\x -> flattern (fmap h x)) . k)] m  ==(by functor law)== [(fmap (\x -> flattern (fmap h x) ) ) . (fmap k) ] m = fmap (\x -> flattern (fmap h x) ) (fmap k m) = fmap (flattern . fmap h) (fmap k m)
      
              Therefore m >>= (\x -> k x >>= h) = flattern $ fmap (flattern . fmap h) (fmap k m) = (flattern . fmap (flattern . fmap h)) (fmap k m) ==(by functor law)== ( flattern . (fmap flatern . fmap (fmap h)) ) (fmap k m)
      3.2) Transforming (m >>= k) >>= h = (flattern $ fmap k m) >>= h = flattern $ fmap h (flattern $ fmap k m) = (flattern . fmap h . flattern) (fmap k m)
      3.3) So se need to proof ( flattern . (fmap flatern . fmap (fmap h)) ) = (flattern . fmap h . flattern)
           Lemma 1 : map h . flattern = flattern . map $ map h
           Lemma 2:  flattern . flattern = flattern . map flattern
           Proof: (flattern . fmap h . flattern) ==(by Lemma 1)== flatern . flatern .  map $ map h =  (flatern . flatern) .  (map $ map h)  = (flattern . map flattern) . (fmap $ fmap h) = ( flattern . (fmap flatern . fmap (fmap h)) )
-}


{-
Also we need to proof m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))) which implies that pure and <*> satisfy the applicative functor laws
Without formal proof The semantics of the <*> is that for all x1 from m1 and all x2 from m2 it returns (x1 x2). 
And m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))) do the same. For all x1 from m1 returns x1 x2 for all x2 from m2

-}