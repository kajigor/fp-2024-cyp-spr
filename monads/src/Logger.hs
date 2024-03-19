module Logger(Logger(..), factLog) where 

data Logger l a = Logger [l] a deriving (Show, Eq)

-- Implement the instance and prove the laws
instance Functor (Logger l) where
    fmap func (Logger ls x) = Logger ls $ func x

{-
fmap id (Logger ls x) = Logger ls $ id x = Logger ls x

fmap (f . g) (Logger ls x) = Logger ls $ id x = Logger ls x
fmap f (fmap g $ Logger ls x) = fmap f (Logger ls $ g x) = Logger ls $ f (g x)
-}

-- Implement the instance and prove the laws
instance Applicative (Logger l) where
  pure = Logger []
  (<*>) (Logger funcLog func) (Logger elemLog element) = Logger (funcLog ++ elemLog) $ func element

{-
pure id <*> (Logger elemLog elem) = (Logger [] id) <*> (Logger elemLog elem) = Logger ([] ++ elemLog) (id elem)
= Logger elemLog elem

pure f <*> pure x = Logger [] f <*> Logger [] x = Logger ([] ++ []) $ f x = Logger [] $ f x = pure $ f x

Logger funcLog func <*> pure y = Logger (funcLog ++ []) (func y) = Logger funcLog $ func y
Logger [] ($ y) <*> Logger funcLog func = Logger ([] ++ funcLog) (func y) = Logger funcLog $ func y

Logger uLog u <*> (Logger vLog v <*> Logger wLog w) = Logger uLog u <*> Logger (vLog ++ wLog) (v w) 
= Logger (uLog ++ (vLog ++ wLog)) (u (v w))
Logger [] (.) <*> Logger uLog u <*> Logger vLog v <*> Logger wLog w 
= Logger uLog (\t x -> u $ t x) <*> Logger vLog v <*> Logger wLog w
= Logger (uLog ++ vLog) (\x -> u $ v x) <*> Logger wLog w
= Logger ((uLog ++ vLog) ++ wLog) (u $ v w)


-}


-- Implement the instance and prove the laws
instance Monad (Logger l) where
  (>>=) (Logger elemLog element) func = case func element of
    Logger resultLog result -> Logger (elemLog ++ resultLog) result

{-
if h a = Logger logRes res, then
return a >>= h = (Logger [] a) >>= h = Logger ([] ++ logRes) res = Logger logRes res

(Logger elemLog elem) >>= return = Logger (elemLog ++ []) elem = Logger elemLog elem

assume g elemLog = Logger gLog gRes
assume h gRes = Logger hLog hRes
(Logger elemLog elem >>= g) >>= h =Logger (elemLog ++ gLog) gRes >>=  h = Logger (elemLog ++ gLog ++ hLog) hRes

g elem >>= h = Logger (gLog ++ hLog) hRes
(Logger elemLog elem) >>= (\x -> g x >>= h) = Logger (elemLog ++ (gLog ++ hLog)) hRes
-}


-- Writes a single log message. 
-- Can be easily bound together with other logging computations.
writeLog :: l -> Logger l ()
writeLog l = Logger [l] () 

-- Logs every intermediate result 
-- ghci> factLog 5
-- Logger [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120)] 120
factLog :: Int -> Logger (Int, Int) Int 
factLog n 
  | n <= 0 = do 
      let res = 1 
      writeLog (n, res)
      return res 
  | otherwise = do 
      prev <- factLog (n - 1)
      let res = n * prev 
      writeLog (n, res)
      return res 

          