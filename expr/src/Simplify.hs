module Simplify where

import Expr


manageArithmetics :: (Floating a, Eq a, Show a) => (Expr a -> Expr a -> Expr a) -> [(Maybe (Expr a), Maybe (Expr a), Maybe (Expr a))] -> Expr a -> Expr a -> Expr a
manageArithmetics op patterns x y =
  let simpleX = simplify x
      simpleY = simplify y
  in let  matchPattern (Just p1, _, Just val) = if simpleX == p1 then Just val else Nothing
          matchPattern (_, Just p2, Just val) = if simpleY == p2 then Just val else Nothing
          matchPattern (Just p1, _, _) = if p1 == simpleX then Just simpleY else Nothing
          matchPattern (_, Just p2, _) = if p2 == simpleY then Just simpleX else Nothing
          matchPattern _ = Nothing
  in let matchedPatterns = filter (Nothing /=) (map matchPattern patterns)
  in case matchedPatterns of
    [] -> op simpleX simpleY
    (Just res):_ -> res
    _ -> Expr 0.0

simplify :: (Floating a, Eq a, Show a) => Expr a -> Expr a
simplify (Expr a) = Expr a
simplify (Sq a) = Sq a
simplify (Var x) = Var x
simplify ((:-) x y) = if x == y then Expr 0.0 else simplify x :- simplify y
simplify (x :+ y) = manageArithmetics (:+) [(Just (Expr 0.0), Nothing, Nothing), (Nothing, Just (Expr 0.0), Nothing)] x y
simplify (x :* y) = manageArithmetics (:*) [(Just (Expr 0.0), Nothing, Just (Expr 0.0))
                                          , (Nothing, Just (Expr 0.0), Just (Expr 0.0))
                                          , (Just (Expr 1.0), Nothing, Nothing)
                                          , (Nothing, Just (Expr 1.0), Nothing)] x y
simplify (x :/ y) = manageArithmetics (:/) [(Nothing, Just (Expr 1.0), Nothing)] x y
simplify (x :^ y) = manageArithmetics (:^) [(Nothing, Just (Expr 1.0), Nothing)] x y