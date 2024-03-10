{-# LANGUAGE InstanceSigs #-}
module Operations (Operation(..)) where

data Operation = Plus 
  | Minus 
  | Mult 
  | Div 
  | Pow 

instance Show Operation where 
  show :: Operation -> String
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "
  show Pow  = " ^ "

instance Eq Operation where 
  (==) :: Operation -> Operation -> Bool
  Plus == Plus = True
  Minus == Minus = True
  Mult == Mult = True
  Div == Div = True
  Pow == Pow = True
  _ == _ = False