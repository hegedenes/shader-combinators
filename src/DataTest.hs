{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module DataTest where

data Value = Const Int | Add Value Value | Sub Value Value

onePlusTwo :: Value
onePlusTwo = Add (Const 1) (Const 2)

compute :: Value -> Int
compute (Const n) = n
compute (Add a b) = eval a + eval b
compute (Sub a b) = eval a - eval b

toStr :: Value -> String
toStr (Const n) = show n
toStr (Add a b) = toStr a ++ " + " ++ toStr b
toStr (Sub a b) = toStr a ++ " - " ++ toStr b

class Eval a where
  eval :: Value -> a

instance Eval Int where
  eval = compute
  
instance Eval String where
  eval = toStr

onePlusTwoInt :: Int
onePlusTwoInt = eval onePlusTwo -- = 3

onePlusTwoStr :: String
onePlusTwoStr = eval onePlusTwo -- = "1 + 2"
