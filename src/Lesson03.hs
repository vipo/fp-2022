{-# LANGUAGE InstanceSigs #-}
module Lesson03
    ( 
    ) where

data IntList = IEmpty | IPrepend Int IntList deriving Show

data MyList a = MyEmpty | MyPrepend a (MyList a) deriving Show

type Map = [(String, Integer)]

empty :: Map
empty = []

add :: String -> Integer -> Map -> Map
add k v m = (k, v) : m

search :: String -> [(String, Integer)] -> Maybe Integer
search _ [] = Nothing
search k1 ((k,v):t) =
    if k1 == k then Just v else search k1 t


data BetterMaybe a = A a | Nothing1  deriving Show

foo :: a -> BetterMaybe a -> a
foo defaul Nothing1 = defaul
foo _ (A b) = b

foo' :: a -> BetterMaybe a -> a
foo' defaul bm =
    case bm of
        A b -> b
        Nothing1 -> defaul

search' :: Eq k => k -> [(k, v)] -> Maybe v
search' _ [] = Nothing
search' k1 ((k,v):t) =
    if k1 == k then Just v else search' k1 t

instance Eq a => Eq (BetterMaybe a ) where
    
  (A a1) == (A a2) = a1 == a2
  Nothing1 == Nothing1 = True
  _ == _ = False
    