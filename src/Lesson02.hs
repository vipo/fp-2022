module Lesson02
    ( 
    ) where


l :: [a] -> Integer
l [] = 0
l (_:xs) = 1 + l xs


l' :: [a] -> Integer
l' a = l'' a 0
    where
        l'' :: [a] -> Integer -> Integer
        l'' [] acc = acc
        l'' (_:xs) acc = l'' xs (acc+1)

e :: Integer -> Bool
e a = a `mod` 2 == 0

data FireExtinguisherType = A | B | C

instance Show FireExtinguisherType where
    show C = "miltelinis"
    show A = "A"
    show B = "B"

data FireExtinguisher = FireExtinguisher {
    capacity :: Int,
    fet :: FireExtinguisherType
} deriving Show

f :: FireExtinguisher -> Bool
f (FireExtinguisher _ C) = True
f _ = False

fe = FireExtinguisher 1 C

data IntList = IEmpty | IPrepend Int IntList deriving Show