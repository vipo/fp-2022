module Lesson01
    ( 
    ) where


x :: Integer
x = 5

b:: Bool
b = True

s :: String
s = "fsdfsdfsdfsd"

add :: Integer -> Integer -> Integer
add a b = a + b

isGrownUp :: Integer -> Bool
isGrownUp a = if a >= 20 then True else False

aTuple :: (Integer, String)
aTuple = (42, "Labas")

add' :: (Integer, Integer) -> Integer
add' t = add (fst t) (snd t)

add'' :: (Integer, Integer) -> Integer
add'' (f, s) = add f s

l :: [Integer]
l = [42, 43]

l1 :: [Integer]
l1 = 1 : l

l2 :: [Integer]
l2 = 3 : 4: l 

leng :: [a] -> Integer
leng [] = 0
leng (_:xs) = 1 + leng xs

