module Lesson04
    ( 
    ) where

filter' p xs = [ x | x <- xs, p x]

foo :: Int -> String
foo = show

bar :: Int -> Int -> Int
bar = (+)

addOne :: Int -> Int
addOne = (+) 1

data ErrorType = NotFound | EmptyKey deriving Show

search :: String -> [(String, Integer)] -> Either ErrorType Integer
search _ [] = Left NotFound
search "" _ = Left EmptyKey
search k1 ((k,v):t) =
    if k1 == k then Right v else search k1 t


