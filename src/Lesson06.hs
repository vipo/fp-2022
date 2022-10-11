module Lesson06
    ( 
    ) where

import Text.Read
import Data.Char

g :: Either Int String
g = Left 6

h :: Either Int String
h = Left 7

ev :: Either Int String
ev = do
    y <- Right "penki"
    x <- g
    k <- h
    return k

ev' :: Either Int String
ev' = do
    y <- Right "penki"
    x <- g
    k <- h :: Either Int String
    Right k

ml :: [(Int, Char)]
ml = do
    a <- [1,2,3]
    b <- ['a', 'b']
    return (a, b)

ml' :: [(Int, Char)]
ml' = [1,2,3] >>= (\a -> (['a', 'b'] >>= (\b -> return (a, b))))

mm :: Maybe Int
mm = do
    a <- Just 5
    b <- Just 4
    return $ a * b

mm' :: Maybe Int
mm' = Just 5 >>= (\a -> (Just 4 >>= (\b -> return $ a * b)))

me :: Either Int String
me = do
    a <- Left 56
    b <- Right "labas"
    return "medis"

me' :: Either Int String
me' = Left 56 >>= (\a ->
        (Right "labas" >>= (\b ->
            return "medis"
        ))
    )

parseInteger' :: String -> Either String Integer
parseInteger' str = case (readMaybe str :: Maybe Integer) of
    Nothing -> Left "Not a int"
    Just i -> Right i

parseInteger :: String -> Either String (Integer, String)
parseInteger str =
    let 
        prefix = takeWhile isDigit str
    in
        case prefix of
            [] -> Left "Empty integer"
            _ -> Right (read prefix, drop (length prefix) str)

parseComma :: String -> Either String ((), String)
parseComma (',':x) = Right ((), x)
parseComma _ = Left "Comma expected"

-- "123,345" -> (123,345)
parsePair :: String -> Either String ((Integer, Integer), String)
parsePair str = do
    (i1, r1) <- parseInteger str
    (_, r2) <- parseComma r1
    (i2, r3) <- parseInteger r2
    return $ ((i1, i2), r3)