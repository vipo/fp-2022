module Lesson07
    (
        Lesson07.or,
        many1,
        parseChar,
        optional,
        parseInteger
    ) where

import Text.Read
import Data.Char
import GHC.Conc (par, retry)

parseInteger :: String -> Either String (Integer, String)
parseInteger str =
    let 
        prefix = takeWhile isDigit str
    in
        case prefix of
            [] -> Left "Empty integer"
            _ -> Right (read prefix, drop (length prefix) str)

-- "[]" -> []
-- "[1,23]" -> [1,23]
parseListOfInt :: String -> Either String ([Integer], String)
parseListOfInt str = do
    (_, r1) <- parseChar '[' str
    (l, r2) <- optional r1 elems
    (_, r3) <- parseChar ']' r2
    case l of
        Just l' -> return (l', r3)
        Nothing -> return ([], r3)

elems :: String -> Either String ([Integer], String)
elems str = do
    (i, r) <- Lesson07.or (lenGt2List str) (len1List str)
    return (i, r)

len1List :: String -> Either String ([Integer], String)
len1List str = do
    (i, r) <- parseInteger str
    return ([i], r)

lenGt2List :: String -> Either String ([Integer], String)
lenGt2List str = do
    (i1, r1) <- parseInteger str
    (i2, r2) <- many1 r1 fromSecond
    return (i1:i2, r2)

fromSecond :: String -> Either String (Integer, String)
fromSecond str = do
    (_, r1) <- parseChar ',' str
    (i, r2) <- parseInteger r1
    return (i, r2)

many1 :: String -> (String -> Either String (a, String)) -> Either String ([a], String)
many1 str parser = many1' str []
    where
        many1' s [] =
             case parser s of
                Left e -> Left e
                Right (i, r) -> many1' r [i]
        many1' s acc =
            case parser s of
                Left e -> Right (reverse acc, s)
                Right (i, r) -> many1' r (i:acc)

optional :: String -> (String -> Either String (a, String)) -> Either String (Maybe a, String)
optional str parser =
    case parser str of
        Left e -> Right (Nothing, str)
        Right (i, r) -> Right (Just i, r)

or :: Either String (a, String)
    -> Either String (a, String)
    -> Either String (a, String)
or parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

parseChar :: Char -> String -> Either String (Char, String)
parseChar ch [] = Left $ "Empty input: '" ++ [ch] ++ "' expected"
parseChar ch (x:xs) | ch == x = Right (x, xs)
                    | otherwise = Left $ ch :" expected"