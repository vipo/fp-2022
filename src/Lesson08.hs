module Lesson08 (
    Doc(..),
    parseDoc
) where

import Lesson07(or, many1, parseChar, optional, parseInteger)

data Doc = DocList [Doc] | DocInt Integer deriving (Show, Eq)

-- 123 -> DocInt 123
-- [] -> DocList []
-- [123] -> DocList [DocInt 123]
-- [1234,123213] -> DocList [DocInt 1234, DocInt 123213]
-- [[]] -> DocList [DocList []]
-- [[],[]] -> DocList [DocList [], DocList []]
-- [1234, [123,33], [], 213] -> DocList [DocInt 1234, DocList[DocInt 123, DocInt 33], DocList[], DocInt 213]
parseDoc :: String -> Either String (Doc, String)
parseDoc str = Lesson07.or (parseDocList str) (parseDocInt str)

parseDocList :: String -> Either String (Doc, String)
parseDocList str = do
    (_, r1) <- parseChar '[' str
    (l, r2) <- optional r1 elems
    (_, r3) <- parseChar ']' r2
    case l of
        Just l' -> return (DocList l', r3)
        Nothing -> return (DocList [], r3)

elems :: String -> Either String ([Doc], String)
elems str = do
    (i, r) <- Lesson07.or (lenGt2List str) (len1List str)
    return (i, r)

len1List :: String -> Either String ([Doc], String)
len1List str = do
    (i, r) <- parseDoc str
    return ([i], r)

lenGt2List :: String -> Either String ([Doc], String)
lenGt2List str = do
    (i1, r1) <- parseDoc str
    (i2, r2) <- many1 r1 fromSecond
    return (i1:i2, r2)

fromSecond :: String -> Either String (Doc, String)
fromSecond str = do
    (_, r1) <- parseChar ',' str
    (i, r2) <- parseDoc r1
    return (i, r2)

parseDocInt' :: String -> Either String (Doc, String)
parseDocInt' str = do
    (i, r) <- Lesson07.parseInteger str
    return $ (DocInt i, r)

parseDocInt :: String -> Either String (Doc, String)
parseDocInt str = fmap (\(i, r) -> (DocInt i, r)) $ Lesson07.parseInteger str

data Person = Person String Int Int deriving Show

validateName = Right "Vardenis"
validateAge = Right 73
validateHeight = Right 185

validatePerson :: Either String Person
validatePerson = do
    name <- validateName
    age <- validateAge
    height <- validateHeight
    return $ Person name age height

validatePerson' :: Either String Person
validatePerson' = Person <$> validateName <*> validateAge <*> validateHeight
