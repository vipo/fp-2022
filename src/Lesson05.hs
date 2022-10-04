module Lesson05
    ( 
    ) where

lc :: [Int]
lc = [x * y | x <- [1,2,3,4], y <- [-1, 1]]

lv :: [Int]
lv = do
    x <- [1,2,3,4]
    y <- [-1, 1]
    return (x * y)

lv' :: [Int]
lv' = do
    x <- [1,2,3,4]
    y <- []
    return (x * y)

mv :: Maybe Int
mv = do
    x <- Just 5
    y <- Just 6
    return $ x * y

mvn :: Maybe Int -> Maybe Int -> Maybe Int
mvn (Just x) (Just y) = Just $ x * y
mvn _ _ = Nothing

mvn' :: Maybe Int
mvn' = case (mvn (Just 5) (Just 1)) of
        Nothing -> Nothing
        Just x -> case (Just 6) of
                    Nothing -> Nothing
                    Just y -> Just (x * y)

mv' :: Maybe Int
mv' = do
    x <- Just 5
    y <- Nothing
    return $ x * y

mv'' :: Maybe Int
mv'' = do
    _ <- Nothing
    x <- Just 5
    y <- Just 6
    z <- Nothing
    return $ x * y

ev :: Either String Int
ev = do
    x <- Right 5
    y <- Right 6
    return $ x * y

g :: Either Int String
g = Left 6

h :: Either Int String
h = Left 7

ev' :: Either Int String
ev' = do
    y <- Right "penki"
    z <- Left 7
    x <- g
    k <- h
    return $ k