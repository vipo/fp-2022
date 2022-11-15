{-# LANGUAGE FlexibleInstances #-}

module Lesson10 () where

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import Control.Applicative((<|>), some, many, Alternative)
import Data.Char

statefull :: State String Int
statefull = do
    s <- get
    put $ s ++ s
    return $ length s

statefull' :: State String String
statefull' = do
    put "Labas"
    return "medis"

comp :: State String String
comp = do
    s <- statefull'
    _ <- statefull
    return s

type Parser a = ExceptT String (State String) a

data Doc = DocList [Doc] | DocInt Integer deriving (Show, Eq)

-- parseDoc :: Parser Doc
-- parseDoc = parseDocList <|> parseDocInt

-- parseDocInt :: Parser Doc
-- parseDocInt = do
--     str <- lift get
--     let prefix = takeWhile isDigit str
--     lift $ put $ drop (length prefix) str
--     case prefix of
--         [] -> throwE "Empty integer"
--         _ -> return $ DocInt $ read prefix

-- parseDocList :: Parser Doc
-- parseDocList = do
--     str <- lift get
--     _ <- parseChar '['
--     l <- many elems
--     _ <- parseChar ']'
--     case l of
--         [] -> return $ DocList []
--         l' -> return $ DocList (concat l')

-- elems :: Parser [Doc]
-- elems = lenGt2List <|> len1List

-- len1List :: Parser [Doc]
-- len1List = do
--     i <- parseDoc
--     return [i]

-- lenGt2List :: Parser [Doc]
-- lenGt2List = do
--     i1 <- parseDoc
--     i2 <- some fromSecond
--     return $ i1:i2

-- fromSecond :: Parser Doc
-- fromSecond = do
--     _ <- parseChar ','
--     i <- parseDoc
--     return i

-- parseChar :: Char -> Parser Char
-- parseChar ch = do
--     str <- lift get
--     case str of
--         [] -> throwE $ "Empty input: '" ++ [ch] ++ "' expected"
--         (x:xs) | ch == x -> lift (put xs) >> return x
--                | otherwise -> throwE $ ch :" expected"






























-- statefull :: State String Int
-- statefull = do
--     s <- get
--     put "ohhh"
--     return $ length s

-- data Doc = DocList [Doc] | DocInt Integer deriving (Show, Eq)

-- type Parser a = ExceptT String (State String) a 

-- parse str = runState ( runExceptT parseDoc) str

parseDoc :: Parser Doc
parseDoc = parseDocInt <|> parseDocList

parseDocInt :: Parser Doc
parseDocInt = do
    str <- lift get
    let prefix = takeWhile isDigit str
    case prefix of
        [] -> throwE "Empty integer"
        _ -> do
            lift $ put $ drop (length prefix) str 
            return $ DocInt $ read prefix

parseDocList :: Parser Doc
parseDocList = do
    _ <- parseChar '['
    l <- many elems
    _ <- parseChar ']'
    case l of
        [] -> return $ DocList []
        l' -> return $ DocList $ concat l'
        _ -> throwE "Unexpected"

elems :: Parser [Doc]
elems = lenGt2List <|> len1List

len1List :: Parser [Doc]
len1List = do
    d <- parseDoc
    return [d]

lenGt2List :: Parser [Doc]
lenGt2List = do
    i1 <- parseDoc
    i2 <- many fromSecond
    return $ i1:i2

fromSecond :: Parser Doc
fromSecond = do
    _ <- parseChar ',' 
    i <- parseDoc
    return i

parseChar :: Char -> Parser Char
parseChar ch = do
    str <- lift get
    case str of
        [] -> throwE $ "Empty input: '" ++ [ch] ++ "' expected"
        (x:xs) | ch == x -> lift $ put xs >> return x
               | otherwise -> throwE $ ch :" expected"

type ParserIO a = ExceptT String (StateT String IO) a

parIO :: ParserIO Int
parIO = do
    s <- lift get
    liftIO $ putStrLn s
    lift $ put "medis"
    throwE "oh"
    return 5


instance Alternative (Either String) where
    Right a <|> _ = Right a
    Left _ <|> Right a = Right a
    Left e1 <|> Left _ = Left e1
