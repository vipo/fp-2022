
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lesson13 (EitherT(..), runEitherT, throwError) where

import Data.Char(isDigit)

import Control.Monad(liftM)
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Monoid((<>))
import Test.QuickCheck.Text (Str)

import Lesson12(State, put, get)

data Exp = Lit Int
            | Neg Exp
            | Add Exp Exp
            | Mul Exp Exp  deriving Show

e :: Exp
e = Mul (Add (Lit 10) (Neg (Lit 4))) (Lit 42)

eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e) = - eval e
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

printE :: Exp -> String
printE (Lit n) = show n
printE (Neg e) = "(-" ++ printE e ++ ")"
printE (Add e1 e2) = "(" ++ printE e1 ++ "+" ++ printE e2 ++ ")"
printE (Mul e1 e2) = "(" ++ printE e1 ++ "*" ++ printE e2 ++ ")"

class ExpTF r where
    lit :: Int -> r
    neg :: r -> r
    add :: r -> r -> r
    mul :: r -> r -> r

instance ExpTF Int where
    lit n = n
    neg e = -e
    add e1 e2 = e1 + e2
    mul e1 e2 = e1 * e2

instance ExpTF String where
    lit n = show n
    neg e = "(-" ++ e ++ ")"
    add e1 e2 = "(" ++ e1 ++ "+" ++ e2 ++ ")"

e2 :: String
e2 =  mul (add (lit 10) (neg (lit 4))) (lit 42)


newtype EitherT e m a = EitherT {
    runEitherT :: m (Either e a)
}

instance MonadTrans (EitherT e) where
    lift ma = EitherT $ fmap Right ma

instance Monad m => Functor (EitherT e m) where
    fmap f = EitherT . liftM (fmap f) . runEitherT

instance Monad m => Applicative (EitherT e m) where
    pure a = EitherT $ return $ Right a
    EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
        Left  e -> return (Left e)
        Right k -> v >>= \mv -> case mv of
            Left  e -> return (Left e)
            Right x -> return (Right (k x))

instance Monad m => Monad (EitherT e m) where
    m >>= k = EitherT $ do
        a <- runEitherT m
        case a of
            Left  l -> return (Left l)
            Right r -> runEitherT (k r)


throwError :: Monad m => e -> EitherT e m a
throwError e = EitherT $ return $ Left e

-- newtype ParseError = ParseError String deriving Show

-- type Parser a = EitherT ParseError (State String) a

-- throwError :: Monad m => e -> EitherT e m a
-- throwError e = EitherT $ return $ Left e

-- parseInt :: Parser Int
-- parseInt = do
--     str <- lift get
--     let prefix = takeWhile isDigit str
--     case prefix of
--         [] -> throwError $ ParseError "Empty integer"
--         _ -> do
--             lift $ put $ drop (length prefix) str 
--             return $ read prefix

-- instance (Monad m, Monoid e) => Alternative (EitherT e m) where
--   EitherT m <|> EitherT n = EitherT $ m >>= \a -> case a of
--     Left l -> liftM (\b -> case b of
--       Left l' -> Left (mappend l l')
--       Right r -> Right r) n
--     Right r -> return (Right r)
--   empty = EitherT $ return (Left mempty)

-- instance Semigroup ParseError where
--     (ParseError s1) <> (ParseError s2) = ParseError $ concat [s2, " (before failed with: ", s2, ")"]

-- instance Monoid ParseError where
--     mempty = ParseError ""

-- p :: Parser Int
-- p = parseInt <|> parseInt


-- class ExpSYM repr where
--     lit :: Int -> repr
--     neg :: repr -> repr
--     add :: repr -> repr -> repr

-- instance ExpSYM Int where
--     lit n = n
--     neg e = - e
--     add e1 e2 = e1 + e2

-- instance ExpSYM String where
--     lit n = show n
--     neg e = "(-" ++ e ++ ")"
--     add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"

-- --f1 = add (lit 8) (neg (add (lit 1) (lit 2)))