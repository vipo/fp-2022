module Lesson12 () where

import Data.Char(isDigit)

import Control.Monad(liftM)
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Monoid((<>))
import Test.QuickCheck.Text (Str)


data Person = Person {
    name :: String,
    age :: Int
} deriving Show

newtype State s a = State {
    runState :: s -> (a, s)
}

put :: s -> State s ()
put a = State $ \_ -> ((), a)

get :: State s s
get = State $ \s -> (s, s)

stateful :: State String Int
stateful = do
    s <- get
    put $ s ++ s
    return $ length s

stateful2 :: State String ()
stateful2 = do
    _ <- stateful
    _ <-stateful
    return ()

create :: a -> State String a
create v = pure v

instance Functor (State s) where
    fmap f functor = State $ \oldState ->
        let (v, newState) = runState functor oldState
        in (f v, newState)

instance Applicative (State s) where
    pure v = State $ \s -> (v, s)
    ff <*> fa = State $ \oldState ->
        let (f, newState1) = runState ff oldState
            (v, newState2) = runState fa newState1
        in (f v, newState2)

instance Monad (State s) where
    ma >>= fm = State $ \oldState ->
        let (a, newState) = runState ma oldState
        in runState (fm a) newState









-- newtype State s a = State {
--     runState :: s -> (a, s) 
-- }

-- get :: State s s
-- get = State $ \s -> (s, s)

-- put :: s -> State s ()
-- put s = State $ \_ -> ((), s)

-- statefull :: State String Int
-- statefull = do
--     s <- get
--     put $ s ++ s
--     return $ length s

-- create :: a -> State s a
-- --create a = State $ \s -> (a, s) 
-- create a = pure a

-- instance Functor (State s) where
--     fmap f functor = State $ \oldState ->
--          let (v, newState) = runState functor oldState
--          in (f v, newState)

-- instance Applicative (State s) where
--     pure x = State $ \s -> (x, s)
--     ff <*> fa = State $ \oldState ->
--         let (f, newState1) = runState ff oldState
--             (v, newState2) = runState fa newState1
--         in (f v, newState2) 

-- instance Monad (State s) where
--     ma >>= mf = State $ \oldState ->
--         let (a, newState) = runState ma oldState
--         in runState (mf a) newState


-- newtype EitherT e m a = EitherT {
--     runEitherT :: m (Either e a)
-- }

-- instance MonadTrans (EitherT e) where
--     lift ma = EitherT $ fmap Right ma

-- instance Monad m => Functor (EitherT e m) where
--     fmap f = EitherT . liftM (fmap f) . runEitherT

-- instance Monad m => Applicative (EitherT e m) where
--     pure a = EitherT $ return $ Right a
--     EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
--         Left  e -> return (Left e)
--         Right k -> v >>= \mv -> case mv of
--             Left  e -> return (Left e)
--             Right x -> return (Right (k x))

-- instance Monad m => Monad (EitherT e m) where
--     m >>= k = EitherT $ do
--         a <- runEitherT m
--         case a of
--             Left  l -> return (Left l)
--             Right r -> runEitherT (k r)

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