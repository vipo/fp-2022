module Lesson11 () where

import Control.Monad(when)
import Control.Concurrent(forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Concurrent.Chan

import System.IO

m :: IO ()
m = do
    chan <- newChan
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    putStrLn "Labas"
    forkIO $ return ()--writeChan chan "!!!!!"
    putStrLn "medi"
    readChan chan >>= putStrLn

transfer :: TVar Integer -> TVar Integer -> Integer -> STM ()
transfer accA accB amount = do
    a <- readTVar accA
    b <- readTVar accB
    when (a - amount < 0) retry
    writeTVar accA (a - amount)
    writeTVar accB (b + amount)

m1 :: IO ()
m1 = do
    a <- newTVarIO 50
    b <- newTVarIO 50
    readTVarIO a >>= putStrLn . show
    readTVarIO b >>= putStrLn . show

    _ <- forkIO $ atomically $ modifyTVar a (+60)
    _ <- t1 a b
    
    return ()
    where
        t1 a b = forkIO $ do
            putStrLn "Trying to transfer"
            atomically $ transfer a b 100
            putStrLn "Transfered"
            readTVarIO a >>= putStrLn . show
            readTVarIO b >>= putStrLn . show
































--mapM_
--monoid

-- m :: IO ()
-- m = do
--     hSetBuffering stdin LineBuffering
--     hSetBuffering stdout LineBuffering
--     a <- newTVarIO 50
--     b <- newTVarIO 50
    
--     readTVarIO a >>= putStrLn . show
--     readTVarIO b >>= putStrLn . show

--     concurrently_ (t1 a b) (t2 a)
    
--     readTVarIO a >>= putStrLn . show
--     readTVarIO b >>= putStrLn . show
--     return ()
--     where
--         t1 a b = forkIO $ do
--             putStrLn "Trying to transfer"
--             atomically $ transfer a b 100
--             putStrLn "Transfered"
--         t2 a = atomically $ modifyTVar a (+60)

-- transfer :: TVar Integer -> TVar Integer -> Integer -> STM ()
-- transfer accA accB amount = do
--     a <- readTVar accA
--     b <- readTVar accB
--     when (a - amount < 0) retry
--     writeTVar accA (a - amount)
--     writeTVar accB (b + amount)

