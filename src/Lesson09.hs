{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lesson09 () where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Gen

import Data.List as L

import Data.IORef

import Lesson08(Doc(..), parseDoc)

instance Arbitrary Doc where
    arbitrary = genDoc

genDoc :: Gen Doc
genDoc = oneof [genDocInt, genDocList]

genDocInt :: Gen Doc
genDocInt = do
    i <- arbitrary `suchThat` (>= 0)
    return $ DocInt i

genDocList :: Gen Doc
genDocList = do
    i <- arbitrary `suchThat` (>= 0)
    v <- vectorOf (min 2 i) genDoc
    return $ DocList v

prop1 :: Doc -> Bool
prop1 doc = doc == doc

prop2 :: Doc -> Bool
prop2 doc = parseDoc (render doc) == Right (doc, "")

render :: Doc -> String
render (DocInt i) = show i
render (DocList l) = concat ["[", intercalate "," (map render l), "]"]

main :: IO ()
main = do
    r <- newIORef 5
    v <- readIORef r
    modifyIORef r (+4)
    v <- readIORef r
    putStrLn $ show v

data Person = Person String String deriving Show

ioPerson :: IO Person
ioPerson = Person <$> getLine <*> getLine

