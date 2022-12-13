
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lesson14 () where

import Data.Char(isDigit)

import Control.Monad(liftM)
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Monoid((<>))
import qualified Data.List as L

import Lesson12(State, runState, put, get)
import Lesson13(EitherT(..), runEitherT, throwError)

newtype ParseError = ParseError String deriving Show

type Parser a = EitherT ParseError (State String) a

parseInt :: Parser Int
parseInt = do
    str <- lift get
    let prefix = takeWhile isDigit str
    case prefix of
        [] -> throwError $ ParseError "Empty integer"
        _ -> do
            lift $ put $ drop (length prefix) str 
            return $ read prefix

instance (Monad m, Monoid e) => Alternative (EitherT e m) where
  EitherT m <|> EitherT n = EitherT $ m >>= \a -> case a of
    Left l -> liftM (\b -> case b of
      Left l' -> Left (mappend l l')
      Right r -> Right r) n
    Right r -> return (Right r)
  empty = EitherT $ return (Left mempty)

instance Semigroup ParseError where
    (ParseError s1) <> (ParseError s2) = ParseError $ concat [s2, " (before failed with: ", s1, ")"]

instance Monoid ParseError where
    mempty = ParseError "(no input)"

p :: Parser Int
p = parseInt <|> parseInt

data Term = Var String | Abs String Term | App Term Term

instance Show Term where
  show (Var n) = n
  show (Abs n t) = concat ["(\\ ", n, ".", show t, ")"]
  show (App t1 t2) = concat ["(", show t1, " ", show t2, ")"]

tru :: Term
tru = Abs "t" (Abs "f" (Var "t"))

fls :: Term
fls = Abs "t" (Abs "f" (Var "f"))

apps :: [Term] -> Term
apps [] = error "Empty application"
apps [_] = error "Two term needed to application"
apps (t1 : t2 : ts) = apps' (App t1 t2) ts
  where
    apps' t [] = t
    apps' t (x : xs) = apps' (App t x) xs

land :: Term
land = Abs "b" $ Abs "c" $ apps [Var "b", Var "c", fls]

-- lor :: Term
-- lor = Abs "b" $ Abs "c" $ apps [Var "b", Var "b", Var "c"]

-- test :: Term -- \l. \m. \n. (l m) n
-- test = Abs "l" $ Abs "m" $ Abs "n" $ apps [Var "l", Var "m", Var "n"]

-- -- (\x. \y. (x \x. x)) z

-- -- de Bruijn "de brown"
-- -- \x.x -> \.0
-- -- \x.\y. x (y x) -> \.\. 1 (0 1)

-- -- (x y) -> (1 2) assuming [(x, 1), (y, 2), (z, 0)]
-- -- \w. z w -> \. 1 0 assuming [(x, 1), (y, 2), (z, 0)]

data ITerm
  = IVar Int
  | IAbs ITerm
  | IApp ITerm ITerm
  deriving (Eq)

instance Show ITerm where
  show (IVar i) = show i
  show (IAbs t) = concat ["(\\.", show t, ")"]
  show (IApp t1 t2) = concat ["(", show t1, " ", show t2, ")"]

deBruijnIndices :: [String] -> Term -> ITerm
deBruijnIndices ctx t = walk [] t
  where
    walk stack (Var n) = IVar (findInd stack n)
    walk stack (Abs n t) = IAbs (walk (n : stack) t)
    walk stack (App t1 t2) = IApp (walk stack t1) (walk stack t2)
    findInd stack n =
      case (n `L.elemIndex` stack, n `L.elemIndex` ctx) of
        (Just i, _) -> i
        (Nothing, Just i) -> L.length stack + i
        _ -> error $ "No index for free variable " ++ n

-- termShift :: Int -> ITerm -> ITerm
-- termShift d = walk 0
--   where
--     walk c (IVar x)
--       | x >= c = IVar (x + d)
--       | otherwise = IVar x
--     walk c (IAbs t') = IAbs (walk (c + 1) t')
--     walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)

-- termSubst :: ITerm -> ITerm -> ITerm
-- termSubst s = walk 0
--   where
--     walk c (IVar x)
--       | x == c = termShift c s
--       | otherwise = IVar x
--     walk c (IAbs t') = IAbs (walk (c + 1) t')
--     walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)

-- termSubstTop :: ITerm -> ITerm -> ITerm
-- termSubstTop s t = termShift (-1) (termSubst (termShift 1 s) t)

-- eval :: ITerm -> ITerm
-- eval (IApp (IAbs t') v2) = termSubstTop v2 t'
-- eval (IApp v1 v2) = IApp (eval v1) (eval v2)
-- eval t = t

-- fullEval :: ITerm -> ITerm
-- fullEval t = fullEval' t $ eval t
--   where
--     fullEval' t1 t2 | t1 == t2 = t2
--     fullEval' _ t2 = fullEval' t2 $ eval t2