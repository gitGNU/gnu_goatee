-- | Common utilities uesd throughout the project.
module Khumba.GoHS.Common ( listReplace
                          , listUpdate
                          , fromLeft
                          , fromRight
                          , onLeft
                          , onRight
                          , andEithers
                          , mapTuple
                          , whenMaybe
                          , cond
                          , orM
                          , whileM
                          ) where

import Control.Arrow ((***))
import Control.Monad (join, when)
import Data.Either (partitionEithers)

listReplace :: Eq a => a -> a -> [a] -> [a]
listReplace from to = map $ replace from to
  where replace from to x = if x == from then to else x

listUpdate :: Show a => (a -> a) -> Int -> [a] -> [a]
listUpdate fn ix xs = listSet' ix xs
  where listSet' 0 (x':xs') = fn x':xs'
        listSet' ix' (x':xs') = x':listSet' (ix' - 1) xs'
        listSet' _ _ = error ("Cannot update index " ++ show ix ++
                              " of list " ++ show xs ++ ".")

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft failed."

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight failed."

onLeft :: (a -> c) -> Either a b -> Either c b
f `onLeft` e = case e of
  Left x -> Left $ f x
  Right y -> Right y

onRight :: (b -> c) -> Either a b -> Either a c
onRight = fmap

-- | If any item is a 'Left', then the list of 'Left's is returned, otherwise
-- the list of 'Right's is returned.
andEithers :: [Either a b] -> Either [a] [b]
andEithers xs = let (as, bs) = partitionEithers xs
                in if not $ null as then Left as else Right bs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe (return ())

cond :: a -> [(Bool, a)] -> a
cond fallback ((test, body):rest) = if test then body else cond fallback rest
cond fallback _ = fallback

orM :: Monad m => [m Bool] -> m Bool
orM (m:ms) = do x <- m
                if x then return True else orM ms
orM _ = return False

whileM :: Monad m => m Bool -> m () -> m ()
whileM test body = do x <- test
                      when x $ body >> whileM test body
