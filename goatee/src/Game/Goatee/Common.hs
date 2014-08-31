-- This file is part of Goatee.
--
-- Copyright 2014 Bryan Gardiner
--
-- Goatee is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Goatee is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Goatee.  If not, see <http://www.gnu.org/licenses/>.

-- | Common utilities used throughout the project.
module Game.Goatee.Common (
  listDeleteAt,
  listInsertAt,
  listReplace,
  listUpdate,
  andEithers,
  for,
  mapTuple,
  mapInvert,
  whenMaybe,
  cond,
  if',
  andM,
  forIndexM_,
  whileM,
  whileM',
  doWhileM,
  ) where

import Control.Arrow ((***))
import Control.Monad (forM_, join, when)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Map (Map)

-- | Drops the element at an index from a list.  If the index is out of bounds
-- then the list is returned unmodified.
listDeleteAt :: Int -> [a] -> [a]
listDeleteAt index list = take index list ++ drop (index + 1) list

-- | Inserts the element into the list before the given position.  If the
-- position is less than 0 or greater than the length of the list, then the
-- index is clamped to this range.
listInsertAt :: Int -> a -> [a] -> [a]
listInsertAt index x xs =
  let (before, after) = splitAt index xs
  in before ++ x : after

-- | @listReplace old new list@ replaces all occurrences of @old@ with @new@ in
-- @list@.
listReplace :: Eq a => a -> a -> [a] -> [a]
listReplace from to = map $ replace from to
  where replace from to x = if x == from then to else x

-- | Modifies the element at a specific index in a list.
listUpdate :: Show a => (a -> a) -> Int -> [a] -> [a]
listUpdate fn ix xs = listSet' ix xs
  where listSet' 0 (x':xs') = fn x':xs'
        listSet' ix' (x':xs') = x':listSet' (ix' - 1) xs'
        listSet' _ _ = error ("Cannot update index " ++ show ix ++
                              " of list " ++ show xs ++ ".")

-- | If any item is a 'Left', then the list of 'Left's is returned, otherwise
-- the list of 'Right's is returned.
andEithers :: [Either a b] -> Either [a] [b]
andEithers xs = let (as, bs) = partitionEithers xs
                in if not $ null as then Left as else Right bs

-- | @for@ is @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Transforms both values in a homogeneous tuple.
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

-- | Inverts a map, collecting all of the keys that map to a single value in one
-- list in the result map.  No guarantees are made on the order of the keys in
-- each value's list.  If you want the results in ascending order, apply
-- @'Data.Map.map' 'Data.List.sort'@ to the result.
mapInvert :: Ord v => Map k v -> Map v [k]
mapInvert = Map.fromListWith (++) . map (\(k, v) -> (v, [k])) . Map.assocs

-- | Executes the monadic function if a 'Maybe' contains a value.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe (return ())

-- | Finds the first tuple whose first element is true, and returns its second
-- element.  If all of the first values are false, then the first argument to
-- @cond@ is returned instead.
cond :: a -> [(Bool, a)] -> a
cond fallback ((test, body):rest) = if test then body else cond fallback rest
cond fallback _ = fallback

-- | A function form of @if@ that takes its test last.
if' :: a -> a -> Bool -> a
if' true false test = if test then true else false

-- | 'and' in a monad.  Executes the actions in the list in order.  If any
-- action returns false then the remaining actions are skipped and the result is
-- false.  Otherwise all actions returned true, and the result is true.  An
-- empty list returns true.
andM :: Monad m => [m Bool] -> m Bool
andM (x:xs) = x >>= if' (andM xs) (return False)
andM _ = return True

-- | 'forM_' that also passes in the index of each element.
forIndexM_ :: Monad m => [a] -> (Int -> a -> m ()) -> m ()
forIndexM_ list = forM_ (zip [0..] list) . uncurry

-- | @whileM test body@ repeatedly evaluates @test@ until it returns false.
-- Every time @test@ returns true, @body@ is executed once.
whileM :: Monad m => m Bool -> m () -> m ()
whileM test body = do x <- test
                      when x $ body >> whileM test body

-- | @whileM' test body@ repeatedly evaluates @test@ until it returns 'Nothing'.
-- Every time it returns a 'Just', that value is passed to @body@ and the result
-- is executed.
whileM' :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileM' test body = do x <- test
                       case x of
                         Nothing -> return ()
                         Just y -> body y >> whileM' test body

-- | @doWhileM init body@ repeatedly calls @body@ with @init@.  As long as
-- @body@ returns a @Right@ value, it is re-executed with the returned value.
-- When it returns a @Left@ value, the loop stops and the value is returned.
doWhileM :: Monad m => a -> (a -> m (Either b a)) -> m b
doWhileM init body = do
  value <- body init
  case value of
    Right next -> doWhileM next body
    Left end -> return end
