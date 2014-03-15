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

-- | SGF data structures modelling the hierarchical game tree.
module Khumba.Goatee.Sgf.Tree (
  Collection(..),
  Node(..), emptyNode, rootNodeWithSize,
  findProperty, findProperty', findPropertyValue, findPropertyValue',
  addProperty, addChild, addChildAt,
  validateNode
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, sortBy)
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Types

-- | An SGF collection of game trees.
data Collection = Collection { collectionTrees :: [Node]
                             } deriving (Show)

-- | An SGF game tree node.  Unlike in the SGF spec, we represent a game tree
-- with nodes uniformly, rather than having the separation between sequences and
-- nodes.
data Node = Node { nodeProperties :: [Property]
                 , nodeChildren :: [Node]
                 } deriving (Eq, Show)

-- | A node with no properties and no children.
emptyNode :: Node
emptyNode = Node { nodeProperties = [], nodeChildren = [] }

rootNodeWithSize :: Int -- ^ Board width
                 -> Int -- ^ Board height
                 -> Node
rootNodeWithSize width height =
  Node { nodeProperties = [SZ width height]
       , nodeChildren = []
       }

-- | Searches for a matching property in a node's property list.
findProperty :: Descriptor a => a -> Node -> Maybe Property
findProperty descriptor node = findProperty' descriptor $ nodeProperties node

-- | Searches for a matching property in a property list.
findProperty' :: Descriptor a => a -> [Property] -> Maybe Property
findProperty' = find . propertyPredicate

-- | Retrieves the value of a property in a node's property list.
findPropertyValue :: ValuedDescriptor a v => a -> Node -> Maybe v
findPropertyValue descriptor node = propertyValue descriptor <$> findProperty descriptor node

-- | Retrieves the value of a property in a property list.
findPropertyValue' :: ValuedDescriptor a v => a -> [Property] -> Maybe v
findPropertyValue' descriptor properties =
  propertyValue descriptor <$> findProperty' descriptor properties

-- | Appends a property to a node's property list.
addProperty :: Property -> Node -> Node
addProperty prop node = node { nodeProperties = nodeProperties node ++ [prop] }

-- | Appends a child node to a node's child list.
--
-- @addChild child parent@
addChild :: Node -> Node -> Node
addChild child node = node { nodeChildren = nodeChildren node ++ [child] }

-- | Inserts a child node into a node's child list at the given index, shifting
-- all nodes at or after the given index to the right.  The index must be in the
-- range @[0, numberOfChildren]@.
--
-- @addChild index child parent@
addChildAt :: Int -> Node -> Node -> Node
addChildAt index child node =
  let (before, after) = splitAt index $ nodeChildren node
  in node { nodeChildren = before ++ child:after }

-- | Returns a list of validation errors for the current node, an
-- empty list if no errors are detected.
validateNode :: Bool -> Bool -> Node -> [String]
validateNode isRoot _{-seenGameNode-} node = execWriter $ do
  let props = nodeProperties node
  let propTypes = nub $ map propertyType $ nodeProperties node

  -- Check for move and setup properties in a single node.
  when (MoveProperty `elem` propTypes && SetupProperty `elem` propTypes) $
    tell ["Node contains move and setup properties."]

  -- Check for root properties in non-root nodes.
  let rootProps = filter ((RootProperty ==) . propertyType) props
  when (not isRoot && not (null rootProps)) $
    tell $ map (\p -> "Root property found on non-root node: " ++
                      show p ++ ".")
           rootProps

  -- TODO Check for game-info properties.

  -- Check for coordinates marked multiple times.
  validateNodeDuplicates props getMarkedCoords $ \group ->
    tell ["Coordinate " ++ show (fst $ head group) ++
          " is specified multiple times in properties " ++
          intercalate ", " (map snd group) ++ "."]

  -- TODO Validate recursively.

  where getMarkedCoords (CR cs) = tagMarkedCoords cs "CR"
        getMarkedCoords (MA cs) = tagMarkedCoords cs "MA"
        getMarkedCoords (SL cs) = tagMarkedCoords cs "SL"
        getMarkedCoords (SQ cs) = tagMarkedCoords cs "SQ"
        getMarkedCoords (TR cs) = tagMarkedCoords cs "TR"
        getMarkedCoords _ = []

        tagMarkedCoords cs tag = map (\x -> (x, tag)) $ expandCoordList cs

validateNodeDuplicates :: (Eq v, Ord v)
                          => [Property]
                          -> (Property -> [(v, t)])
                          -> ([(v, t)] -> Writer [String] ())
                          -> Writer [String] ()
validateNodeDuplicates props getTaggedElts errAction =
  let groups = groupBy ((==) `on` fst) $
               sortBy (compare `on` fst) $
               concatMap getTaggedElts props
  in forM_ groups $ \group ->
       unless (null $ tail group) $
         errAction group
