-- | Test utilities for working with the SGF modules.
module Khumba.Goatee.Sgf.TestUtils (
  node
  , node1
  , node'
  , rootNode
  , child
  , sortProperties
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Tree

node :: [Property] -> Node
node props = emptyNode { nodeProperties = props }

node1 :: [Property] -> Node -> Node
node1 props child = emptyNode { nodeProperties = props
                              , nodeChildren = [child]
                              }

node' :: [Property] -> [Node] -> Node
node' props children = emptyNode { nodeProperties = props
                                , nodeChildren = children
                                }

rootNode :: Int -> Int -> [Property] -> [Node] -> Node
rootNode width height props =
  foldr addChild $ foldr addProperty (rootNodeWithSize width height) props

child :: Int -> Cursor -> Cursor
child = flip cursorChild

-- | Sorts properties by their 'Show' strings, for use in tests that need to
-- check an unordered list of properties.
--
-- TODO Probably better to have a compare-unordered operator.
sortProperties :: [Property] -> [Property]
sortProperties = sortBy (compare `on` show)
