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

-- | Test utilities for working with the SGF modules.
module Game.Goatee.Lib.TestUtils (
  node,
  node1,
  node',
  root,
  child,
  sortProperties,
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Tree

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

-- | Creates a root node that starts with only a 'SZ' property.  This is more
-- minimal than 'rootNode'.
root :: Int -> Int -> [Property] -> [Node] -> Node
root width height props =
  foldr addChild $ foldr addProperty emptyNode (SZ width height:props)

child :: Int -> Cursor -> Cursor
child = flip cursorChild

-- | Sorts properties by their 'Show' strings, for use in tests that need to
-- check an unordered list of properties.
--
-- TODO Probably better to have a compare-unordered operator.
sortProperties :: [Property] -> [Property]
sortProperties = sortBy (compare `on` show)
