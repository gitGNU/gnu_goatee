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

-- | Functions for serializing SGF trees.
module Game.Goatee.Lib.Renderer.Tree (
  renderCollection,
  renderGameTree,
  renderProperty,
  ) where

import Control.Monad.Writer (tell)
import Game.Goatee.Common
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Renderer
import Game.Goatee.Lib.Tree

-- | Renders an SGF 'Collection' to a string.
renderCollection :: Collection -> Render ()
renderCollection collection = do
  mapM_ renderGameTree $ collectionTrees collection
  tell "\n"

-- | Recursively renders an SGF GameTree (as defined in the spec) rooted at the
-- given node.
renderGameTree :: Node -> Render ()
renderGameTree node = do
  tell "("
  doWhileM node
    (\node' -> do renderNode node'
                  case nodeChildren node' of
                    [] -> return $ Left Nothing
                    [child] -> return $ Right child
                    children -> return $ Left $ Just children)
    >>= maybe (return ()) (mapM_ renderGameTree)
  tell ")"

-- | Renders a node and its properties without recurring to its children.
renderNode :: Node -> Render ()
renderNode node = do
  tell "\n;"
  mapM_ renderProperty $ nodeProperties node

renderProperty :: Property -> Render ()
renderProperty property = do
  tell $ propertyName property
  propertyValueRenderer property property
