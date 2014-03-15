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
module Khumba.Goatee.Sgf.Printer (
  printCollection
  ) where

import Control.Monad
import Control.Monad.Writer
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Tree

-- | Serializes an SGF 'Collection' to a string.
printCollection :: Collection -> String
printCollection collection = snd $ runWriter $ do
  mapM_ writeGameTree $ collectionTrees collection
  tell "\n"

-- | Recursively writes an SGF GameTree (as defined in the spec) rooted at the
-- given node.
writeGameTree :: Node -> Writer String ()
writeGameTree node = do
  tell "("
  doWhileM node
    (\node' -> do writeNode node'
                  case nodeChildren node' of
                    [] -> return $ Left Nothing
                    child:[] -> return $ Right child
                    children -> return $ Left $ Just children)
    >>= maybe (return ()) (mapM_ writeGameTree)
  tell ")"

-- | Writes a node and its properties without recurring to its children.
writeNode :: Node -> Writer String ()
writeNode node = do
  tell "\n;"
  forM_ (nodeProperties node) $ \property -> do
    tell $ propertyName property
    tell $ propertyValuePrinter property property
