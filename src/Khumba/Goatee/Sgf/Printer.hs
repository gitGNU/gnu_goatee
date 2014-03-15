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
