module Khumba.GoHS.SgfTestUtils ( node
                                , node1
                                , node'
                                , rootNode
                                ) where

import qualified Khumba.GoHS.Sgf as Sgf
import Khumba.GoHS.Sgf hiding (rootNode)

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
