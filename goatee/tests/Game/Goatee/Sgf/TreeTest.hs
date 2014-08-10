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

module Game.Goatee.Sgf.TreeTest (tests) where

import Data.Version (showVersion)
import Game.Goatee.App (applicationName)
import Game.Goatee.Common
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.TestInstances ()
import Game.Goatee.Sgf.TestUtils
import Game.Goatee.Sgf.Tree
import Game.Goatee.Sgf.Types
import Game.Goatee.Test.Common
import Paths_goatee (version)
import Test.HUnit ((~:), (@=?), (@?=), Test (TestList))

tests = "Game.Goatee.Sgf.Tree" ~: TestList [
  emptyNodeTests,
  rootNodeTests,
  findPropertyTests,
  addPropertyTests,
  addChildTests,
  addChildAtTests,
  deleteChildAtTests
  ]

emptyNodeTests = "emptyNode" ~: TestList [
  "has no properties" ~: [] @=? nodeProperties emptyNode,
  "has no children" ~: [] @=? nodeChildren emptyNode
  ]

rootNodeTests = "rootNode" ~: TestList [
  "sets SZ correctly" ~: do
    assertElem (SZ 9 9) $ nodeProperties $ rootNode $ Just (9, 9)
    assertElem (SZ 19 19) $ nodeProperties $ rootNode $ Just (19, 19)
    assertElem (SZ 9 5) $ nodeProperties $ rootNode $ Just (9, 5),

  "sets AP correctly" ~: do
    let ap = AP (toSimpleText applicationName)
                (toSimpleText $ showVersion version)
    assertElem ap $ nodeProperties $ rootNode Nothing
    assertElem ap $ nodeProperties $ rootNode $ Just (9, 9)
  ]

findPropertyTests = "findProperty" ~: TestList [
  "returns Nothing for an empty node" ~:
    Nothing @=? findProperty propertyB (mk []),

  "returns Nothing if no properties match" ~:
    Nothing @=? findProperty propertyB (mk [FF 4, GM 1, ST defaultVariationMode, SZ 9 9]),

  "finds present properties" ~: do
     Just (B (Just (2,3))) @=?
       findProperty propertyB (mk [B (Just (2,3))])
     Just (W Nothing) @=?
       findProperty propertyW (mk [B Nothing, W Nothing])
     Just IT @=?
       findProperty propertyIT (mk [IT, GW Double2])
     Just (GW Double2) @=?
       findProperty propertyGW (mk [IT, GW Double2]),

  "doesn't find absent properties" ~: do
    Nothing @=? findProperty propertyW (mk [B Nothing])
    Nothing @=? findProperty propertyW (mk [FF 4, GM 1, ST defaultVariationMode, SZ 9 9])
  ]
  where mk properties = emptyNode { nodeProperties = properties }

addPropertyTests = "addProperty" ~: TestList [
  "adds properties in order" ~: do
    let prop1 = B (Just (6,6))
        prop2 = RE GameResultVoid
        prop3 = C (toText "Game over.")
        node1 = addProperty prop1 emptyNode
        node2 = addProperty prop2 node1
        node3 = addProperty prop3 node2
    node1 @?= emptyNode { nodeProperties = [prop1] }
    node2 @?= emptyNode { nodeProperties = [prop1, prop2] }
    node3 @?= emptyNode { nodeProperties = [prop1, prop2, prop3] }
  ]

addChildTests = "addChild" ~: TestList [
  "adds children in order" ~: do
    let node1 = emptyNode { nodeProperties = [B (Just (0,0))] }
        node2 = emptyNode { nodeProperties = [W (Just (1,1))] }
        node3 = emptyNode { nodeProperties = [B (Just (2,2))] }
        node4 = emptyNode { nodeProperties = [W Nothing] }
        node2' = addChild node3 node2
        node1' = addChild node2' node1
        node1'' = addChild node4 node1'
    node1'' @?= mk [B (Just (0,0))]
                   [mk [W (Just (1,1))]
                       [mk [B (Just (2,2))]
                           []],
                    mk [W Nothing]
                       []]
  ]
  where mk properties children = emptyNode { nodeProperties = properties
                                           , nodeChildren = children
                                           }

addChildAtTests = "addChildAt" ~: TestList [
  "adds to a childless node" ~:
    let parent = node [B Nothing]
        child = node [W Nothing]
    in node1 [B Nothing] (node [W Nothing]) @=? addChildAt 0 child parent,

  "adds as a first child" ~: do
    let expected = base { nodeChildren = newNode : baseChildren }
    expected @=? addChildAt 0 newNode base
    expected @=? addChildAt (-1) newNode base
    expected @=? addChildAt (-2) newNode base,

  "adds as a middle child" ~: do
    base { nodeChildren = listInsertAt 1 newNode baseChildren } @=?
      addChildAt 1 newNode base
    base { nodeChildren = listInsertAt 2 newNode baseChildren } @=?
      addChildAt 2 newNode base,

  "adds as a last child" ~: do
    let expected = base { nodeChildren = baseChildren ++ [newNode] }
    expected @=? addChildAt baseChildCount newNode base
    expected @=? addChildAt (baseChildCount + 1) newNode base
    expected @=? addChildAt (baseChildCount + 2) newNode base
  ]
  where base = node' [B $ Just (0,0)] baseChildren
        baseChildren = [node [W Nothing],
                        node [W $ Just (1,1)],
                        node [W $ Just (9,9)]]
        baseChildCount = length $ nodeChildren base
        newNode = node [B $ Just (1,1)]

deleteChildAtTests = "deleteChildAt" ~: TestList [
  "deletes nothing from an empty list" ~: do
    emptyNode @=? deleteChildAt (-1) emptyNode
    emptyNode @=? deleteChildAt 0 emptyNode
    emptyNode @=? deleteChildAt 1 emptyNode,

  "deletes an only child" ~:
    node [B Nothing] @=? deleteChildAt 0 (node1 [B Nothing] $ node [W Nothing]),

  "deletes a first child" ~:
    base { nodeChildren = listDeleteAt 0 baseChildren } @=?
    deleteChildAt 0 base,

  "deletes middle children" ~: do
    base { nodeChildren = listDeleteAt 1 baseChildren } @=?
      deleteChildAt 1 base
    base { nodeChildren = listDeleteAt 2 baseChildren } @=?
      deleteChildAt 2 base,

  "delete a last child" ~:
    base { nodeChildren = init baseChildren } @=?
    deleteChildAt (baseChildCount - 1) base,

  "has no effect for invalid indices" ~: do
    base @=? deleteChildAt (-1) base
    base @=? deleteChildAt (-2) base
    base @=? deleteChildAt baseChildCount base
    base @=? deleteChildAt (baseChildCount + 1) base
    base @=? deleteChildAt (baseChildCount + 2) base
  ]
  where base = node' [B $ Just (0,0)] baseChildren
        baseChildren = [node [W Nothing],
                        node [W $ Just (1,1)],
                        node [W $ Just (2,2)],
                        node [W $ Just (9,9)]]
        baseChildCount = length $ nodeChildren base

-- TODO Test validateNode.
