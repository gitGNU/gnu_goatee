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

module Khumba.Goatee.Sgf.BoardTest (tests) where

import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.TestUtils
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types
import Khumba.Goatee.Test.Common
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.Goatee.Sgf.Board" [
  expandCoordListTests,
  emptyNodeTests,
  rootNodeWithSizeTests,
  findPropertyTests,
  addPropertyTests,
  addChildTests,
  simpleTextTests,
  cnotTests,
  colorToMoveTests,
  propertyMetadataTests,
  moveNumberTests,
  annotationTests,
  cursorModifyNodeTests
  ]

expandCoordListTests = testGroup "expandCoordList" [
  testCase "works with an empty CoordList" $
    [] @=? expandCoordList CoordList {coordListSingles = [], coordListRects = []},

  testCase "works for single points" $ do
    [(1,2)] @=? expandCoordList CoordList { coordListSingles = [(1,2)]
                                          , coordListRects = []
                                          }
    [(3,4), (1,2)] @=? expandCoordList CoordList { coordListSingles = [(3,4), (1,2)]
                                                 , coordListRects = []
                                                 }
    let ten = [(i,i) | i <- [1..10]]
    ten @=? expandCoordList CoordList { coordListSingles = ten
                                      , coordListRects = []
                                      },

  -- TODO Test that a 1x1 rectangle is rejected.

  testCase "works for a nx1 rect" $
    [(5,2), (5,3), (5,4)] @=? expandCoordList CoordList { coordListSingles = []
                                                        , coordListRects = [((5,2), (5,4))]
                                                        },

  testCase "works for a 1xn rect" $
    [(1,0), (1,1), (1,2), (1,3)] @=? expandCoordList CoordList { coordListSingles = []
                                                               , coordListRects = [((1,0), (1,3))]
                                                               },

  testCase "works for an mxn rect" $
    [(m,n) | m <- [2..5], n <- [3..7]] @=?
    expandCoordList CoordList { coordListSingles = []
                              , coordListRects = [((2,3), (5,7))]
                              },

  -- TODO Test that x0 > x1 || y0 > y1 is rejected.

  testCase "works with multiple rects" $
    [(0,0), (0,1), (0,2), (3,4), (4,4), (5,4)] @=?
    expandCoordList CoordList { coordListSingles = []
                              , coordListRects = [((0,0), (0,2)), ((3,4), (5,4))]
                              },

  testCase "concatenates single points and rects" $
    [(1,1), (0,0), (2,2), (2,3), (2,4)] @=?
    expandCoordList CoordList { coordListSingles = [(1,1), (0,0)]
                              , coordListRects = [((2,2), (2,4))]
                              }
  ]

emptyNodeTests = testGroup "emptyNode" [
  testCase "has no properties" $ [] @=? nodeProperties emptyNode,
  testCase "has no children" $ [] @=? nodeChildren emptyNode
  ]

rootNodeWithSizeTests = testGroup "rootNodeWithSize" [
  testCase "sets SZ correctly" $ do
    assertElem (SZ 9 9) $ nodeProperties $ rootNodeWithSize 9 9
    assertElem (SZ 19 19) $ nodeProperties $ rootNodeWithSize 19 19
    assertElem (SZ 9 5) $ nodeProperties $ rootNodeWithSize 9 5
  ]

findPropertyTests = testGroup "findProperty" [
  testCase "returns Nothing for an empty node" $
    Nothing @=? findProperty propertyB (mk []),

  testCase "returns Nothing if no properties match" $
    Nothing @=? findProperty propertyB (mk [FF 4, GM 1, ST defaultVariationMode, SZ 9 9]),

  testCase "finds present properties" $ do
     Just (B (Just (2,3))) @=?
       findProperty propertyB (mk [B (Just (2,3))])
     Just (W Nothing) @=?
       findProperty propertyW (mk [B Nothing, W Nothing])
     Just IT @=?
       findProperty propertyIT (mk [IT, GW Double2])
     Just (GW Double2) @=?
       findProperty propertyGW (mk [IT, GW Double2]),

  testCase "doesn't find absent properties" $ do
    Nothing @=? findProperty propertyW (mk [B Nothing])
    Nothing @=? findProperty propertyW (mk [FF 4, GM 1, ST defaultVariationMode, SZ 9 9])
  ]
  where mk properties = emptyNode { nodeProperties = properties }

addPropertyTests = testGroup "addProperty" [
  testCase "adds properties in order" $ do
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

addChildTests = testGroup "addChild" [
  testCase "adds children in order" $ do
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

-- TODO Test validateNode.

simpleTextTests = testGroup "SimpleText" [
  testCase "accepts the empty string" $
    "" @=? fromSimpleText (toSimpleText ""),

  testCase "passes through strings without newlines" $ do
    "Hello." @=? fromSimpleText (toSimpleText "Hello.")
    "Bad for B." @=? fromSimpleText (toSimpleText "Bad for B.")
    "[4k?]" @=? fromSimpleText (toSimpleText "[4k?]")
    printableAsciiChars @=? fromSimpleText (toSimpleText printableAsciiChars),

  testCase "converts newlines to spaces" $ do
    " " @=? fromSimpleText (toSimpleText "\n")
    "Hello, world." @=? fromSimpleText (toSimpleText "Hello,\nworld.")
    "Hello,   world." @=? fromSimpleText (toSimpleText "Hello, \n world.")
    " Hello, world. " @=? fromSimpleText (toSimpleText "\nHello, world.\n")
  ]

cnotTests = testGroup "cnot" [
  testCase "changes Black to White" $ White @=? cnot Black,
  testCase "changes White to Black" $ Black @=? cnot White
  ]

colorToMoveTests = testGroup "colorToMove" [
  testCase "creates Black moves" $ do
    B (Just (3,2)) @=? colorToMove Black (3,2)
    B (Just (0,0)) @=? colorToMove Black (0,0),

  testCase "creates White moves" $ do
    W (Just (18,18)) @=? colorToMove White (18,18)
    W (Just (15,16)) @=? colorToMove White (15,16)
  ]

propertyMetadataTests = testGroup "property metadata" [
  testCase "game info properties" $ gameInfoProperties @=? filterTo GameInfoProperty allProperties,
  testCase "general properties" $ generalProperties @=? filterTo GeneralProperty allProperties,
  testCase "move properties" $ moveProperties @=? filterTo MoveProperty allProperties,
  testCase "root properties" $ rootProperties @=? filterTo RootProperty allProperties,
  testCase "setup properties" $ setupProperties @=? filterTo SetupProperty allProperties,

  testCase "inherited properties" $ [DD cl] @=? filter propertyInherited allProperties
  ]
  where filterTo propType = filter ((propType ==) . propertyType)
        moveProperties = [-- Move properties.
                          B Nothing, KO, MN 1, W Nothing,
                          -- Move annotation properties.
                          BM db, DO, IT, TE db]
        setupProperties = [-- Setup properties.
                           AB cl, AE cl, AW cl, PL Black]
        generalProperties = [-- Node annotation properties.
                             C tx, DM db, GB db, GW db, HO db, N st, UC db, V rv,
                             -- Markup properties.
                             AR [], CR cl, DD cl, LB [], LN [], MA cl, SL cl, SQ cl, TR cl,
                             -- Guess this fits here.
                             UnknownProperty "" ""]
        rootProperties = [-- Root properties.
                          AP st st, CA st, FF 1, GM 1, ST vm, SZ 1 1]
        gameInfoProperties = [-- Game info properties.
                              AN st, BR st, BT st, CP st, DT st, EV st, GC st, GN st, ON st, OT st,
                              PB st, PC st, PW st, RE GameResultVoid, RO st, RU ru,
                              SO st, TM rv, US st, WR st, WT st]
        allProperties = moveProperties ++ setupProperties ++ generalProperties ++
                        rootProperties ++ gameInfoProperties
        cl = emptyCoordList
        db = Double1
        tx = toText ""
        st = toSimpleText ""
        ru = KnownRuleset RulesetJapanese
        rv = 1
        vm = defaultVariationMode

moveNumberTests = testGroup "move number" [
  testCase "starts at zero" $
    0 @=? boardMoveNumber (cursorBoard $ rootCursor $ node1 [] $ node [B Nothing]),

  testCase "advances for nodes with moves" $ do
    1 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $ node1 [] $ node [B Nothing])
    1 @=? boardMoveNumber (cursorBoard $ rootCursor $ node1 [B Nothing] $ node [])
    1 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $ node1 [B Nothing] $ node [])
    2 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [B Nothing] $ node [W Nothing])
    2 @=? boardMoveNumber (cursorBoard $ child 0 $ child 0 $ rootCursor $
                           node1 [B Nothing] $ node1 [W Nothing] $ node [])
    3 @=? boardMoveNumber (cursorBoard $ child 0 $ child 0 $ rootCursor $
                           node1 [B Nothing] $ node1 [W Nothing] $ node [B Nothing]),

  testCase "doesn't advance for non-move nodes" $ do
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [] $ node [])
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [PL White] $ node [])
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [AB $ coords [(0,0)]] $ node [])
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [IT, TE Double2, GN $ toSimpleText "Title"] $ node [])
  ]

annotationTests = testGroup "annotations" [
  testGroup "adds marks to a BoardState" [
    testCase "CR" $ Just MarkCircle @=? getMark 0 0 (rootCursor $ node [CR $ coords [(0,0)]]),
    testCase "MA" $ Just MarkX @=? getMark 0 0 (rootCursor $ node [MA $ coords [(0,0)]]),
    testCase "SL" $ Just MarkSelected @=? getMark 0 0 (rootCursor $ node [SL $ coords [(0,0)]]),
    testCase "SQ" $ Just MarkSquare @=? getMark 0 0 (rootCursor $ node [SQ $ coords [(0,0)]]),
    testCase "TR" $ Just MarkTriangle @=? getMark 0 0 (rootCursor $ node [TR $ coords [(0,0)]]),
    testCase "multiple at once" $ do
      let cursor = rootCursor $ node [SZ 2 2,
                                      CR $ coords [(0,0), (1,1)],
                                      TR $ coords [(1,0)]]
      [[Just MarkCircle, Just MarkTriangle],
       [Nothing, Just MarkCircle]]
        @=? map (map coordMark) (boardCoordStates $ cursorBoard cursor)
    ],

  testGroup "adds more complex annotations to a BoardState" [
    testCase "AR" $ [((0,0), (1,1))] @=? boardArrows (rootCoord $ node [AR [((0,0), (1,1))]]),
    testCase "LB" $ [((0,0), st "Hi")] @=? boardLabels (rootCoord $ node [LB [((0,0), st "Hi")]]),
    testCase "LN" $ [((0,0), (1,1))] @=? boardLines (rootCoord $ node [LN [((0,0), (1,1))]])
    ],

  testCase "clears annotations when moving to a child node" $ do
    let root = node1 [SZ 3 2,
                      CR $ coords [(0,0)],
                      MA $ coords [(1,0)],
                      SL $ coords [(2,0)],
                      SQ $ coords [(0,1)],
                      TR $ coords [(1,1)],
                      AR [((0,0), (2,1))],
                      LB [((2,1), st "Empty")],
                      LN [((1,1), (2,0))]] $
                 node []
        board = cursorBoard $ child 0 $ rootCursor root
    mapM_ (mapM_ ((Nothing @=?) . coordMark)) $ boardCoordStates board
    [] @=? boardArrows board
    [] @=? boardLines board
    [] @=? boardLabels board
  ]
  where rootCoord = cursorBoard . rootCursor
        st = toSimpleText

        getMark :: Int -> Int -> Cursor -> Maybe Mark
        getMark x y cursor = coordMark $ boardCoordStates (cursorBoard cursor) !! y !! x

-- TODO Test visibility properties (DD, VW).

cursorModifyNodeTests = testGroup "cursorModifyNode" [
  testCase "updates the BoardState" $
    let cursor = child 0 $ rootCursor $
                 node1 [SZ 3 1, B $ Just (0,0)] $
                 node [W $ Just (1,0)]
        modifyProperty prop = case prop of
          W (Just (1,0)) -> W $ Just (2,0)
          _ -> prop
        modifyNode node = node { nodeProperties = map modifyProperty $ nodeProperties node }
        result = cursorModifyNode modifyNode cursor
    in map (map coordStone) (boardCoordStates $ cursorBoard result) @?=
       [[Just Black, Nothing, Just White]]
  ]
