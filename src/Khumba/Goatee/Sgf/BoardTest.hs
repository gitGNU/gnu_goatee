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
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.Goatee.Sgf.Board" [
  boardCoordStateTests,
  moveNumberTests,
  markupPropertiesTests,
  visibilityPropertyTests,
  cursorModifyNodeTests,
  colorToMoveTests
  ]

boardCoordStateTests = testGroup "boardCoordState" [
  testCase "works for a 1x1 board" $ do
    let state = boardCoordState (0,0) $ rootBoardState $
                node [SZ 1 1, CR $ coord1 (0,0)]
    coordStone state @?= Nothing
    coordMark state @?= Just MarkCircle,

  testCase "works for a 2x2 board" $ do
    let board = rootBoardState $
                node [SZ 2 2, B $ Just (0,0), TR $ coord1 (0,0), MA $ coord1 (1,1)]
    coordStone (boardCoordState (0,0) board) @?= Just Black
    coordMark (boardCoordState (0,0) board) @?= Just MarkTriangle
    coordStone (boardCoordState (1,0) board) @?= Nothing
    coordMark (boardCoordState (1,0) board) @?= Nothing
    coordStone (boardCoordState (1,1) board) @?= Nothing
    coordMark (boardCoordState (1,1) board) @?= Just MarkX
  ]


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

markupPropertiesTests = testGroup "markup properties" [
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

visibilityPropertyTests = testGroup "visibility properties" [
  testCase "boards start with all points undimmed" $
    replicate 9 (replicate 9 False) @=?
    map (map coordDimmed) (boardCoordStates $ cursorBoard $ rootCursor $ node [SZ 9 9]),

  testCase "DD selectively dims points" $
    let root = node [SZ 5 2, DD $ coords' [(3,0)] [((0,0), (1,1))]]
    in [[True, True, False, True, False],
        [True, True, False, False, False]] @=?
       map (map coordDimmed) (boardCoordStates $ cursorBoard $ rootCursor root),

  testCase "dimming is inherited" $
    let root = node1 [SZ 5 2, DD $ coords' [(3,0)] [((0,0), (1,1))]] $ node []
    in [[True, True, False, True, False],
        [True, True, False, False, False]] @=?
       map (map coordDimmed) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root),

  testCase "DD[] clears dimming" $
    let root = node1 [SZ 2 1, DD $ coords [(0,0)]] $ node [DD $ coords []]
    in [[False, False]] @=?
       map (map coordDimmed) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root),

  testCase "boards start with all points visible" $
    replicate 9 (replicate 9 True) @=?
    map (map coordVisible) (boardCoordStates $ cursorBoard $ rootCursor $ node [SZ 9 9]),

  testCase "VW selectively makes points visible" $
    let root = node [SZ 5 2, VW $ coords' [(1,0), (0,1)] [((2,0), (4,1))]]
    in [[False, True, True, True, True],
        [True, False, True, True, True]] @=?
       map (map coordVisible) (boardCoordStates $ cursorBoard $ rootCursor root),

  testCase "visibility is inherited" $
    let root = node1 [SZ 5 2, VW $ coords' [(1,0), (0,1)] [((2,0), (4,1))]] $ node []
    in [[False, True, True, True, True],
        [True, False, True, True, True]] @=?
       map (map coordVisible) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root),

  testCase "VW[] clears dimming" $
    let root = node1 [SZ 2 1, VW $ coords [(0,0)]] $ node [VW $ coords []]
    in [[True, True]] @=?
       map (map coordVisible) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root)
  ]

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

colorToMoveTests = testGroup "colorToMove" [
  testCase "creates Black moves" $ do
    B (Just (3,2)) @=? colorToMove Black (3,2)
    B (Just (0,0)) @=? colorToMove Black (0,0),

  testCase "creates White moves" $ do
    W (Just (18,18)) @=? colorToMove White (18,18)
    W (Just (15,16)) @=? colorToMove White (15,16)
  ]
