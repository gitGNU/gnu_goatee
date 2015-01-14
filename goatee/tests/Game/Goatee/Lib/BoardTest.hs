-- This file is part of Goatee.
--
-- Copyright 2014-2015 Bryan Gardiner
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

module Game.Goatee.Lib.BoardTest (tests) where

import Game.Goatee.Common
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.TestInstances ()
import Game.Goatee.Lib.TestUtils
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import Test.HUnit ((~:), (@=?), (@?=), Test (TestList))

tests = "Game.Goatee.Lib.Board" ~: TestList
  [ boardCoordStateTests
  , boardCoordModifyTests
  , moveNumberTests
  , markupPropertiesTests
  , visibilityPropertyTests
  , cursorModifyNodeTests
  , moveToPropertyTests
  ]

boardCoordStateTests = "boardCoordState" ~: TestList
  [ "works for a 1x1 board" ~: do
    let state = boardCoordState (0,0) $ rootBoardState $
                node [SZ 1 1, CR $ coord1 (0,0)]
    coordStone state @?= Nothing
    coordMark state @?= Just MarkCircle

  , "works for a 2x2 board" ~: do
    let board = rootBoardState $
                node [SZ 2 2, B $ Just (0,0), TR $ coord1 (0,0), MA $ coord1 (1,1)]
    coordStone (boardCoordState (0,0) board) @?= Just Black
    coordMark (boardCoordState (0,0) board) @?= Just MarkTriangle
    coordStone (boardCoordState (1,0) board) @?= Nothing
    coordMark (boardCoordState (1,0) board) @?= Nothing
    coordStone (boardCoordState (1,1) board) @?= Nothing
    coordMark (boardCoordState (1,1) board) @?= Just MarkX
  ]

boardCoordModifyTests = "boardCoordModify" ~: TestList
  [ "modifies a single coord" ~:
    let state = emptyCoordState
        state' = emptyCoordState { coordStone = Just White }
        state'' = emptyCoordState { coordMark = Just MarkTriangle }
        expectedStates = [[state, state'], [state'', state]]
        actualStates = boardCoordStates $
                       (\board -> boardCoordModify board (1, 0) (const state')) $
                       (\board -> boardCoordModify board (0, 1) (const state'')) $
                       rootBoardState $ rootNode $ Just (2, 2)
    in expectedStates @=? actualStates
  ]

moveNumberTests = "move number" ~: TestList
  [ "starts at zero" ~:
    0 @=? boardMoveNumber (cursorBoard $ rootCursor $ node1 [] $ node [B Nothing])

  , "advances for nodes with moves" ~: do
    1 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $ node1 [] $ node [B Nothing])
    1 @=? boardMoveNumber (cursorBoard $ rootCursor $ node1 [B Nothing] $ node [])
    1 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $ node1 [B Nothing] $ node [])
    2 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [B Nothing] $ node [W Nothing])
    2 @=? boardMoveNumber (cursorBoard $ child 0 $ child 0 $ rootCursor $
                           node1 [B Nothing] $ node1 [W Nothing] $ node [])
    3 @=? boardMoveNumber (cursorBoard $ child 0 $ child 0 $ rootCursor $
                           node1 [B Nothing] $ node1 [W Nothing] $ node [B Nothing])

  , "doesn't advance for non-move nodes" ~: do
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [] $ node [])
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [PL White] $ node [])
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [AB $ coords [(0,0)]] $ node [])
    0 @=? boardMoveNumber (cursorBoard $ child 0 $ rootCursor $
                           node1 [IT, TE Double2, GN $ toSimpleText "Title"] $ node [])
  ]

markupPropertiesTests = "markup properties" ~: TestList
  [ "adds marks to a BoardState" ~: TestList
    [ "CR" ~: Just MarkCircle @=? getMark 0 0 (rootCursor $ node [CR $ coords [(0,0)]])
    , "MA" ~: Just MarkX @=? getMark 0 0 (rootCursor $ node [MA $ coords [(0,0)]])
    , "SL" ~: Just MarkSelected @=? getMark 0 0 (rootCursor $ node [SL $ coords [(0,0)]])
    , "SQ" ~: Just MarkSquare @=? getMark 0 0 (rootCursor $ node [SQ $ coords [(0,0)]])
    , "TR" ~: Just MarkTriangle @=? getMark 0 0 (rootCursor $ node [TR $ coords [(0,0)]])
    , "multiple at once" ~: do
      let cursor = rootCursor $ node [SZ 2 2,
                                      CR $ coords [(0,0), (1,1)],
                                      TR $ coords [(1,0)]]
      [[Just MarkCircle, Just MarkTriangle],
       [Nothing, Just MarkCircle]]
        @=? map (map coordMark) (boardCoordStates $ cursorBoard cursor)
    ]

  , "adds more complex annotations to a BoardState" ~: TestList
    [ "AR" ~: [((0,0), (1,1))] @=? boardArrows (rootCoord $ node [AR [((0,0), (1,1))]])
    , "LB" ~: [((0,0), st "Hi")] @=? boardLabels (rootCoord $ node [LB [((0,0), st "Hi")]])
    , "LN" ~: [Line (0,0) (1,1)] @=? boardLines (rootCoord $ node [LN [Line (0,0) (1,1)]])
    ]

  , "clears annotations when moving to a child node" ~: do
    let root = node1 [SZ 3 2,
                      CR $ coords [(0,0)],
                      MA $ coords [(1,0)],
                      SL $ coords [(2,0)],
                      SQ $ coords [(0,1)],
                      TR $ coords [(1,1)],
                      AR [((0,0), (2,1))],
                      LB [((2,1), st "Empty")],
                      LN [Line (1,1) (2,0)]] $
                 node []
        board = cursorBoard $ child 0 $ rootCursor root
    mapM_ (mapM_ ((Nothing @=?) . coordMark)) $ boardCoordStates board
    [] @=? boardArrows board
    [] @=? boardLines board
    [] @=? boardLabels board

  , "boardHasCoordMarks" ~: TestList
    [ "defaults to false" ~:
      False @=? boardHasCoordMarks (cursorBoard $ rootCursor $ node [])

    , "is true when marks are present" ~:
      True @=? boardHasCoordMarks (cursorBoard $ rootCursor $ node [CR $ coord1 (0,0)])

    , "is set to false when moving to a child node" ~:
      False @=? boardHasCoordMarks (cursorBoard $ child 0 $ rootCursor $
                                    node1 [CR $ coord1 (0,0)] $ node [])

    , "all marks set it to true" ~: TestList
      (for [minBound..] $ \mark ->
        let p = markProperty mark
        in propertyName p ~:
           True @=? boardHasCoordMarks (cursorBoard $ rootCursor $
                                        node [propertyBuilder p $ coord1 (0,0)]))
    ]
  ]
  where rootCoord = cursorBoard . rootCursor
        st = toSimpleText

        getMark :: Int -> Int -> Cursor -> Maybe Mark
        getMark x y cursor = coordMark $ boardCoordStates (cursorBoard cursor) !! y !! x

visibilityPropertyTests = "visibility properties" ~: TestList
  [ "boards start with all points undimmed" ~:
    replicate 9 (replicate 9 False) @=?
    map (map coordDimmed) (boardCoordStates $ cursorBoard $ rootCursor $ node [SZ 9 9])

  , "DD selectively dims points" ~:
    let root = node [SZ 5 2, DD $ coords' [(3,0)] [((0,0), (1,1))]]
    in [[True, True, False, True, False],
        [True, True, False, False, False]] @=?
       map (map coordDimmed) (boardCoordStates $ cursorBoard $ rootCursor root)

  , "dimming is inherited" ~:
    let root = node1 [SZ 5 2, DD $ coords' [(3,0)] [((0,0), (1,1))]] $ node []
    in [[True, True, False, True, False],
        [True, True, False, False, False]] @=?
       map (map coordDimmed) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root)

  , "DD[] clears dimming" ~:
    let root = node1 [SZ 2 1, DD $ coords [(0,0)]] $ node [DD $ coords []]
    in [[False, False]] @=?
       map (map coordDimmed) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root)

  , "boards start with all points visible" ~:
    replicate 9 (replicate 9 True) @=?
    map (map coordVisible) (boardCoordStates $ cursorBoard $ rootCursor $ node [SZ 9 9])

  , "VW selectively makes points visible" ~:
    let root = node [SZ 5 2, VW $ coords' [(1,0), (0,1)] [((2,0), (4,1))]]
    in [[False, True, True, True, True],
        [True, False, True, True, True]] @=?
       map (map coordVisible) (boardCoordStates $ cursorBoard $ rootCursor root)

  , "visibility is inherited" ~:
    let root = node1 [SZ 5 2, VW $ coords' [(1,0), (0,1)] [((2,0), (4,1))]] $ node []
    in [[False, True, True, True, True],
        [True, False, True, True, True]] @=?
       map (map coordVisible) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root)

  , "VW[] clears dimming" ~:
    let root = node1 [SZ 2 1, VW $ coords [(0,0)]] $ node [VW $ coords []]
    in [[True, True]] @=?
       map (map coordVisible) (boardCoordStates $ cursorBoard $ child 0 $ rootCursor root)
  ]

cursorModifyNodeTests = "cursorModifyNode" ~: TestList
  [ "updates the BoardState" ~:
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

moveToPropertyTests = "moveToProperty" ~: TestList
  [ "creates Black moves" ~: do
    B Nothing @=? moveToProperty Black Nothing
    B (Just (3,2)) @=? moveToProperty Black (Just (3,2))
    B (Just (0,0)) @=? moveToProperty Black (Just (0,0))

  , "creates White moves" ~: do
    W Nothing @=? moveToProperty White Nothing
    W (Just (18,18)) @=? moveToProperty White (Just (18,18))
    W (Just (15,16)) @=? moveToProperty White (Just (15,16))
  ]
