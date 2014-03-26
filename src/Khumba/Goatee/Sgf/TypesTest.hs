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

module Khumba.Goatee.Sgf.TypesTest (tests) where

import Data.List (sort)
import Khumba.Goatee.Sgf.Types
import Khumba.Goatee.Test.Common
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.Goatee.Sgf.Types" [
  expandCoordListTests,
  buildCoordListTests,
  simpleTextTests,
  cnotTests
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

buildCoordListTests = testGroup "buildCoordList" [
  testCase "handles the trivial case []" $ assertSinglesAndRects [] [] [],

  testCase "handles the trivial case [[]]" $ assertSinglesAndRects [] [] [[]],

  testCase "handles all being false (1x1)" $ assertGrid 1 1 [],

  testCase "handles all being false (2x2)" $ assertGrid 2 2 [],

  testCase "handles all being false (4x3)" $ assertGrid 4 3 [],

  testCase "handles one single" $
    assertSinglesAndRects [(0,1)] [] $ grid 2 2 [(0,1)],

  testCase "handles multiple singles" $
    assertSinglesAndRects [(0,0), (1,1)] [] $ grid 2 2 [(0,0), (1,1)],

  testCase "handles a small rect (1x2)" $
    assertSinglesAndRects [] [((0,1), (0,2))] $ grid 1 3 [(0,1), (0,2)],

  testCase "handles a small square (2x2)" $
    assertSinglesAndRects [] [((1,1), (2,2))] $ grid 4 4 [(x,y) | x <- [1..2], y <- [1..2]],

  testCase "handles a larger rect" $
    assertSinglesAndRects [] [((0,0), (2,4))] $ grid 3 5 [(x,y) | x <- [0..2], y <- [0..4]],

  testCase "handles two rects" $
    assertSinglesAndRects [] [((0,1), (1,2)), ((3,0), (4,1))] $
    grid 5 3 [(0,1), (1,1), (0,2), (1,2), (3,0), (4,0), (3,1), (4,1)],

  testCase "handles rects and singles together" $
    assertSinglesAndRects [(0,0)] [((1,1), (2,2))] $
    grid 3 3 [(0,0), (1,1), (1,2), (2,1), (2,2)],

  testCase "handles five points together" $ do
    assertGrid 2 3 [(1,0), (0,1), (1,1), (0,2), (1,2)]
    assertGrid 2 3 [(0,0), (1,0), (0,1), (1,1), (0,2)]
    assertGrid 3 2 [(0,0), (1,0), (2,0), (0,1), (1,1)]
    assertGrid 3 2 [(1,0), (2,0), (0,1), (1,1), (2,1)],

  testCase "handles a more complex case" $
    assertGrid 5 5 [(3,0), (4,0),
                    (3,1), (4,1),
                    (0,2), (1,2), (2,2), (3,2), (4,2),
                    (0,3), (1,3),
                    (0,4), (1,4), (3,4), (4,4)]
  ]
  where -- | Constructs a grid of booleans with the given coordinates set to
        -- true.
        grid :: Int -> Int -> [Coord] -> [[Bool]]
        grid width height trues = [[(x,y) `elem` trues | x <- [0..width-1]] | y <- [0..height-1]]

        -- | Asserts that the given grid of booleans parses to a 'CoordList'
        -- that has the expected single points (not necessarily in the same
        -- order), as well as the expected rectangles (not necessarily in the
        -- same order).
        assertSinglesAndRects :: [Coord] -> [(Coord, Coord)] -> [[Bool]] -> Assertion
        assertSinglesAndRects expectedSingles expectedRects input =
          coords' expectedSingles expectedRects @=? buildCoordList input

        -- | Asserts that the set of points returned from applying
        -- 'buildCoordList' to the given grid of booleans is equal to the given
        -- set of points.
        assertCoords :: [Coord] -> [[Bool]] -> Assertion
        assertCoords expectedCoords input =
          sort expectedCoords @=? sort (expandCoordList $ buildCoordList input)

        -- | Asserts that, on a grid of the given width and height and only the
        -- given points set to true, that the set of points returned from
        -- applying 'buildCoordList' to this grid is equal to the set of points
        -- given as input.
        assertGrid :: Int -> Int -> [Coord] -> Assertion
        assertGrid width height trues = assertCoords trues $ grid width height trues

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
