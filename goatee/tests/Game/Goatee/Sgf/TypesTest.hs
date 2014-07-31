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

module Game.Goatee.Sgf.TypesTest (tests) where

import Data.List (sort)
import Game.Goatee.Sgf.Types
import Game.Goatee.Test.Common
import Test.HUnit ((~:), (@=?), (@?=), Assertion, Test (TestList))

tests = "Game.Goatee.Sgf.Types" ~: TestList [
  expandCoordListTests,
  buildCoordListTests,
  starLinesTests,
  isStarPointTests,
  handicapStonesTests,
  simpleTextTests,
  cnotTests
  ]

expandCoordListTests = "expandCoordList" ~: TestList [
  "works with an empty CoordList" ~: do
    [] @=? expandCoordList (coords [])
    [] @=? expandCoordList (coords' [] []),

  "works for single points" ~: do
    [(1,2)] @=? expandCoordList (coord1 (1,2))
    [(3,4), (1,2)] @=? expandCoordList (coords [(3,4), (1,2)])
    let ten = [(i,i) | i <- [1..10]]
    ten @=? expandCoordList (coords ten),

  -- TODO Test that a 1x1 rectangle is rejected.

  "works for a nx1 rect" ~:
    [(5,2), (5,3), (5,4)] @=? expandCoordList (coords' [] [((5,2), (5,4))]),

  "works for a 1xn rect" ~:
    [(1,0), (1,1), (1,2), (1,3)] @=? expandCoordList (coords' [] [((1,0), (1,3))]),

  "works for an mxn rect" ~:
    [(m,n) | m <- [2..5], n <- [3..7]] @=?
    expandCoordList (coords' [] [((2,3), (5,7))]),

  -- TODO Test that x0 > x1 || y0 > y1 is rejected.

  "works with multiple rects" ~:
    [(0,0), (0,1), (0,2), (3,4), (4,4), (5,4)] @=?
    expandCoordList (coords' [] [((0,0), (0,2)), ((3,4), (5,4))]),

  "concatenates single points and rects" ~:
    [(1,1), (0,0), (2,2), (2,3), (2,4)] @=?
    expandCoordList (coords' [(1,1), (0,0)] [((2,2), (2,4))])
  ]

buildCoordListTests = "buildCoordList" ~: TestList [
  "handles the empty case" ~: assertSinglesAndRects [] [] [],

  "handles one single" ~:
    assertSinglesAndRects [(0,1)] [] [(0,1)],

  "handles multiple singles" ~:
    assertSinglesAndRects [(0,0), (1,1)] [] [(0,0), (1,1)],

  "handles a small rect (1x2)" ~:
    assertSinglesAndRects [] [((0,1), (0,2))] [(0,1), (0,2)],

  "handles a small square (2x2)" ~:
    assertSinglesAndRects [] [((1,1), (2,2))] [(x,y) | x <- [1..2], y <- [1..2]],

  "handles a larger rect" ~:
    assertSinglesAndRects [] [((0,0), (2,4))] [(x,y) | x <- [0..2], y <- [0..4]],

  "handles two rects" ~:
    assertSinglesAndRects [] [((0,1), (1,2)), ((3,0), (4,1))]
    [(0,1), (1,1), (0,2), (1,2), (3,0), (4,0), (3,1), (4,1)],

  "handles rects and singles together" ~:
    assertSinglesAndRects [(0,0)] [((1,1), (2,2))]
    [(0,0), (1,1), (1,2), (2,1), (2,2)],

  "handles five points together" ~: do
    assertCoords [(1,0), (0,1), (1,1), (0,2), (1,2)]
    assertCoords [(0,0), (1,0), (0,1), (1,1), (0,2)]
    assertCoords [(0,0), (1,0), (2,0), (0,1), (1,1)]
    assertCoords [(1,0), (2,0), (0,1), (1,1), (2,1)],

  "handles a more complex case" ~:
    assertCoords [(3,0), (4,0),
                  (3,1), (4,1),
                  (0,2), (1,2), (2,2), (3,2), (4,2),
                  (0,3), (1,3),
                  (0,4), (1,4), (3,4), (4,4)],

  "handles a diagonal pattern" ~:
    assertCoords [(0,0), (2,0), (4,0),
                  (1,1), (3,1), (5,1),
                  (2,2)]
  ]
  where -- | Asserts that the given grid of booleans parses to a 'CoordList'
        -- that has the expected single points (not necessarily in the same
        -- order), as well as the expected rectangles (not necessarily in the
        -- same order).
        assertSinglesAndRects :: [Coord] -> [(Coord, Coord)] -> [Coord] -> Assertion
        assertSinglesAndRects expectedSingles expectedRects input =
          coords' expectedSingles expectedRects @=? buildCoordList input

        -- | Asserts that the set of points returned from applying
        -- 'buildCoordList' to the given grid of booleans is equal to
        -- the given set of points.
        assertCoords :: [Coord] -> Assertion
        assertCoords input = sort input @=? sort (expandCoordList $ buildCoordList input)

starLinesTests = "starLines" ~: TestList [
  "knows 9x9 boards" ~: Just [2, 4, 6] @=? starLines 9 9,
  "knows 13x13 boards" ~: Just [3, 6, 9] @=? starLines 13 13,
  "knows 19x19 boards" ~: Just [3, 9, 15] @=? starLines 19 19,
  "doesn't know 13x19" ~: Nothing @=? starLines 13 19,
  "doesn't know 10x10" ~: Nothing @=? starLines 10 10
  ]

isStarPointTests = "isStarPoint" ~: TestList [
  "only knows correct star points on a 9x9 board" ~:
    let expected = [(x, y) | x <- [2, 4, 6], y <- [2, 4, 6]]
        actual = actualStarPoints 9 9
    in expected @=?* actual,

  "only knows correct star points on a 13x13 board" ~:
    let expected = [(x, y) | x <- [3, 6, 9], y <- [3, 6, 9]]
        actual = actualStarPoints 13 13
    in expected @=?* actual,

  "only knows correct star points on a 19x19 board" ~:
    let expected = [(x, y) | x <- [3, 9, 15], y <- [3, 9, 15]]
        actual = actualStarPoints 19 19
    in expected @=?* actual,

  "knows no star poinst on a 5x1 board" ~:
    [] @=? actualStarPoints 5 1,

  "knows no star points on a 15x15 board" ~:
    [] @=? actualStarPoints 15 15,

  "knows no star points on a 20x20 board" ~:
    [] @=? actualStarPoints 20 20
  ]
  where actualStarPoints width height =
          map fst $ filter snd
          [((x, y), isStarPoint width height x y) | x <- [0..width-1], y <- [0..height-1]]

handicapStonesTests = "handicapStones" ~: TestList [
  "returns nothing for invalid handicaps" ~: do
     Nothing @=? handicapStones 9 9 (-1)
     Nothing @=? handicapStones 9 9 10
     Nothing @=? handicapStones 4 4 25,

  "9x9 0 handicap" ~: assertHandicap 9 9 0 $ Just [],
  "9x9 1 handicap" ~: assertHandicap 9 9 1 $ Just [],
  "9x9 2 handicap" ~: assertHandicap 9 9 2 $ Just [(2,6), (6,2)],
  "9x9 3 handicap" ~: assertHandicap 9 9 3 $ Just [(2,6), (6,2), (6,6)],
  "9x9 4 handicap" ~: assertHandicap 9 9 4 $ Just [(2,2), (2,6), (6,2), (6,6)],
  "9x9 5 handicap" ~: assertHandicap 9 9 5 $ Just [(2,2), (2,6), (4,4), (6,2), (6,6)],
  "9x9 6 handicap" ~: assertHandicap 9 9 6 $
    Just [(2,2), (2,4), (2,6), (6,2), (6,4), (6,6)],
  "9x9 7 handicap" ~: assertHandicap 9 9 7 $
    Just [(2,2), (2,4), (2,6), (4,4), (6,2), (6,4), (6,6)],
  "9x9 8 handicap" ~: assertHandicap 9 9 8 $
    Just [(2,2), (2,4), (2,6), (4,2), (4,6), (6,2), (6,4), (6,6)],
  "9x9 9 handicap" ~: assertHandicap 9 9 9 $
    Just [(2,2), (2,4), (2,6), (4,2), (4,4), (4,6), (6,2), (6,4), (6,6)],

  "13x13 0 handicap" ~: assertHandicap 13 13 0 $ Just [],
  "13x13 1 handicap" ~: assertHandicap 13 13 1 $ Just [],
  "13x13 2 handicap" ~: assertHandicap 13 13 2 $ Just [(3,9), (9,3)],
  "13x13 3 handicap" ~: assertHandicap 13 13 3 $ Just [(3,9), (9,3), (9,9)],
  "13x13 4 handicap" ~: assertHandicap 13 13 4 $ Just [(3,3), (3,9), (9,3), (9,9)],
  "13x13 5 handicap" ~: assertHandicap 13 13 5 $ Just [(3,3), (3,9), (6,6), (9,3), (9,9)],
  "13x13 6 handicap" ~: assertHandicap 13 13 6 $
    Just [(3,3), (3,6), (3,9), (9,3), (9,6), (9,9)],
  "13x13 7 handicap" ~: assertHandicap 13 13 7 $
    Just [(3,3), (3,6), (3,9), (6,6), (9,3), (9,6), (9,9)],
  "13x13 8 handicap" ~: assertHandicap 13 13 8 $
    Just [(3,3), (3,6), (3,9), (6,3), (6,9), (9,3), (9,6), (9,9)],
  "13x13 9 handicap" ~: assertHandicap 13 13 9 $
    Just [(3,3), (3,6), (3,9), (6,3), (6,6), (6,9), (9,3), (9,6), (9,9)],

  "19x19 0 handicap" ~: assertHandicap 19 19 0 $ Just [],
  "19x19 1 handicap" ~: assertHandicap 19 19 1 $ Just [],
  "19x19 2 handicap" ~: assertHandicap 19 19 2 $ Just [(3,15), (15,3)],
  "19x19 3 handicap" ~: assertHandicap 19 19 3 $ Just [(3,15), (15,3), (15,15)],
  "19x19 4 handicap" ~: assertHandicap 19 19 4 $ Just [(3,3), (3,15), (15,3), (15,15)],
  "19x19 5 handicap" ~: assertHandicap 19 19 5 $
  Just [(3,3), (3,15), (9,9), (15,3), (15,15)],
  "19x19 6 handicap" ~: assertHandicap 19 19 6 $
    Just [(3,3), (3,9), (3,15), (15,3), (15,9), (15,15)],
  "19x19 7 handicap" ~: assertHandicap 19 19 7 $
    Just [(3,3), (3,9), (3,15), (9,9), (15,3), (15,9), (15,15)],
  "19x19 8 handicap" ~: assertHandicap 19 19 8 $
    Just [(3,3), (3,9), (3,15), (9,3), (9,15), (15,3), (15,9), (15,15)],
  "19x19 9 handicap" ~: assertHandicap 19 19 9 $
    Just [(3,3), (3,9), (3,15), (9,3), (9,9), (9,15), (15,3), (15,9), (15,15)]
  ]
  where assertHandicap width height handicap maybeExpectedStones =
          case (handicapStones width height handicap, maybeExpectedStones) of
            (Just actualStones, Just expectedStones) -> actualStones @?=* expectedStones
            (actual, _) -> actual @?= maybeExpectedStones

simpleTextTests = "SimpleText" ~: TestList [
  "accepts the empty string" ~:
    "" @=? fromSimpleText (toSimpleText ""),

  "passes through strings without newlines" ~: do
    "Hello." @=? fromSimpleText (toSimpleText "Hello.")
    "Bad for B." @=? fromSimpleText (toSimpleText "Bad for B.")
    "[4k?]" @=? fromSimpleText (toSimpleText "[4k?]")
    printableAsciiChars @=? fromSimpleText (toSimpleText printableAsciiChars),

  "converts newlines to spaces" ~: do
    " " @=? fromSimpleText (toSimpleText "\n")
    "Hello, world." @=? fromSimpleText (toSimpleText "Hello,\nworld.")
    "Hello,   world." @=? fromSimpleText (toSimpleText "Hello, \n world.")
    " Hello, world. " @=? fromSimpleText (toSimpleText "\nHello, world.\n")
  ]

cnotTests = "cnot" ~: TestList [
  "changes Black to White" ~: White @=? cnot Black,
  "changes White to Black" ~: Black @=? cnot White
  ]
