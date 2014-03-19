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

import Khumba.Goatee.Sgf.Types
import Khumba.Goatee.Test.Common
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.Goatee.Sgf.Types" [
  expandCoordListTests,
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
