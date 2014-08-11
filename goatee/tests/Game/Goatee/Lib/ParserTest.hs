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

module Game.Goatee.Lib.ParserTest (tests) where

import Control.Monad (forM_)
import Game.Goatee.Lib.ParserTestUtils
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.TestInstances ()
import Game.Goatee.Lib.TestUtils
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import Test.HUnit ((~:), (@?=), Test (TestList))

tests = "Game.Goatee.Lib.Parser" ~: TestList [
  baseCaseTests,
  whitespaceTests,
  passConversionTests,
  movePropertyTests,
  setupPropertyTests,
  nodeAnnotationPropertyTests,
  moveAnnotationPropertyTests,
  rootPropertyTests
  ]

baseCaseTests = "base cases" ~: TestList [
  "works with the trivial collection" ~:
    parseOrFail "(;)" (@?= emptyNode),

  "works with only a size property" ~: do
    parseOrFail "(;SZ[1])" (@?= root 1 1 [] [])
    parseOrFail "(;SZ[9])" (@?= root 9 9 [] [])
  ]

whitespaceTests = "whitespace handling" ~: TestList [
  "parses with no extra whitespace" ~:
    parseOrFail "(;SZ[4];AB[aa][bb]AW[cc];W[dd])"
    (@?= root 4 4 [] [node1 [AB $ coords [(0,0), (1,1)], AW $ coords [(2,2)]] $
                      node [W $ Just (3,3)]]),

  "parses with spaces between nodes" ~:
    parseOrFail "(;SZ[1] ;B[])" (@?= root 1 1 [] [node [B Nothing]]),

  "parses with spaces between properties" ~:
    parseOrFail "(;SZ[1] AB[aa])" (@?= root 1 1 [AB $ coords [(0,0)]] []),

  "parses with spaces between a property's name and value" ~:
    parseOrFail "(;SZ [1])" (@?= root 1 1 [] []),

  "parses with spaces between property values" ~:
    parseOrFail "(;SZ[2]AB[aa] [bb])" (@?= root 2 2 [AB $ coords [(0,0), (1,1)]] []),

  "parses with spaces between many elements" ~:
    parseOrFail " ( ; SZ [4] ; AB [aa:ad] [bb] AW [cc] ; W [dd] ; B [] ) "
    (@?= root 4 4 [] [node1 [AB $ coords' [(1,1)] [((0,0), (0,3))],
                             AW $ coords [(2,2)]] $
                      node1 [W $ Just (3,3)] $
                      node [B Nothing]])

  -- TODO Test handling of whitespace between an unknown property name and
  -- [value].
  ]

passConversionTests = "B[tt]/W[tt] pass conversion" ~: TestList [
  "converts B[tt] for a board sizes <=19x19" ~: do
    parseOrFail "(;SZ[1];B[tt])" (@?= root 1 1 [] [node [B Nothing]])
    parseOrFail "(;SZ[9];B[tt])" (@?= root 9 9 [] [node [B Nothing]])
    parseOrFail "(;SZ[19];B[tt])" (@?= root 19 19 [] [node [B Nothing]]),

  "converts W[tt] for a board sizes <=19x19" ~: do
    parseOrFail "(;SZ[1];W[tt])" (@?= root 1 1 [] [node [W Nothing]])
    parseOrFail "(;SZ[9];W[tt])" (@?= root 9 9 [] [node [W Nothing]])
    parseOrFail "(;SZ[19];W[tt])" (@?= root 19 19 [] [node [W Nothing]]),

  "doesn't convert B[tt] for a board sizes >19x19" ~: do
    parseOrFail "(;SZ[20];B[tt])" (@?= root 20 20 [] [node [B $ Just (19, 19)]])
    parseOrFail "(;SZ[21];B[tt])" (@?= root 21 21 [] [node [B $ Just (19, 19)]]),

  "doesn't convert W[tt] for a board sizes >19x19" ~: do
    parseOrFail "(;SZ[20];W[tt])" (@?= root 20 20 [] [node [W $ Just (19, 19)]])
    parseOrFail "(;SZ[21];W[tt])" (@?= root 21 21 [] [node [W $ Just (19, 19)]]),

  "doesn't convert non-move properties" ~: do
    -- TODO These should error, rather than parsing fine.
    parseOrFail "(;SZ[9];AB[tt])" (@?= root 9 9 [] [node [AB $ coords [(19, 19)]]])
    parseOrFail "(;SZ[9];TR[tt])" (@?= root 9 9 [] [node [TR $ coords [(19, 19)]]])
  ]

movePropertyTests = "move properties" ~: TestList [
  "B" ~: TestList [
    "parses moves" ~: do
      parseOrFail "(;B[aa])" (@?= node [B $ Just (0, 0)])
      parseOrFail "(;B[cp])" (@?= node [B $ Just (2, 15)]),
    "parses passes" ~:
      parseOrFail "(;B[])" (@?= node [B Nothing])
    ],
  "KO parses" ~:
    parseOrFail "(;KO[])" (@?= node [KO]),
  -- TODO Test MN (assert positive).
  "W" ~: TestList [
    "parses moves" ~: do
      parseOrFail "(;W[aa])" (@?= node [W $ Just (0, 0)])
      parseOrFail "(;W[cp])" (@?= node [W $ Just (2, 15)]),
    "parses passes" ~:
      parseOrFail "(;W[])" (@?= node [W Nothing])
    ]
  ]

setupPropertyTests = "setup properties" ~: TestList [
  "AB parses" ~: do
    parseOrFail "(;AB[ab])" (@?= node [AB $ coords [(0, 1)]])
    parseOrFail "(;AB[ab][cd:ef])" (@?= node [AB $ coords' [(0, 1)] [((2, 3), (4, 5))]]),
  "AE parses" ~: do
    parseOrFail "(;AE[ae])" (@?= node [AE $ coords [(0, 4)]])
    parseOrFail "(;AE[ae][ff:gg])" (@?= node [AE $ coords' [(0, 4)] [((5, 5), (6, 6))]]),
  "AW parses" ~: do
    parseOrFail "(;AW[aw])" (@?= node [AW $ coords [(0, 22)]])
    parseOrFail "(;AW[aw][xy:yz])" (@?= node [AW $ coords' [(0, 22)] [((23, 24), (24, 25))]]),
  "PL parses" ~: do
    parseOrFail "(;PL[B])" (@?= node [PL Black])
    parseOrFail "(;PL[W])" (@?= node [PL White])
  ]

nodeAnnotationPropertyTests = "node annotation properties" ~: TestList [
  "C parses" ~:
    parseOrFail "(;C[Me [30k\\]: What is White doing??\\\n\n:(])"
      (@?= node [C $ toText "Me [30k]: What is White doing??\n:("]),
  "DM parses" ~: do
    parseOrFail "(;DM[1])" (@?= node [DM Double1])
    parseOrFail "(;DM[2])" (@?= node [DM Double2]),
  "GB parses" ~: do
    parseOrFail "(;GB[1])" (@?= node [GB Double1])
    parseOrFail "(;GB[2])" (@?= node [GB Double2]),
  "GW parses" ~: do
    parseOrFail "(;GW[1])" (@?= node [GW Double1])
    parseOrFail "(;GW[2])" (@?= node [GW Double2]),
  "HO parses" ~: do
    parseOrFail "(;HO[1])" (@?= node [HO Double1])
    parseOrFail "(;HO[2])" (@?= node [HO Double2]),
  "N parses" ~:
    parseOrFail "(;N[The best\\\n\nmove])" (@?= node [N $ toSimpleText "The best move"]),
  "UC parses" ~: do
    parseOrFail "(;UC[1])" (@?= node [UC Double1])
    parseOrFail "(;UC[2])" (@?= node [UC Double2]),
  "V parses" ~: do
    parseOrFail "(;V[-34.5])" (@?= node [V $ read "-34.5"])
    parseOrFail "(;V[50])" (@?= node [V $ read "50"])
  ]

moveAnnotationPropertyTests = "move annotation properties" ~: TestList [
  "BM parses" ~: do
    parseOrFail "(;BM[1])" (@?= node [BM Double1])
    parseOrFail "(;BM[2])" (@?= node [BM Double2]),
  "DO parses" ~:
    parseOrFail "(;DO[])" (@?= node [DO]),
  "IT parses" ~:
    parseOrFail "(;IT[])" (@?= node [IT]),
  "TE parses" ~: do
    parseOrFail "(;TE[1])" (@?= node [TE Double1])
    parseOrFail "(;TE[2])" (@?= node [TE Double2])
  ]

-- TODO Test markup properties.

rootPropertyTests = "root properties" ~: TestList [
  "AP parses" ~:
    parseOrFail "(;AP[GoGoGo:1.2.3])"
    (@?= node [AP (toSimpleText "GoGoGo") (toSimpleText "1.2.3")]),
  "CA parses" ~: do
    parseOrFail "(;CA[UTF-8])" (@?= node [CA $ toSimpleText "UTF-8"])
    parseOrFail "(;CA[ISO-8859-1])" (@?= node [CA $ toSimpleText "ISO-8859-1"]),
  "FF" ~: TestList [
    "accepts version 4" ~:
      parseOrFail "(;FF[4])" (@?= node [FF 4]),
    "rejects versions 1-3" ~: do
      parseAndFail "(;FF[1])"
      parseAndFail "(;FF[2])"
      parseAndFail "(;FF[3])"
    ],
  "GM" ~: TestList [
    "parses 1 (Go)" ~:
      parseOrFail "(;GM[1])" (@?= node [GM 1]),
    "rejects unsupported games" ~:
      forM_ [2..16] $ \x -> parseAndFail $ "(;GM[" ++ show x ++ "]"
    ],
  "ST" ~: TestList [
    "parses valid variation modes" ~: do
      parseOrFail "(;ST[0])" (@?= node [ST $ VariationMode ShowChildVariations True])
      parseOrFail "(;ST[1])" (@?= node [ST $ VariationMode ShowCurrentVariations True])
      parseOrFail "(;ST[2])" (@?= node [ST $ VariationMode ShowChildVariations False])
      parseOrFail "(;ST[3])" (@?= node [ST $ VariationMode ShowCurrentVariations False]),
    "rejects invalid variation modes" ~:
      forM_ [4..10] $ \x -> parseAndFail $ "(;ST[" ++ show x ++ "]"
    ],
  "SZ" ~: TestList [
    "parses square boards" ~:
      forM_ [1..52] $ \x ->
        parseOrFail ("(;SZ[" ++ show x ++ "])") (@?= node [SZ x x]),
    "parses nonsquare boards" ~: do
      parseOrFail "(;SZ[1:2])" (@?= node [SZ 1 2])
      parseOrFail "(;SZ[1:9])" (@?= node [SZ 1 9])
      parseOrFail "(;SZ[1:19])" (@?= node [SZ 1 19])
      parseOrFail "(;SZ[2:1])" (@?= node [SZ 2 1])
      parseOrFail "(;SZ[9:1])" (@?= node [SZ 9 1])
      parseOrFail "(;SZ[19:1])" (@?= node [SZ 19 1])
      parseOrFail "(;SZ[19:52])" (@?= node [SZ 19 52])
      parseOrFail "(;SZ[10:16])" (@?= node [SZ 10 16]),
    "rejects invalid sizes" ~: do
      -- Boards must have length at least 1...
      parseAndFail "(;SZ[0])"
      parseAndFail "(;SZ[-1])"
      -- ...and at most 52.
      parseAndFail "(;SZ[53])"
      parseAndFail "(;SZ[54])"
      parseAndFail "(;SZ[0:19])"
      parseAndFail "(;SZ[19:0])"
      parseAndFail "(;SZ[9:53])"
      parseAndFail "(;SZ[53:9])",
    -- This is specified by the SGF spec:
    "rejects square boards defined with two numbers" ~: do
      parseAndFail "(;SZ[1:1])"
      parseAndFail "(;SZ[19:19])"
    ]
  ]

-- TODO Test the test of the properties.
