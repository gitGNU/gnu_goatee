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

module Game.Goatee.Sgf.RoundTripTest (tests) where

import Data.Function (on)
import Game.Goatee.Common
import Game.Goatee.Sgf.Parser
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.Renderer
import Game.Goatee.Sgf.Renderer.Tree
import Game.Goatee.Sgf.TestUtils
import Game.Goatee.Sgf.Tree
import Game.Goatee.Sgf.Types
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion, assertFailure)

testCollection' :: Collection -> Assertion
testCollection' collection =
  case runRender $ renderCollection collection of
    Left message -> assertFailure $ "Failed to render: " ++ message
    Right serialized ->
      case parseString serialized of
        Left error -> assertFailure $ "Failed to parse: " ++ error ++
                      "\n\nEntire SGF: " ++ serialized
        Right collection' -> do
          on (@?=) CollectionWithDeepEquality collection' collection
          case runRender $ renderCollection collection' of
            Left message -> assertFailure $ "Second render failed: " ++ message
            Right serializedAgain -> serializedAgain @?= serialized

-- | Returns an assertion that the given node round-trips okay.
testNode' :: Node -> Assertion
testNode' = testCollection' . Collection . (:[])

-- | Returns a test with the given string name that round-trips a single node.
testNode :: String -> Node -> Test
testNode label node = testCase label $ testNode' node

tests = testGroup "Game.Goatee.Sgf.RoundTripTest" [
  singleNodeGameTests,
  propertyValueTests
  ]

singleNodeGameTests = testGroup "games with single nodes" [
  testNode "empty game" $ node [],
  testNode "some default properties" $ node [FF 4, GM 1, ST defaultVariationMode]
  ]

propertyValueTests = testGroup "property value types" [
  testGroup "label list values" [
    testNode "one value" $ node [LB [((5,2), toSimpleText "Hi.")]],
    testNode "multiple value" $ node [LB [((5, 2), toSimpleText "Hi."),
                                          ((0, 1), toSimpleText "Bye.")]]
    ],

  testGroup "point-valued values" $
    for [0..boardSizeMax-1]
    (\row -> testNode ("row " ++ show row) $ node [B $ Just (0, row)]) ++
    for [0..boardSizeMax-1]
    (\col -> testNode ("row " ++ show col) $ node [B $ Just (col, 0)]),

  testGroup "real values" [
    testNode "1500" $ node [TM 1500],
    testNode "60.5" $ node [TM 60.5],
    testNode "10.1" $ node [TM 10.1]
    ]
  ]

-- TODO Many more round-trip tests.
