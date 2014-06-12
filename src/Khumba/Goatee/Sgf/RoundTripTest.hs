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

module Khumba.Goatee.Sgf.RoundTripTest (tests) where

import Data.Function (on)
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Parser
import Khumba.Goatee.Sgf.Printer
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.TestUtils
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), Assertion, assertFailure)

testCollection' :: Collection -> Assertion
testCollection' collection =
  let serialized = printCollection collection
  in case parseString serialized of
    Left error -> assertFailure $ "Failed to parse: " ++ error
    Right collection' -> do
      on (@?=) CollectionWithDeepEquality collection' collection
      let serializedAgain = printCollection collection'
      serializedAgain @?= serialized

-- | Returns an assertion that the given node round-trips okay.
testNode' :: Node -> Assertion
testNode' = testCollection' . Collection . (:[])

-- | Returns a test with the given string name that round-trips a single node.
testNode :: String -> Node -> Test
testNode label node = testCase label $ testNode' node

tests = testGroup "Khumba.Goatee.Sgf.RoundTripTest" [
  singleNodeGameTests,
  propertyValueTests
  ]

singleNodeGameTests = testGroup "games with single nodes" [
  testNode "empty game" $ node [],
  testNode "some default properties" $ node [FF 4, GM 1, ST defaultVariationMode]
  ]

propertyValueTests = testGroup "property values" [
  testGroup "point-valued properties" $
    for [0..maxBoardSize-1]
    (\row -> testNode ("row " ++ show row) $ node [B $ Just (0, row)]) ++
    for [0..maxBoardSize-1]
    (\col -> testNode ("row " ++ show col) $ node [B $ Just (col, 0)])
  ]

-- TODO Many more round-trip tests.
