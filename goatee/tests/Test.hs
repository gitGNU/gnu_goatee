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

module Main (main) where

import qualified Game.Goatee.Common.BigfloatTest
import qualified Game.Goatee.CommonTest
import qualified Game.Goatee.Lib.BoardTest
import qualified Game.Goatee.Lib.MonadTest
import qualified Game.Goatee.Lib.ParserTest
import qualified Game.Goatee.Lib.Property.ParserTest
import qualified Game.Goatee.Lib.PropertyTest
import qualified Game.Goatee.Lib.RoundTripTest
import qualified Game.Goatee.Lib.TreeTest
import qualified Game.Goatee.Lib.TypesTest
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (errors, failures), Test (TestList), runTestTT)

tests :: Test
tests = TestList
  [ Game.Goatee.Common.BigfloatTest.tests
  , Game.Goatee.CommonTest.tests
  , Game.Goatee.Lib.BoardTest.tests
  , Game.Goatee.Lib.MonadTest.tests
  , Game.Goatee.Lib.ParserTest.tests
  , Game.Goatee.Lib.Property.ParserTest.tests
  , Game.Goatee.Lib.PropertyTest.tests
  , Game.Goatee.Lib.RoundTripTest.tests
  , Game.Goatee.Lib.TreeTest.tests
  , Game.Goatee.Lib.TypesTest.tests
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts > 0 || failures counts > 0
    then exitFailure
    else exitSuccess
