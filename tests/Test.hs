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

import qualified Game.Goatee.CommonTest
import qualified Game.Goatee.Sgf.BoardTest
import qualified Game.Goatee.Sgf.MonadTest
import qualified Game.Goatee.Sgf.ParserTest
import qualified Game.Goatee.Sgf.Property.ParserTest
import qualified Game.Goatee.Sgf.PropertyTest
import qualified Game.Goatee.Sgf.RoundTripTest
import qualified Game.Goatee.Sgf.TreeTest
import qualified Game.Goatee.Sgf.TypesTest
import qualified Game.Goatee.Ui.Gtk.LatchTest
import Test.Framework (defaultMain)

tests = [ Game.Goatee.CommonTest.tests
        , Game.Goatee.Sgf.BoardTest.tests
        , Game.Goatee.Sgf.MonadTest.tests
        , Game.Goatee.Sgf.ParserTest.tests
        , Game.Goatee.Sgf.Property.ParserTest.tests
        , Game.Goatee.Sgf.PropertyTest.tests
        , Game.Goatee.Sgf.RoundTripTest.tests
        , Game.Goatee.Sgf.TreeTest.tests
        , Game.Goatee.Sgf.TypesTest.tests
        , Game.Goatee.Ui.Gtk.LatchTest.tests
        ]

main :: IO ()
main = defaultMain tests
