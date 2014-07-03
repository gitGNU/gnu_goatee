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

import qualified Khumba.Goatee.CommonTest
import qualified Khumba.Goatee.Sgf.BoardTest
import qualified Khumba.Goatee.Sgf.MonadTest
import qualified Khumba.Goatee.Sgf.ParserTest
import qualified Khumba.Goatee.Sgf.PropertyTest
import qualified Khumba.Goatee.Sgf.Property.ParserTest
import qualified Khumba.Goatee.Sgf.RoundTripTest
import qualified Khumba.Goatee.Sgf.TreeTest
import qualified Khumba.Goatee.Sgf.TypesTest
import qualified Khumba.Goatee.Ui.Gtk.LatchTest
import Test.Framework (defaultMain)

tests = [ Khumba.Goatee.CommonTest.tests
        , Khumba.Goatee.Sgf.BoardTest.tests
        , Khumba.Goatee.Sgf.MonadTest.tests
        , Khumba.Goatee.Sgf.ParserTest.tests
        , Khumba.Goatee.Sgf.PropertyTest.tests
        , Khumba.Goatee.Sgf.Property.ParserTest.tests
        , Khumba.Goatee.Sgf.RoundTripTest.tests
        , Khumba.Goatee.Sgf.TreeTest.tests
        , Khumba.Goatee.Sgf.TypesTest.tests
        , Khumba.Goatee.Ui.Gtk.LatchTest.tests
        ]

main :: IO ()
main = defaultMain tests
