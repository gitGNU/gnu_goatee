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

module Khumba.Goatee.Sgf.PropertyTest (tests) where

import qualified Data.Set as Set
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Types
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.Goatee.Sgf.Property" [
  propertyMetadataTests,
  markPropertyTests
  ]

propertyMetadataTests = testGroup "property metadata" [
  testCase "game info properties" $ gameInfoProperties @=? filterTo GameInfoProperty allProperties,
  testCase "general properties" $ generalProperties @=? filterTo GeneralProperty allProperties,
  testCase "move properties" $ moveProperties @=? filterTo MoveProperty allProperties,
  testCase "root properties" $ rootProperties @=? filterTo RootProperty allProperties,
  testCase "setup properties" $ setupProperties @=? filterTo SetupProperty allProperties,

  testCase "inherited properties" $ [DD cl] @=? filter propertyInherited allProperties
  ]
  where filterTo propType = filter ((propType ==) . propertyType)
        moveProperties = [-- Move properties.
                          B Nothing, KO, MN 1, W Nothing,
                          -- Move annotation properties.
                          BM db, DO, IT, TE db]
        setupProperties = [-- Setup properties.
                           AB cl, AE cl, AW cl, PL Black]
        generalProperties = [-- Node annotation properties.
                             C tx, DM db, GB db, GW db, HO db, N st, UC db, V rv,
                             -- Markup properties.
                             AR [], CR cl, DD cl, LB [], LN [], MA cl, SL cl, SQ cl, TR cl,
                             -- Guess this fits here.
                             UnknownProperty "" ""]
        rootProperties = [-- Root properties.
                          AP st st, CA st, FF 1, GM 1, ST vm, SZ 1 1]
        gameInfoProperties = [-- Game info properties.
                              AN st, BR st, BT st, CP st, DT st, EV st, GC st, GN st, ON st, OT st,
                              PB st, PC st, PW st, RE GameResultVoid, RO st, RU ru,
                              SO st, TM rv, US st, WR st, WT st]
        allProperties = moveProperties ++ setupProperties ++ generalProperties ++
                        rootProperties ++ gameInfoProperties
        cl = emptyCoordList
        db = Double1
        tx = toText ""
        st = toSimpleText ""
        ru = KnownRuleset RulesetJapanese
        rv = 1
        vm = defaultVariationMode

markPropertyTests = testGroup "markProperty" [
  testCase "doesn't repeat properties" $
    let marks = [minBound..maxBound]
    in length marks @=? Set.size (Set.fromList $ map (propertyName . markProperty) marks)
  ]
