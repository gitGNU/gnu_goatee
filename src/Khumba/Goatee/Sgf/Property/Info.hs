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

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Property metadata declarations.
--
-- Import "Khumba.Goatee.Sgf.Property" rather than importing this module.
module Khumba.Goatee.Sgf.Property.Info where

import Khumba.Goatee.Sgf.Property.Base
import Khumba.Goatee.Sgf.Types

-- Move properties.
$(defValuedProperty "B" 'MoveProperty False 'maybeCoordPrinter)
$(defProperty "KO" 'MoveProperty False)
$(defValuedProperty "MN" 'MoveProperty False 'numberPrinter)
$(defValuedProperty "W" 'MoveProperty False 'maybeCoordPrinter)

-- Setup properties.
$(defValuedProperty "AB" 'SetupProperty False 'coordListPrinter)
$(defValuedProperty "AE" 'SetupProperty False 'coordListPrinter)
$(defValuedProperty "AW" 'SetupProperty False 'coordListPrinter)
$(defValuedProperty "PL" 'SetupProperty False 'colorPrinter)

-- Node annotation properties.
$(defValuedProperty "C" 'GeneralProperty False 'stringlikePrinter)
$(defValuedProperty "DM" 'GeneralProperty False 'doublePrinter)
$(defValuedProperty "GB" 'GeneralProperty False 'doublePrinter)
$(defValuedProperty "GW" 'GeneralProperty False 'doublePrinter)
$(defValuedProperty "HO" 'GeneralProperty False 'doublePrinter)
$(defValuedProperty "N" 'GeneralProperty False 'stringlikePrinter)
$(defValuedProperty "UC" 'GeneralProperty False 'doublePrinter)
$(defValuedProperty "V" 'GeneralProperty False 'realPrinter)

-- Move annotation properties.
$(defValuedProperty "BM" 'MoveProperty False 'doublePrinter)
$(defProperty "DO" 'MoveProperty False)
$(defProperty "IT" 'MoveProperty False)
$(defValuedProperty "TE" 'MoveProperty False 'doublePrinter)

-- Markup properties.
$(defValuedProperty "AR" 'GeneralProperty False 'coordPairListPrinter)
$(defValuedProperty "CR" 'GeneralProperty False 'coordListPrinter)
$(defValuedProperty "DD" 'GeneralProperty True 'coordListPrinter)
$(defValuedProperty "LB" 'GeneralProperty False 'labelListPrinter)
$(defValuedProperty "LN" 'GeneralProperty False 'coordPairListPrinter)
$(defValuedProperty "MA" 'GeneralProperty False 'coordListPrinter)
$(defValuedProperty "SL" 'GeneralProperty False 'coordListPrinter)
$(defValuedProperty "SQ" 'GeneralProperty False 'coordListPrinter)
$(defValuedProperty "TR" 'GeneralProperty False 'coordListPrinter)

-- Root properties.
propertyAP = makeValuedPropertyInfo "AP" RootProperty False
             (\x -> case x of { AP {} -> True; _ -> False })
             (\(x, y) -> '[' : printStringlike True x ++
                         ":" ++ printStringlike True y ++ "]")
             (\(AP x y) -> (x, y))
             (uncurry AP)
$(defValuedProperty "CA" 'RootProperty False 'stringlikePrinter)
$(defValuedProperty "FF" 'RootProperty False 'numberPrinter)
$(defValuedProperty "GM" 'RootProperty False 'numberPrinter)
$(defValuedProperty "ST" 'RootProperty False 'variationModePrinter)
propertySZ = makeValuedPropertyInfo "SZ" RootProperty False
             (\x -> case x of { SZ {} -> True; _ -> False })
             (\(x, y) -> '[' : show x ++ if x == y
                                         then "]"
                                         else ":" ++ show y ++ "]")
             (\(SZ x y) -> (x, y))
             (uncurry SZ)

-- Game info properties.
$(defValuedProperty "AN" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "BR" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "BT" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "CP" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "DT" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "EV" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "GC" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "GN" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "ON" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "OT" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "PB" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "PC" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "PW" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "RE" 'GameInfoProperty False 'gameResultPrinter)
$(defValuedProperty "RO" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "RU" 'GameInfoProperty False 'rulesetPrinter)
$(defValuedProperty "SO" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "TM" 'GameInfoProperty False 'realPrinter)
$(defValuedProperty "US" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "WR" 'GameInfoProperty False 'stringlikePrinter)
$(defValuedProperty "WT" 'GameInfoProperty False 'stringlikePrinter)

-- Miscellaneous properties.
$(defValuedProperty "VW" 'GeneralProperty True 'coordElistPrinter)

-- | Returns metadata about a property.
propertyInfo :: Property -> PropertyInfo
propertyInfo property = case property of
  B {} -> valuedPropertyInfoBase propertyB
  KO {} -> propertyKO
  MN {} -> valuedPropertyInfoBase propertyMN
  W {} -> valuedPropertyInfoBase propertyW

  AB {} -> valuedPropertyInfoBase propertyAB
  AE {} -> valuedPropertyInfoBase propertyAE
  AW {} -> valuedPropertyInfoBase propertyAW
  PL {} -> valuedPropertyInfoBase propertyPL

  C {} -> valuedPropertyInfoBase propertyC
  DM {} -> valuedPropertyInfoBase propertyDM
  GB {} -> valuedPropertyInfoBase propertyGB
  GW {} -> valuedPropertyInfoBase propertyGW
  HO {} -> valuedPropertyInfoBase propertyHO
  N {} -> valuedPropertyInfoBase propertyN
  UC {} -> valuedPropertyInfoBase propertyUC
  V {} -> valuedPropertyInfoBase propertyV

  BM {} -> valuedPropertyInfoBase propertyBM
  DO {} -> propertyDO
  IT {} -> propertyIT
  TE {} -> valuedPropertyInfoBase propertyTE

  AR {} -> valuedPropertyInfoBase propertyAR
  CR {} -> valuedPropertyInfoBase propertyCR
  DD {} -> valuedPropertyInfoBase propertyDD
  LB {} -> valuedPropertyInfoBase propertyLB
  LN {} -> valuedPropertyInfoBase propertyLN
  MA {} -> valuedPropertyInfoBase propertyMA
  SL {} -> valuedPropertyInfoBase propertySL
  SQ {} -> valuedPropertyInfoBase propertySQ
  TR {} -> valuedPropertyInfoBase propertyTR

  AP {} -> valuedPropertyInfoBase propertyAP
  CA {} -> valuedPropertyInfoBase propertyCA
  FF {} -> valuedPropertyInfoBase propertyFF
  GM {} -> valuedPropertyInfoBase propertyGM
  ST {} -> valuedPropertyInfoBase propertyST
  SZ {} -> valuedPropertyInfoBase propertySZ

  AN {} -> valuedPropertyInfoBase propertyAN
  BR {} -> valuedPropertyInfoBase propertyBR
  BT {} -> valuedPropertyInfoBase propertyBT
  CP {} -> valuedPropertyInfoBase propertyCP
  DT {} -> valuedPropertyInfoBase propertyDT
  EV {} -> valuedPropertyInfoBase propertyEV
  GC {} -> valuedPropertyInfoBase propertyGC
  GN {} -> valuedPropertyInfoBase propertyGN
  ON {} -> valuedPropertyInfoBase propertyON
  OT {} -> valuedPropertyInfoBase propertyOT
  PB {} -> valuedPropertyInfoBase propertyPB
  PC {} -> valuedPropertyInfoBase propertyPC
  PW {} -> valuedPropertyInfoBase propertyPW
  RE {} -> valuedPropertyInfoBase propertyRE
  RO {} -> valuedPropertyInfoBase propertyRO
  RU {} -> valuedPropertyInfoBase propertyRU
  SO {} -> valuedPropertyInfoBase propertySO
  TM {} -> valuedPropertyInfoBase propertyTM
  US {} -> valuedPropertyInfoBase propertyUS
  WR {} -> valuedPropertyInfoBase propertyWR
  WT {} -> valuedPropertyInfoBase propertyWT

  VW {} -> valuedPropertyInfoBase propertyVW

  UnknownProperty name _ ->
    makePropertyInfo name GeneralProperty False
    (\x -> case x of
        UnknownProperty name' _ | name' == name -> True
        _ -> False)
    unknownPropertyPrinter

instance Descriptor Property where
  propertyName = propertyName . propertyInfo
  propertyType = propertyType . propertyInfo
  propertyInherited = propertyInherited . propertyInfo
  propertyPredicate = propertyPredicate . propertyInfo
  propertyValuePrinter = propertyValuePrinter . propertyInfo

-- | Returns the descriptor for a mark.
markProperty :: Mark -> ValuedPropertyInfo CoordList
markProperty MarkCircle = propertyCR
markProperty MarkSelected = propertySL
markProperty MarkSquare = propertySQ
markProperty MarkTriangle = propertyTR
markProperty MarkX = propertyMA
