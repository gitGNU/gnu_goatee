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
-- Import "Game.Goatee.Sgf.Property" rather than importing this module.
module Game.Goatee.Sgf.Property.Info where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Game.Goatee.Sgf.Property.Base
import Game.Goatee.Sgf.Property.Value
import Game.Goatee.Sgf.Types

-- Move properties.
$(defValuedProperty "B" 'MoveProperty False 'movePvt)
$(defProperty "KO" 'MoveProperty False)
$(defValuedProperty "MN" 'MoveProperty False 'integralPvt)
$(defValuedProperty "W" 'MoveProperty False 'movePvt)

-- Setup properties.
$(defValuedProperty "AB" 'SetupProperty False 'coordListPvt)
$(defValuedProperty "AE" 'SetupProperty False 'coordListPvt)
$(defValuedProperty "AW" 'SetupProperty False 'coordListPvt)
$(defValuedProperty "PL" 'SetupProperty False 'colorPvt)

-- Node annotation properties.
$(defValuedProperty "C" 'GeneralProperty False 'textPvt)
$(defValuedProperty "DM" 'GeneralProperty False 'doublePvt)
$(defValuedProperty "GB" 'GeneralProperty False 'doublePvt)
$(defValuedProperty "GW" 'GeneralProperty False 'doublePvt)
$(defValuedProperty "HO" 'GeneralProperty False 'doublePvt)
$(defValuedProperty "N" 'GeneralProperty False 'simpleTextPvt)
$(defValuedProperty "UC" 'GeneralProperty False 'doublePvt)
$(defValuedProperty "V" 'GeneralProperty False 'realPvt)

-- Move annotation properties.
$(defValuedProperty "BM" 'MoveProperty False 'doublePvt)
$(defProperty "DO" 'MoveProperty False)
$(defProperty "IT" 'MoveProperty False)
$(defValuedProperty "TE" 'MoveProperty False 'doublePvt)

-- Markup properties.
$(defValuedProperty "AR" 'GeneralProperty False 'coordPairListPvt)
$(defValuedProperty "CR" 'GeneralProperty False 'coordListPvt)
$(defValuedProperty "DD" 'GeneralProperty True 'coordListPvt)
$(defValuedProperty "LB" 'GeneralProperty False 'labelListPvt)
$(defValuedProperty "LN" 'GeneralProperty False 'coordPairListPvt)
$(defValuedProperty "MA" 'GeneralProperty False 'coordListPvt)
$(defValuedProperty "SL" 'GeneralProperty False 'coordListPvt)
$(defValuedProperty "SQ" 'GeneralProperty False 'coordListPvt)
$(defValuedProperty "TR" 'GeneralProperty False 'coordListPvt)

-- Root properties.
propertyAP :: ValuedPropertyInfo (SimpleText, SimpleText)
propertyAP = ValuedPropertyInfo "AP" RootProperty False
             (\x -> case x of { AP {} -> True; _ -> False })
             simpleTextPairPvt
             (\(AP x y) -> (x, y))
             (uncurry AP)
$(defValuedProperty "CA" 'RootProperty False 'simpleTextPvt)
$(defValuedProperty "FF" 'RootProperty False 'integralPvt)  -- TODO Add parser validation.
$(defValuedProperty "GM" 'RootProperty False 'integralPvt)  -- TODO Add parser validation.
$(defValuedProperty "ST" 'RootProperty False 'variationModePvt)
propertySZ :: ValuedPropertyInfo (Int, Int)
propertySZ = ValuedPropertyInfo "SZ" RootProperty False
             (\x -> case x of { SZ {} -> True; _ -> False })
             sizePvt
             (\(SZ x y) -> (x, y))
             (uncurry SZ)

-- Game info properties.
$(defValuedProperty "AN" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "BR" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "BT" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "CP" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "DT" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "EV" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "GC" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "GN" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "ON" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "OT" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "PB" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "PC" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "PW" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "RE" 'GameInfoProperty False 'gameResultPvt)
$(defValuedProperty "RO" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "RU" 'GameInfoProperty False 'rulesetPvt)
$(defValuedProperty "SO" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "TM" 'GameInfoProperty False 'realPvt)
$(defValuedProperty "US" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "WR" 'GameInfoProperty False 'simpleTextPvt)
$(defValuedProperty "WT" 'GameInfoProperty False 'simpleTextPvt)

-- Miscellaneous properties.
$(defValuedProperty "VW" 'GeneralProperty True 'coordElistPvt)

propertyUnknown :: String -> ValuedPropertyInfo UnknownPropertyValue
propertyUnknown name =
  ValuedPropertyInfo name GeneralProperty False
  (\x -> case x of
      UnknownProperty name' _ | name' == name -> True
      _ -> False)
  unknownPropertyPvt
  (\(UnknownProperty _ value) -> value)
  (UnknownProperty name)

allDescriptors :: [SomeDescriptor]
allDescriptors = [
  SomeDescriptor propertyB
  , SomeDescriptor propertyKO
  , SomeDescriptor propertyMN
  , SomeDescriptor propertyW

  , SomeDescriptor propertyAB
  , SomeDescriptor propertyAE
  , SomeDescriptor propertyAW
  , SomeDescriptor propertyPL

  , SomeDescriptor propertyC
  , SomeDescriptor propertyDM
  , SomeDescriptor propertyGB
  , SomeDescriptor propertyGW
  , SomeDescriptor propertyHO
  , SomeDescriptor propertyN
  , SomeDescriptor propertyUC
  , SomeDescriptor propertyV

  , SomeDescriptor propertyBM
  , SomeDescriptor propertyDO
  , SomeDescriptor propertyIT
  , SomeDescriptor propertyTE

  , SomeDescriptor propertyAR
  , SomeDescriptor propertyCR
  , SomeDescriptor propertyDD
  , SomeDescriptor propertyLB
  , SomeDescriptor propertyLN
  , SomeDescriptor propertyMA
  , SomeDescriptor propertySL
  , SomeDescriptor propertySQ
  , SomeDescriptor propertyTR

  , SomeDescriptor propertyAP
  , SomeDescriptor propertyCA
  , SomeDescriptor propertyFF
  , SomeDescriptor propertyGM
  , SomeDescriptor propertyST
  , SomeDescriptor propertySZ

  , SomeDescriptor propertyAN
  , SomeDescriptor propertyBR
  , SomeDescriptor propertyBT
  , SomeDescriptor propertyCP
  , SomeDescriptor propertyDT
  , SomeDescriptor propertyEV
  , SomeDescriptor propertyGC
  , SomeDescriptor propertyGN
  , SomeDescriptor propertyON
  , SomeDescriptor propertyOT
  , SomeDescriptor propertyPB
  , SomeDescriptor propertyPC
  , SomeDescriptor propertyPW
  , SomeDescriptor propertyRE
  , SomeDescriptor propertyRO
  , SomeDescriptor propertyRU
  , SomeDescriptor propertySO
  , SomeDescriptor propertyTM
  , SomeDescriptor propertyUS
  , SomeDescriptor propertyWR
  , SomeDescriptor propertyWT

  , SomeDescriptor propertyVW
  ]

propertyInfo :: Property -> SomeDescriptor
propertyInfo property = case property of
  B {} -> SomeDescriptor propertyB
  KO {} -> SomeDescriptor propertyKO
  MN {} -> SomeDescriptor propertyMN
  W {} -> SomeDescriptor propertyW

  AB {} -> SomeDescriptor propertyAB
  AE {} -> SomeDescriptor propertyAE
  AW {} -> SomeDescriptor propertyAW
  PL {} -> SomeDescriptor propertyPL

  C {} -> SomeDescriptor propertyC
  DM {} -> SomeDescriptor propertyDM
  GB {} -> SomeDescriptor propertyGB
  GW {} -> SomeDescriptor propertyGW
  HO {} -> SomeDescriptor propertyHO
  N {} -> SomeDescriptor propertyN
  UC {} -> SomeDescriptor propertyUC
  V {} -> SomeDescriptor propertyV

  BM {} -> SomeDescriptor propertyBM
  DO {} -> SomeDescriptor propertyDO
  IT {} -> SomeDescriptor propertyIT
  TE {} -> SomeDescriptor propertyTE

  AR {} -> SomeDescriptor propertyAR
  CR {} -> SomeDescriptor propertyCR
  DD {} -> SomeDescriptor propertyDD
  LB {} -> SomeDescriptor propertyLB
  LN {} -> SomeDescriptor propertyLN
  MA {} -> SomeDescriptor propertyMA
  SL {} -> SomeDescriptor propertySL
  SQ {} -> SomeDescriptor propertySQ
  TR {} -> SomeDescriptor propertyTR

  AP {} -> SomeDescriptor propertyAP
  CA {} -> SomeDescriptor propertyCA
  FF {} -> SomeDescriptor propertyFF
  GM {} -> SomeDescriptor propertyGM
  ST {} -> SomeDescriptor propertyST
  SZ {} -> SomeDescriptor propertySZ

  AN {} -> SomeDescriptor propertyAN
  BR {} -> SomeDescriptor propertyBR
  BT {} -> SomeDescriptor propertyBT
  CP {} -> SomeDescriptor propertyCP
  DT {} -> SomeDescriptor propertyDT
  EV {} -> SomeDescriptor propertyEV
  GC {} -> SomeDescriptor propertyGC
  GN {} -> SomeDescriptor propertyGN
  ON {} -> SomeDescriptor propertyON
  OT {} -> SomeDescriptor propertyOT
  PB {} -> SomeDescriptor propertyPB
  PC {} -> SomeDescriptor propertyPC
  PW {} -> SomeDescriptor propertyPW
  RE {} -> SomeDescriptor propertyRE
  RO {} -> SomeDescriptor propertyRO
  RU {} -> SomeDescriptor propertyRU
  SO {} -> SomeDescriptor propertySO
  TM {} -> SomeDescriptor propertyTM
  US {} -> SomeDescriptor propertyUS
  WR {} -> SomeDescriptor propertyWR
  WT {} -> SomeDescriptor propertyWT

  VW {} -> SomeDescriptor propertyVW

  UnknownProperty name _ -> SomeDescriptor $ propertyUnknown name

instance Descriptor Property where
  propertyName = propertyName . propertyInfo
  propertyType = propertyType . propertyInfo
  propertyInherited = propertyInherited . propertyInfo
  propertyPredicate = propertyPredicate . propertyInfo
  propertyValueParser = propertyValueParser . propertyInfo
  propertyValueRenderer = propertyValueRenderer . propertyInfo
  propertyValueRendererPretty = propertyValueRendererPretty . propertyInfo

descriptorsByName :: Map String SomeDescriptor
descriptorsByName = Map.fromList $ map (propertyName &&& id) allDescriptors

descriptorForName :: String -> SomeDescriptor
descriptorForName name = fromMaybe (SomeDescriptor $ propertyUnknown name) $ descriptorForName' name

descriptorForName' :: String -> Maybe SomeDescriptor
descriptorForName' = flip Map.lookup descriptorsByName

-- | Returns the descriptor for a mark.
markProperty :: Mark -> ValuedPropertyInfo CoordList
markProperty MarkCircle = propertyCR
markProperty MarkSelected = propertySL
markProperty MarkSquare = propertySQ
markProperty MarkTriangle = propertyTR
markProperty MarkX = propertyMA
