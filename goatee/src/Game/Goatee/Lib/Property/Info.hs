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
-- Import "Game.Goatee.Lib.Property" rather than importing this module.
module Game.Goatee.Lib.Property.Info (
  -- * Known property metadata
  propertyB,
  propertyKO,
  propertyMN,
  propertyW,

  propertyAB,
  propertyAE,
  propertyAW,
  propertyPL,

  propertyC,
  propertyDM,
  propertyGB,
  propertyGW,
  propertyHO,
  propertyN,
  propertyUC,
  propertyV,

  propertyBM,
  propertyDO,
  propertyIT,
  propertyTE,

  propertyAR,
  propertyCR,
  propertyDD,
  propertyLB,
  propertyLN,
  propertyMA,
  propertySL,
  propertySQ,
  propertyTR,

  propertyAP,
  propertyCA,
  propertyFF,
  propertyGM,
  propertyST,
  propertySZ,

  propertyAN,
  propertyBR,
  propertyBT,
  propertyCP,
  propertyDT,
  propertyEV,
  propertyGC,
  propertyGN,
  propertyON,
  propertyOT,
  propertyPB,
  propertyPC,
  propertyPW,
  propertyRE,
  propertyRO,
  propertyRU,
  propertySO,
  propertyTM,
  propertyUS,
  propertyWR,
  propertyWT,

  propertyBL,
  propertyOB,
  propertyOW,
  propertyWL,

  propertyVW,

  propertyHA,
  propertyKM,
  propertyTB,
  propertyTW,

  -- * Property metadata utilities
  allKnownDescriptors,
  propertyUnknown,
  propertyInfo,
  descriptorForName, descriptorForName',
  stoneAssignmentProperties, stoneAssignmentPropertyToStone, stoneToStoneAssignmentProperty,
  markProperty,
  ) where

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Game.Goatee.Lib.Property.Base
import Game.Goatee.Lib.Property.Value
import Game.Goatee.Lib.Types

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
$(defValuedProperty "LN" 'GeneralProperty False 'lineListPvt)
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
$(defValuedProperty "GC" 'GameInfoProperty False 'textPvt)
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

-- Timing properties.
$(defValuedProperty "BL" 'MoveProperty False 'realPvt)
$(defValuedProperty "OB" 'MoveProperty False 'integralPvt)
$(defValuedProperty "OW" 'MoveProperty False 'integralPvt)
$(defValuedProperty "WL" 'MoveProperty False 'realPvt)

-- Miscellaneous properties.
$(defValuedProperty "VW" 'GeneralProperty True 'coordElistPvt)

-- Go-specific properties.
$(defValuedProperty "HA" 'GameInfoProperty False 'integralPvt)
$(defValuedProperty "KM" 'GameInfoProperty False 'realPvt)
$(defValuedProperty "TB" 'GeneralProperty False 'coordElistPvt)
$(defValuedProperty "TW" 'GeneralProperty False 'coordElistPvt)

-- | A list of descriptors for all known 'Property's.
allKnownDescriptors :: [AnyDescriptor]
allKnownDescriptors =
  [ AnyDescriptor propertyB
  , AnyDescriptor propertyKO
  , AnyDescriptor propertyMN
  , AnyDescriptor propertyW

  , AnyDescriptor propertyAB
  , AnyDescriptor propertyAE
  , AnyDescriptor propertyAW
  , AnyDescriptor propertyPL

  , AnyDescriptor propertyC
  , AnyDescriptor propertyDM
  , AnyDescriptor propertyGB
  , AnyDescriptor propertyGW
  , AnyDescriptor propertyHO
  , AnyDescriptor propertyN
  , AnyDescriptor propertyUC
  , AnyDescriptor propertyV

  , AnyDescriptor propertyBM
  , AnyDescriptor propertyDO
  , AnyDescriptor propertyIT
  , AnyDescriptor propertyTE

  , AnyDescriptor propertyAR
  , AnyDescriptor propertyCR
  , AnyDescriptor propertyDD
  , AnyDescriptor propertyLB
  , AnyDescriptor propertyLN
  , AnyDescriptor propertyMA
  , AnyDescriptor propertySL
  , AnyDescriptor propertySQ
  , AnyDescriptor propertyTR

  , AnyDescriptor propertyAP
  , AnyDescriptor propertyCA
  , AnyDescriptor propertyFF
  , AnyDescriptor propertyGM
  , AnyDescriptor propertyST
  , AnyDescriptor propertySZ

  , AnyDescriptor propertyAN
  , AnyDescriptor propertyBR
  , AnyDescriptor propertyBT
  , AnyDescriptor propertyCP
  , AnyDescriptor propertyDT
  , AnyDescriptor propertyEV
  , AnyDescriptor propertyGC
  , AnyDescriptor propertyGN
  , AnyDescriptor propertyON
  , AnyDescriptor propertyOT
  , AnyDescriptor propertyPB
  , AnyDescriptor propertyPC
  , AnyDescriptor propertyPW
  , AnyDescriptor propertyRE
  , AnyDescriptor propertyRO
  , AnyDescriptor propertyRU
  , AnyDescriptor propertySO
  , AnyDescriptor propertyTM
  , AnyDescriptor propertyUS
  , AnyDescriptor propertyWR
  , AnyDescriptor propertyWT

  , AnyDescriptor propertyBL
  , AnyDescriptor propertyOB
  , AnyDescriptor propertyOW
  , AnyDescriptor propertyWL

  , AnyDescriptor propertyVW

  , AnyDescriptor propertyHA
  , AnyDescriptor propertyKM
  , AnyDescriptor propertyTB
  , AnyDescriptor propertyTW
  ]

-- | Builds a 'ValuedPropertyInfo' for an unknown property with the given name.
-- /Does not check that the name is actually unknown./
propertyUnknown :: String -> ValuedPropertyInfo UnknownPropertyValue
propertyUnknown name =
  ValuedPropertyInfo name GeneralProperty False
  (\x -> case x of
      UnknownProperty name' _ | name' == name -> True
      _ -> False)
  unknownPropertyPvt
  (\(UnknownProperty _ value) -> value)
  (UnknownProperty name)

-- | Returns a descriptor for any 'Property', known or unknown.  Because a
-- 'Property' has a 'Descriptor' instance, this function is not normally
-- necessary for use outside of this module, but it can be used to throw away a
-- value associated with a 'Property' and retain only the metadata.
propertyInfo :: Property -> AnyDescriptor
propertyInfo property = case property of
  B {} -> AnyDescriptor propertyB
  KO {} -> AnyDescriptor propertyKO
  MN {} -> AnyDescriptor propertyMN
  W {} -> AnyDescriptor propertyW

  AB {} -> AnyDescriptor propertyAB
  AE {} -> AnyDescriptor propertyAE
  AW {} -> AnyDescriptor propertyAW
  PL {} -> AnyDescriptor propertyPL

  C {} -> AnyDescriptor propertyC
  DM {} -> AnyDescriptor propertyDM
  GB {} -> AnyDescriptor propertyGB
  GW {} -> AnyDescriptor propertyGW
  HO {} -> AnyDescriptor propertyHO
  N {} -> AnyDescriptor propertyN
  UC {} -> AnyDescriptor propertyUC
  V {} -> AnyDescriptor propertyV

  BM {} -> AnyDescriptor propertyBM
  DO {} -> AnyDescriptor propertyDO
  IT {} -> AnyDescriptor propertyIT
  TE {} -> AnyDescriptor propertyTE

  AR {} -> AnyDescriptor propertyAR
  CR {} -> AnyDescriptor propertyCR
  DD {} -> AnyDescriptor propertyDD
  LB {} -> AnyDescriptor propertyLB
  LN {} -> AnyDescriptor propertyLN
  MA {} -> AnyDescriptor propertyMA
  SL {} -> AnyDescriptor propertySL
  SQ {} -> AnyDescriptor propertySQ
  TR {} -> AnyDescriptor propertyTR

  AP {} -> AnyDescriptor propertyAP
  CA {} -> AnyDescriptor propertyCA
  FF {} -> AnyDescriptor propertyFF
  GM {} -> AnyDescriptor propertyGM
  ST {} -> AnyDescriptor propertyST
  SZ {} -> AnyDescriptor propertySZ

  AN {} -> AnyDescriptor propertyAN
  BR {} -> AnyDescriptor propertyBR
  BT {} -> AnyDescriptor propertyBT
  CP {} -> AnyDescriptor propertyCP
  DT {} -> AnyDescriptor propertyDT
  EV {} -> AnyDescriptor propertyEV
  GC {} -> AnyDescriptor propertyGC
  GN {} -> AnyDescriptor propertyGN
  ON {} -> AnyDescriptor propertyON
  OT {} -> AnyDescriptor propertyOT
  PB {} -> AnyDescriptor propertyPB
  PC {} -> AnyDescriptor propertyPC
  PW {} -> AnyDescriptor propertyPW
  RE {} -> AnyDescriptor propertyRE
  RO {} -> AnyDescriptor propertyRO
  RU {} -> AnyDescriptor propertyRU
  SO {} -> AnyDescriptor propertySO
  TM {} -> AnyDescriptor propertyTM
  US {} -> AnyDescriptor propertyUS
  WR {} -> AnyDescriptor propertyWR
  WT {} -> AnyDescriptor propertyWT

  BL {} -> AnyDescriptor propertyBL
  OB {} -> AnyDescriptor propertyOB
  OW {} -> AnyDescriptor propertyOW
  WL {} -> AnyDescriptor propertyWL

  VW {} -> AnyDescriptor propertyVW

  HA {} -> AnyDescriptor propertyHA
  KM {} -> AnyDescriptor propertyKM
  TB {} -> AnyDescriptor propertyTB
  TW {} -> AnyDescriptor propertyTW

  UnknownProperty name _ -> AnyDescriptor $ propertyUnknown name

instance Descriptor Property where
  propertyName = propertyName . propertyInfo
  propertyType = propertyType . propertyInfo
  propertyInherited = propertyInherited . propertyInfo
  propertyPredicate = propertyPredicate . propertyInfo
  propertyValueParser = propertyValueParser . propertyInfo
  propertyValueRenderer = propertyValueRenderer . propertyInfo
  propertyValueRendererPretty = propertyValueRendererPretty . propertyInfo

descriptorsByName :: Map String AnyDescriptor
descriptorsByName = Map.fromList $ map (propertyName &&& id) allKnownDescriptors

-- | Returns a descriptor for the given property name.  The name does not have
-- to be for a known property; an unknown property will use 'propertyUnknown'.
descriptorForName :: String -> AnyDescriptor
descriptorForName name = fromMaybe (AnyDescriptor $ propertyUnknown name) $ descriptorForName' name

-- | Returns a descriptor for a known property with the given name, or 'Nothing'
-- if the name does not belong to a known property.
descriptorForName' :: String -> Maybe AnyDescriptor
descriptorForName' = flip Map.lookup descriptorsByName

-- | Descriptors for setup properties that assign stones to the board.  For use
-- with 'stoneAssignmentPropertyToStone' and 'stoneToStoneAssignmentProperty'.
stoneAssignmentProperties :: [AnyCoordListDescriptor]
stoneAssignmentProperties =
  [ AnyCoordListDescriptor propertyAB
  , AnyCoordListDescriptor propertyAE
  , AnyCoordListDescriptor propertyAW
  ]

-- | Converts a descriptor in 'stoneAssignmentProperties' to the type of stone
-- it assigns.
stoneAssignmentPropertyToStone :: AnyCoordListDescriptor -> Maybe Color
stoneAssignmentPropertyToStone (AnyCoordListDescriptor d) = case propertyName d of
  "AB" -> Just Black
  "AE" -> Nothing
  "AW" -> Just White
  _ -> error $ "stoneAssignmentPropertyToColor: " ++ show (propertyName d) ++
       " is not a stone assignment property."

-- | Converts a type of stone assignment to a descriptor in
-- 'stoneAssignmentProperties'.
stoneToStoneAssignmentProperty :: Maybe Color -> AnyCoordListDescriptor
stoneToStoneAssignmentProperty stone = case stone of
  Nothing -> AnyCoordListDescriptor propertyAE
  Just Black -> AnyCoordListDescriptor propertyAB
  Just White -> AnyCoordListDescriptor propertyAW

-- | Returns the descriptor for a mark.
markProperty :: Mark -> ValuedPropertyInfo CoordList
markProperty MarkCircle = propertyCR
markProperty MarkSelected = propertySL
markProperty MarkSquare = propertySQ
markProperty MarkTriangle = propertyTR
markProperty MarkX = propertyMA
