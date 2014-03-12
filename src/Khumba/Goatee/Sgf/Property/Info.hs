{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Property metadata declarations.
--
-- Import "Khumba.Goatee.Sgf.Property" rather than importing this module.
module Khumba.Goatee.Sgf.Property.Info where

import Khumba.Goatee.Sgf.Property.Base

-- Move properties.
$(defValuedProperty "B" 'MoveProperty False)
$(defProperty "KO" 'MoveProperty False)
$(defValuedProperty "MN" 'MoveProperty False)
$(defValuedProperty "W" 'MoveProperty False)

-- Setup properties.
$(defValuedProperty "AB" 'SetupProperty False)
$(defValuedProperty "AE" 'SetupProperty False)
$(defValuedProperty "AW" 'SetupProperty False)
$(defValuedProperty "PL" 'SetupProperty False)

-- Node annotation properties.
$(defValuedProperty "C" 'GeneralProperty False)
$(defValuedProperty "DM" 'GeneralProperty False)
$(defValuedProperty "GB" 'GeneralProperty False)
$(defValuedProperty "GW" 'GeneralProperty False)
$(defValuedProperty "HO" 'GeneralProperty False)
$(defValuedProperty "N" 'GeneralProperty False)
$(defValuedProperty "UC" 'GeneralProperty False)
$(defValuedProperty "V" 'GeneralProperty False)

-- Move annotation properties.
$(defValuedProperty "BM" 'MoveProperty False)
$(defProperty "DO" 'MoveProperty False)
$(defProperty "IT" 'MoveProperty False)
$(defValuedProperty "TE" 'MoveProperty False)

-- Markup properties.
$(defValuedProperty "AR" 'GeneralProperty False)
$(defValuedProperty "CR" 'GeneralProperty False)
$(defValuedProperty "DD" 'GeneralProperty True)
$(defValuedProperty "LB" 'GeneralProperty False)
$(defValuedProperty "LN" 'GeneralProperty False)
$(defValuedProperty "MA" 'GeneralProperty False)
$(defValuedProperty "SL" 'GeneralProperty False)
$(defValuedProperty "SQ" 'GeneralProperty False)
$(defValuedProperty "TR" 'GeneralProperty False)

-- Root properties.
propertyAP = makeValuedPropertyInfo "AP" RootProperty False
             (\x -> case x of { AP {} -> True; _ -> False })
             (\(AP x y) -> (x, y))
             (uncurry AP)
$(defValuedProperty "CA" 'RootProperty False)
$(defValuedProperty "FF" 'RootProperty False)
$(defValuedProperty "GM" 'RootProperty False)
$(defValuedProperty "ST" 'RootProperty False)
propertySZ = makeValuedPropertyInfo "SZ" RootProperty False
             (\x -> case x of { SZ {} -> True; _ -> False })
             (\(SZ x y) -> (x, y))
             (uncurry SZ)

-- Game info properties.
$(defValuedProperty "AN" 'GameInfoProperty False)
$(defValuedProperty "BR" 'GameInfoProperty False)
$(defValuedProperty "BT" 'GameInfoProperty False)
$(defValuedProperty "CP" 'GameInfoProperty False)
$(defValuedProperty "DT" 'GameInfoProperty False)
$(defValuedProperty "EV" 'GameInfoProperty False)
$(defValuedProperty "GC" 'GameInfoProperty False)
$(defValuedProperty "GN" 'GameInfoProperty False)
$(defValuedProperty "ON" 'GameInfoProperty False)
$(defValuedProperty "OT" 'GameInfoProperty False)
$(defValuedProperty "PB" 'GameInfoProperty False)
$(defValuedProperty "PC" 'GameInfoProperty False)
$(defValuedProperty "PW" 'GameInfoProperty False)
$(defValuedProperty "RE" 'GameInfoProperty False)
$(defValuedProperty "RO" 'GameInfoProperty False)
$(defValuedProperty "RU" 'GameInfoProperty False)
$(defValuedProperty "SO" 'GameInfoProperty False)
$(defValuedProperty "TM" 'GameInfoProperty False)
$(defValuedProperty "US" 'GameInfoProperty False)
$(defValuedProperty "WR" 'GameInfoProperty False)
$(defValuedProperty "WT" 'GameInfoProperty False)

-- Miscellaneous properties.
$(defValuedProperty "VW" 'GeneralProperty True)

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

instance Descriptor Property where
  propertyName = propertyName . propertyInfo
  propertyType = propertyType . propertyInfo
  propertyInherited = propertyInherited . propertyInfo
  propertyPredicate = propertyPredicate . propertyInfo
