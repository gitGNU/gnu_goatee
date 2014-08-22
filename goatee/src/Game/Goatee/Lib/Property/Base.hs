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

-- | Core property-related data types, and some Template Haskell declarations
-- for defining property metadata.
--
-- Import "Game.Goatee.Lib.Property" rather than importing this module.
module Game.Goatee.Lib.Property.Base (
  -- * Properties
  Property (..),
  -- * Property metadata
  PropertyType (..),
  Descriptor (..),
  SomeDescriptor (..),
  ValuedDescriptor (..),
  PropertyInfo,
  ValuedPropertyInfo (ValuedPropertyInfo),
  -- * Property declaration
  defProperty, defValuedProperty,
  ) where

import Control.Applicative ((<$))
import Game.Goatee.Lib.Property.Value (PropertyValueType(..), nonePvt)
import Game.Goatee.Lib.Renderer
import Game.Goatee.Lib.Types
import Language.Haskell.TH (
  Info (DataConI), DecsQ, Name, Type (AppT),
  appE, appT, caseE, conE, conP, conT, lam1E, match, mkName, newName,
  normalB, recP, reify, sigD, stringE, valD, varE, varP, wildP,
  )
import Text.ParserCombinators.Parsec (Parser)

-- | An SGF property that gives a node meaning.
data Property =
  -- Move properties.
    B (Maybe Coord)      -- ^ Black move (nothing iff pass).
  | KO                   -- ^ Execute move unconditionally (even if illegal).
  | MN Integer           -- ^ Assign move number.
  | W (Maybe Coord)      -- ^ White move (nothing iff pass).

  -- Setup properties.
  | AB CoordList         -- ^ Assign black stones.
  | AE CoordList         -- ^ Assign empty stones.
  | AW CoordList         -- ^ Assign white stones.
  | PL Color             -- ^ Player to play.

  -- Node annotation properties.
  | C Text               -- ^ Comment.
  | DM DoubleValue       -- ^ Even position.
  | GB DoubleValue       -- ^ Good for black.
  | GW DoubleValue       -- ^ Good for white.
  | HO DoubleValue       -- ^ Hotspot.
  | N SimpleText         -- ^ Node name.
  | UC DoubleValue       -- ^ Unclear position.
  | V RealValue          -- ^ Node value.

  -- Move annotation properties.
  | BM DoubleValue       -- ^ Bad move.
  | DO                   -- ^ Doubtful move.
  | IT                   -- ^ Interesting move.
  | TE DoubleValue       -- ^ Tesuji.

  -- Markup properties.
  | AR ArrowList         -- ^ Arrows.
  | CR CoordList         -- ^ Mark points with circles.
  | DD CoordList         -- ^ Dim points.
  | LB LabelList         -- ^ Label points with text.
  | LN LineList          -- ^ Lines.
  | MA CoordList         -- ^ Mark points with 'X's.
  | SL CoordList         -- ^ Mark points as selected.
  | SQ CoordList         -- ^ Mark points with squares.
  | TR CoordList         -- ^ Mark points with trianges.

  -- Root properties.
  | AP SimpleText SimpleText -- ^ Application info.
  | CA SimpleText        -- ^ Charset for SimpleText and Text.
  | FF Int               -- ^ File format version.
  | GM Int               -- ^ Game (must be 1 = Go).
  | ST VariationMode     -- ^ Variation display format.
  | SZ Int Int           -- ^ Board size, columns then rows.

  -- Game info properties.
  | AN SimpleText        -- ^ Name of annotator.
  | BR SimpleText        -- ^ Rank of black player.
  | BT SimpleText        -- ^ Name of black team.
  | CP SimpleText        -- ^ Copyright info.
  | DT SimpleText        -- ^ Dates played.
  | EV SimpleText        -- ^ Event name.
  | GC Text              -- ^ Game comment, or background, or summary.
  | GN SimpleText        -- ^ Game name.
  | ON SimpleText        -- ^ Information about the opening.
  | OT SimpleText        -- ^ The method used for overtime.
  | PB SimpleText        -- ^ Name of black player.
  | PC SimpleText        -- ^ Where the game was played.
  | PW SimpleText        -- ^ Name of white player.
  | RE GameResult        -- ^ Result of the game.
  | RO SimpleText        -- ^ Round info.
  | RU Ruleset           -- ^ Ruleset used.
  | SO SimpleText        -- ^ Source of the game.
  | TM RealValue         -- ^ Time limit, in seconds.
  | US SimpleText        -- ^ Name of user or program who entered the game.
  | WR SimpleText        -- ^ Rank of white player.
  | WT SimpleText        -- ^ Name of white team.

  -- Timing properties.
  | BL RealValue         -- ^ Black time left.
  | OB Int               -- ^ Black moves left in byo-yomi period.
  | OW Int               -- ^ White moves left in byo-yomi period.
  | WL RealValue         -- ^ White time left.

  -- Miscellaneous properties.
  -- TODO FG property.
  -- TODO PM property.
  | VW CoordList         -- ^ Set viewing region.

  -- Go-specific properties.
  | HA Int               -- ^ Handicap stones (>=2).
  | KM RealValue         -- ^ Komi.
  | TB CoordList         -- ^ Black territory.
  | TW CoordList         -- ^ White territory.

  | UnknownProperty String UnknownPropertyValue

  -- TODO Game info, timing, and miscellaneous properties.
  -- Also in functions below.
  deriving (Eq, Show)

-- | The property types that SGF uses to group properties.
data PropertyType = MoveProperty     -- ^ Cannot mix with setup nodes.
                  | SetupProperty    -- ^ Cannot mix with move nodes.
                  | RootProperty     -- ^ May only appear in root nodes.
                  | GameInfoProperty -- ^ At most one on any path.
                  | GeneralProperty  -- ^ May appear anywhere in the game tree.
                  deriving (Eq, Show)

-- | A class for types that contain metadata about a 'Property'.
class Descriptor a where
  -- | Returns the name of the property, as used in SGF files.
  propertyName :: a -> String

  -- | Returns the type of the property, as specified by the SGF spec.
  propertyType :: a -> PropertyType

  -- | Returns whether the value of the given property is inherited from the
  -- lowest ancestor specifying the property, when the property is not set on a
  -- node itself.
  propertyInherited :: a -> Bool

  -- | Returns whether the given property has the type of a descriptor.
  propertyPredicate :: a -> Property -> Bool

  -- | A parser of property values in SGF format (e.g. @"[ab]"@ for a property
  -- that takes a point).
  propertyValueParser :: a -> Parser Property

  -- | A renderer property values to SGF format (e.g. @B (Just (1,2))@ renders
  -- to @"[ab]"@).
  propertyValueRenderer :: a -> Property -> Render ()

  -- | A renderer for displaying property values in a UI.  Displays the value in
  -- a human-readable format.
  propertyValueRendererPretty :: a -> Property -> Render ()

data SomeDescriptor = forall a. Descriptor a => SomeDescriptor a

instance Descriptor SomeDescriptor where
  propertyName (SomeDescriptor d) = propertyName d
  propertyType (SomeDescriptor d) = propertyType d
  propertyInherited (SomeDescriptor d) = propertyInherited d
  propertyPredicate (SomeDescriptor d) = propertyPredicate d
  propertyValueParser (SomeDescriptor d) = propertyValueParser d
  propertyValueRenderer (SomeDescriptor d) = propertyValueRenderer d
  propertyValueRendererPretty (SomeDescriptor d) = propertyValueRendererPretty d

-- | A class for 'Descriptor's of 'Property's that also contain values.
class (Descriptor a, Eq v) => ValuedDescriptor a v | a -> v where
  -- | Extracts the value from a property of the given type.  Behaviour is
  -- undefined if the property is not of the given type.
  propertyValue :: a -> Property -> v

  -- | Builds a property from a given value.
  propertyBuilder :: a -> v -> Property

-- | Metadata for a property that does not contain a value.  Corresponds to a
-- single nullary data constructor of 'Property'.
data PropertyInfo = PropertyInfo
  { propertyInfoName :: String
    -- ^ The SGF textual name for the property.
  , propertyInfoInstance :: Property
    -- ^ The single instance of the property.
  , propertyInfoType :: PropertyType
    -- ^ The SGF property type.
  , propertyInfoInherited :: Bool
    -- ^ Whether the property is inherited.
  }

instance Descriptor PropertyInfo where
  propertyName = propertyInfoName
  propertyType = propertyInfoType
  propertyInherited = propertyInfoInherited
  propertyPredicate = (==) . propertyInfoInstance
  propertyValueParser descriptor = propertyInfoInstance descriptor <$ pvtParser nonePvt
  propertyValueRenderer _ _ = pvtRenderer nonePvt ()
  propertyValueRendererPretty _ _ = pvtRendererPretty nonePvt ()

-- | Metadata for a property that contains a value.  Corresponds to a single
-- unary data constructor of 'Property'.
data ValuedPropertyInfo v = ValuedPropertyInfo
  { valuedPropertyInfoName :: String
    -- ^ The SGF textual name for the property (also the name of the data
    -- constructor).
  , valuedPropertyInfoType :: PropertyType
    -- ^ The SGF property type.
  , valuedPropertyInfoInherited :: Bool
    -- ^ Whether the property is inherited.
  , valuedPropertyInfoPredicate :: Property -> Bool
    -- ^ A predicate that matches predicates to which this 'ValuedPropertyInfo'
    -- applies.
  , valuedPropertyInfoValueType :: PropertyValueType v
    -- ^ Metadata about the type of the property's value.
  , valuedPropertyInfoValue :: Property -> v
    -- ^ A function that extracts values from properties to which this
    -- 'ValuedPropertyInfo' applies.  It is invalid to call this function with a
    -- different type of property.
  , valuedPropertyInfoBuilder :: v -> Property
    -- ^ A function that builds a property containing a value.
  }

instance Descriptor (ValuedPropertyInfo v) where
  propertyName = valuedPropertyInfoName
  propertyType = valuedPropertyInfoType
  propertyInherited = valuedPropertyInfoInherited
  propertyPredicate = valuedPropertyInfoPredicate
  propertyValueParser descriptor =
    fmap (valuedPropertyInfoBuilder descriptor) $
    pvtParser $
    valuedPropertyInfoValueType descriptor
  propertyValueRenderer descriptor property =
    pvtRenderer (valuedPropertyInfoValueType descriptor) $
    valuedPropertyInfoValue descriptor property
  propertyValueRendererPretty descriptor property =
    pvtRendererPretty (valuedPropertyInfoValueType descriptor) $
    valuedPropertyInfoValue descriptor property

instance Eq v => ValuedDescriptor (ValuedPropertyInfo v) v where
  propertyValue = valuedPropertyInfoValue
  propertyBuilder = valuedPropertyInfoBuilder

-- | Template Haskell function to declare a property that does not contain a
-- value.
--
-- > $(defProperty "KO" 'MoveProperty False)
--
-- This example declares a @propertyKO :: 'PropertyInfo'@ that is a
-- 'MoveProperty' and is not inherited.
defProperty :: String
               -- ^ The SGF textual name of the property.
            -> Name
               -- ^ The name of the 'PropertyType'.
            -> Bool
               -- ^ Whether the property is inherited.
            -> DecsQ
defProperty name propType inherited = do
  let propName = mkName name
      varName = mkName $ "property" ++ name
  sequence [
    sigD varName $ conT $ mkName "PropertyInfo",
    valD (varP varName)
         (normalB [| PropertyInfo name $(conE propName) $(conE propType) inherited |])
         []
    ]

-- | Template Haskell function to declare a property that contains a value.
--
-- > $(defValuedProperty "B" 'MoveProperty False 'maybeCoordPrinter)
--
-- This example declares a @propertyB :: 'ValuedPropertyInfo' (Maybe 'Coord')@
-- that is a 'MoveProperty' and is not inherited.  The value type is
-- automatically inferred.
defValuedProperty :: String -> Name -> Bool -> Name -> DecsQ
defValuedProperty name propType inherited valueType = do
  let propName = mkName name
      varName = mkName $ "property" ++ name
  foo <- newName "foo"
  bar <- newName "bar"
  DataConI _ (AppT (AppT _ haskellValueType) _) _ _ <- reify propName
  sequence [
    sigD varName $ appT (conT ''ValuedPropertyInfo) $ return haskellValueType,
    valD (varP varName)
         (normalB [| ValuedPropertyInfo
                     name
                     $(conE propType)
                     inherited
                     $(lam1E (varP foo) $ caseE (varE foo)
                       [match (recP propName [])
                              (normalB $ conE $ mkName "True")
                              [],
                        match wildP (normalB $ conE $ mkName "False") []])
                     $(varE valueType)
                     $(lam1E (varP foo) $ caseE (varE foo)
                       [match (conP propName [varP bar]) (normalB $ varE bar) [],
                        match wildP
                              (normalB
                               [| error $ "Property value getter for " ++ $(stringE name) ++
                                  " applied to " ++ show $(varE foo) ++ "." |])
                              []])
                     $(lam1E (varP foo) $ appE (conE propName) (varE foo))
                   |])
         []
    ]
