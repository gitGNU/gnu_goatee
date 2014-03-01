{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Core property-related data types, and some Template Haskell declarations
-- for defining property metadata.
--
-- Import "Khumba.Goatee.Sgf.Property" rather than importing this module.
module Khumba.Goatee.Sgf.Property.Base (
  -- * Properties
  Property(..),
  -- * Property metadata
  PropertyType(..),
  Descriptor(..), ValuedDescriptor(..),
  PropertyInfo, ValuedPropertyInfo(valuedPropertyInfoBase),
  -- * Property declaration
  makePropertyInfo, makeValuedPropertyInfo,
  defProperty, defValuedProperty
  ) where

import Control.Monad
import Khumba.Goatee.Sgf.Types
import Language.Haskell.TH

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
  | GC SimpleText        -- ^ Game comment/background/summary.
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

  -- Miscellaneous properties.
  | VW CoordList         -- ^ Set viewing region.

  | UnknownProperty String String

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
  -- | Returns whether the given property has the type of an info.
  propertyPredicate :: a -> Property -> Bool

-- | A class for 'Descriptor's of 'Property's that also contain values.
class Descriptor a => ValuedDescriptor a v | a -> v where
  -- | Extracts the value from a property of the given type.  Behaviour is
  -- undefined if the property is not of the given type.
  propertyGetValue :: a -> Property -> v

-- | Metadata for a property that does not contain a value.
data PropertyInfo = PropertyInfo {
  propertyInfoName :: String
  , propertyInfoType :: PropertyType
  , propertyInfoInherited :: Bool
  , propertyInfoPredicate :: Property -> Bool
  }

instance Descriptor PropertyInfo where
  propertyName = propertyInfoName
  propertyType = propertyInfoType
  propertyInherited = propertyInfoInherited
  propertyPredicate = propertyInfoPredicate

makePropertyInfo :: String
                    -- ^ The SGF textual name for the property.
                 -> PropertyType
                    -- ^ The SGF property type.
                 -> Bool
                    -- ^ Whether the property is inherited.
                 -> (Property -> Bool)
                    -- ^ A predicate that matchss properties with the name given
                    -- previously.
                 -> PropertyInfo
makePropertyInfo = PropertyInfo

-- | Metadata for a property that contains a value.
data ValuedPropertyInfo v = ValuedPropertyInfo {
  valuedPropertyInfoBase :: PropertyInfo
  , valuedPropertyInfoGetValue :: Property -> v
  }

makeValuedPropertyInfo :: String
                          -- ^ The SGF textual name for the property.
                       -> PropertyType
                          -- ^ The SGF property type.
                       -> Bool
                          -- ^ Whether the property is inherited.
                       -> (Property -> Bool)
                          -- ^ A predicate that matches properties with the name
                          -- given previously.
                       -> (Property -> v)
                          -- ^ A function that extracts values from properties
                          -- with the name given previously.  No need to handle
                          -- other types of properties.
                       -> ValuedPropertyInfo v
makeValuedPropertyInfo name propType inherited predicate getter =
  ValuedPropertyInfo { valuedPropertyInfoBase = makePropertyInfo name propType inherited predicate
                     , valuedPropertyInfoGetValue = getter
                     }

instance Descriptor (ValuedPropertyInfo v) where
  propertyName = propertyName . valuedPropertyInfoBase
  propertyType = propertyType . valuedPropertyInfoBase
  propertyInherited = propertyInherited . valuedPropertyInfoBase
  propertyPredicate = propertyPredicate . valuedPropertyInfoBase

instance ValuedDescriptor (ValuedPropertyInfo v) v where
  propertyGetValue = valuedPropertyInfoGetValue

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
  foo <- newName "foo"
  liftM (:[]) $ valD
    (varP $ mkName $ "property" ++ name)
    (normalB [| makePropertyInfo name $(conE propType) inherited
                $(lam1E (varP foo) $ caseE (varE foo)
                  [match (recP propName [])
                   (normalB $ conE $ mkName "True")
                   [],
                   match wildP (normalB $ conE $ mkName "False") []])
              |])
    []

-- | Template Haskell function to declare a property that contains a value.
--
-- > $(defValuedProperty "B" 'MoveProperty False)
--
-- This example declares a @propertyB :: 'ValuedPropertyInfo' (Maybe 'Coord')@
-- that is a 'MoveProperty' and is not inherited.  The value type is
-- automatically inferred.
defValuedProperty :: String -> Name -> Bool -> DecsQ
defValuedProperty name propType inherited = do
  let propName = mkName name
  foo <- newName "foo"
  bar <- newName "bar"
  liftM (:[]) $ valD
    (varP $ mkName $ "property" ++ name)
    (normalB [| makeValuedPropertyInfo name $(conE propType) inherited
                $(lam1E (varP foo) $ caseE (varE foo)
                  [match (recP propName [])
                         (normalB $ conE $ mkName "True")
                         [],
                   match wildP (normalB $ conE $ mkName "False") []])
                $(lam1E (varP foo) $ caseE (varE foo)
                  [match (conP propName [varP bar]) (normalB $ varE bar) [],
                   match wildP
                         (normalB
                          [| error $ "Property value getter for " ++ $(stringE name) ++
                             " applied to " ++ show $(varE foo) ++ "." |])
                         []])
              |])
    []
