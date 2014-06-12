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
-- Import "Khumba.Goatee.Sgf.Property" rather than importing this module.
module Khumba.Goatee.Sgf.Property.Base (
  -- * Properties
  Property(..),
  -- * Property metadata
  PropertyType(..),
  Descriptor(..), ValuedDescriptor(..),
  PropertyInfo,
  ValuedPropertyInfo(
    valuedPropertyInfoBase,
    valuedPropertyInfoValue,
    valuedPropertyInfoBuilder),
  -- * Property declaration
  makePropertyInfo, makeValuedPropertyInfo,
  defProperty, defValuedProperty,
  -- * Property value renderers
  noValuePrinter,
  stringlikePrinter,
  printStringlike,
  maybeCoordPrinter,
  coordListPrinter,
  coordElistPrinter,
  coordPairListPrinter,
  labelListPrinter,
  colorPrinter,
  doublePrinter,
  numberPrinter,
  realPrinter,
  variationModePrinter,
  gameResultPrinter,
  rulesetPrinter,
  unknownPropertyPrinter,
  ) where

import Data.Char (chr, ord)
import Khumba.Goatee.Sgf.Types
import Language.Haskell.TH (
  Info (DataConI), DecsQ, Name, Type (AppT),
  appE, appT, caseE, conE, conP, conT, lam1E, match, mkName, newName,
  normalB, recP, reify, sigD, stringE, valD, varE, varP, wildP,
  )

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
  -- | Renders a property value to the SGF textual format.  Should include
  -- enclosing [brackets] (for multiple values if necessary).
  propertyValuePrinter :: a -> Property -> String

-- | A class for 'Descriptor's of 'Property's that also contain values.
class (Descriptor a, Eq v) => ValuedDescriptor a v | a -> v where
  -- | Extracts the value from a property of the given type.  Behaviour is
  -- undefined if the property is not of the given type.
  propertyValue :: a -> Property -> v
  -- | Builds a property from a given value.
  propertyBuilder :: a -> v -> Property

-- | Metadata for a property that does not contain a value.
data PropertyInfo = PropertyInfo {
  propertyInfoName :: String
  , propertyInfoType :: PropertyType
  , propertyInfoInherited :: Bool
  , propertyInfoPredicate :: Property -> Bool
  , propertyInfoValuePrinter :: Property -> String
  }

instance Descriptor PropertyInfo where
  propertyName = propertyInfoName
  propertyType = propertyInfoType
  propertyInherited = propertyInfoInherited
  propertyPredicate = propertyInfoPredicate
  propertyValuePrinter = propertyInfoValuePrinter

makePropertyInfo :: String
                    -- ^ The SGF textual name for the property.
                 -> PropertyType
                    -- ^ The SGF property type.
                 -> Bool
                    -- ^ Whether the property is inherited.
                 -> (Property -> Bool)
                    -- ^ A predicate that matchss properties with the name given
                    -- previously.
                 -> (Property -> String)
                    -- ^ A function that serializes a property value to the SGF
                    -- textual format.  Should include enclosing [brackets].
                 -> PropertyInfo
makePropertyInfo = PropertyInfo

-- | Metadata for a property that contains a value.
data ValuedPropertyInfo v = ValuedPropertyInfo {
  valuedPropertyInfoBase :: PropertyInfo
  , valuedPropertyInfoValue :: Property -> v
  , valuedPropertyInfoBuilder :: v -> Property
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
                       -> (v -> String)
                          -- ^ A function that serializes a property value to
                          -- the SGF textual format.  Should include enclosing
                          -- [brackets].
                       -> (Property -> v)
                          -- ^ A function that extracts values from properties
                          -- with the name given previously.  No need to handle
                          -- other types of properties.
                       -> (v -> Property)
                          -- ^ A function that builds a property containing a
                          -- value.
                       -> ValuedPropertyInfo v
makeValuedPropertyInfo name propType inherited predicate valuePrinter getter builder =
  ValuedPropertyInfo { valuedPropertyInfoBase =
                          makePropertyInfo name propType inherited predicate (valuePrinter . getter)
                     , valuedPropertyInfoValue = getter
                     , valuedPropertyInfoBuilder = builder
                     }

instance Descriptor (ValuedPropertyInfo v) where
  propertyName = propertyName . valuedPropertyInfoBase
  propertyType = propertyType . valuedPropertyInfoBase
  propertyInherited = propertyInherited . valuedPropertyInfoBase
  propertyPredicate = propertyPredicate . valuedPropertyInfoBase
  propertyValuePrinter = propertyValuePrinter . valuedPropertyInfoBase

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
  foo <- newName "foo"
  sequence [
    sigD varName $ conT $ mkName "PropertyInfo",
    valD (varP varName)
         (normalB [| makePropertyInfo name $(conE propType) inherited
                     $(lam1E (varP foo) $ caseE (varE foo)
                       [match (recP propName [])
                        (normalB $ conE $ mkName "True")
                        [],
                        match wildP (normalB $ conE $ mkName "False") []])
                     noValuePrinter
                   |])
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
defValuedProperty name propType inherited valuePrinter = do
  let propName = mkName name
      varName = mkName $ "property" ++ name
  foo <- newName "foo"
  bar <- newName "bar"
  DataConI _ (AppT (AppT _ valueType) _) _ _ <- reify propName
  sequence [
    sigD varName $ appT (conT ''ValuedPropertyInfo) $ return valueType,
    valD (varP varName)
         (normalB [| makeValuedPropertyInfo name $(conE propType) inherited
                     $(lam1E (varP foo) $ caseE (varE foo)
                       [match (recP propName [])
                              (normalB $ conE $ mkName "True")
                              [],
                        match wildP (normalB $ conE $ mkName "False") []])
                     $(varE valuePrinter)
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

-- A note about the naming of the functions here.  'printFoo' prints a foo with
-- no decoration, whereas 'fooPrinter' prints a foo with '[]' around it.

noValuePrinter :: Property -> String
noValuePrinter = const "[]"

printer :: String -> String
printer x = '[' : x ++ "]"

showPrinter :: Show a => a -> String
showPrinter = printer . show

stringlikePrinter :: Stringlike a => a -> String
stringlikePrinter = printer . printStringlike False

printStringlike :: Stringlike a => Bool -> a -> String
printStringlike isComposed str = escape $ sgfToString str
  where escape [] = []
        escape (first:rest) | first `elem` specialChars = '\\':first:escape rest
                            | otherwise = first:escape rest
        -- TODO Deduplicate these characters with the parser:
        specialChars = if isComposed then ":]\\" else "]\\"

maybeCoordPrinter :: Maybe Coord -> String
maybeCoordPrinter = maybe "[]" coordPrinter

coordPrinter :: Coord -> String
coordPrinter = printer . printCoord

coordListPrinter :: CoordList -> String
coordListPrinter coords =
  if null $ expandCoordList coords
  then error "coordListPrinter: Unexpected empty coordinate list."
  else coordListPrinter' coords

coordElistPrinter :: CoordList -> String
coordElistPrinter coords =
  if null $ expandCoordList coords
  then "[]"
  else coordListPrinter' coords

coordListPrinter' :: CoordList -> String
coordListPrinter' coords =
  concat $
  map coordPairPrinter (coordListRects coords) ++
  map coordPrinter (coordListSingles coords)

coordPairListPrinter :: [(Coord, Coord)] -> String
coordPairListPrinter = concatMap coordPairPrinter

labelListPrinter :: [(Coord, SimpleText)] -> String
labelListPrinter = concatMap $ \((x, y), text) ->
  '[' : printLine x : printLine y : ':' : printStringlike True text ++ "]"

printCoord :: Coord -> String
printCoord (x, y) = [printLine x, printLine y]

coordPairPrinter :: (Coord, Coord) -> String
coordPairPrinter ((x0,y0), (x1,y1)) =
  ['[', printLine x0, printLine y0, ':', printLine x1, printLine y1, ']']

printLine :: Int -> Char
printLine x = if x < 52
              then chr $ x + (if x < 26 then ord 'a' else ord 'A' - 26)
              else error $ "printLine: Index too big: " ++ show x

colorPrinter :: Color -> String
colorPrinter Black = "[B]"
colorPrinter White = "[W]"

doublePrinter :: DoubleValue -> String
doublePrinter Double1 = "[1]"
doublePrinter Double2 = "[2]"

numberPrinter :: (Num a, Show a) => a -> String
numberPrinter = showPrinter

realPrinter :: RealValue -> String
realPrinter = showPrinter . fromRational

variationModePrinter :: VariationMode -> String
variationModePrinter = printer . show . fromVariationMode

gameResultPrinter :: GameResult -> String
gameResultPrinter = printer . fromGameResult

rulesetPrinter :: Ruleset -> String
rulesetPrinter = printer . fromRuleset

unknownPropertyPrinter :: Property -> String
unknownPropertyPrinter (UnknownProperty _ value) = value
unknownPropertyPrinter property =
  error $ "unknownPropertyPrinter: Not an unknown property: " ++ show property
