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

-- | Data types for property values used in SGF game trees.
module Khumba.Goatee.Sgf.Types (
  Coord, CoordList(..), coords, coords',
  emptyCoordList, expandCoordList,
  RealValue,
  Stringlike(..),
  Text(fromText), toText,
  SimpleText(fromSimpleText), toSimpleText,
  DoubleValue(Double1, Double2),
  Color(Black, White), cnot,
  VariationMode(..), VariationModeSource(..), defaultVariationMode,
  toVariationMode, fromVariationMode,
  ArrowList, LineList, LabelList, Mark(..),
  GameResult(..), fromGameResult,
  WinReason(..),
  Ruleset(..), RulesetType(..), fromRuleset, toRuleset
  ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sort)
import Data.Monoid

-- | A coordinate on a Go board.  @(0, 0)@ refers to the upper-left corner of
-- the board.  The first component is the horizontal position; the second
-- component is the vertical position.
type Coord = (Int, Int)

-- | Allows for compact representation of a list of coordinates.  Convert to a
-- list with 'expandCoordList'.
data CoordList = CoordList { coordListSingles :: [Coord]
                           , coordListRects :: [(Coord, Coord)]
                           } deriving (Show)

instance Eq CoordList where
  (==) = (==) `on` sort . expandCoordList

instance Monoid CoordList where
  mempty = CoordList { coordListSingles = []
                     , coordListRects = []
                     }

  mappend x y = CoordList { coordListSingles = coordListSingles x ++ coordListSingles y
                          , coordListRects = coordListRects x ++ coordListRects y
                          }

coords :: [Coord] -> CoordList
coords singles = CoordList { coordListSingles = singles
                           , coordListRects = []
                           }

coords' :: [Coord] -> [(Coord, Coord)] -> CoordList
coords' singles rects = CoordList { coordListSingles = singles
                                  , coordListRects = rects
                                  }

emptyCoordList :: CoordList
emptyCoordList = CoordList { coordListSingles = []
                           , coordListRects = []
                           }

-- | Converts a compact 'CoordList' to a list of coordinates.
expandCoordList :: CoordList -> [Coord]
expandCoordList cl = coordListSingles cl ++
                     foldr (\r@((x0, y0), (x1, y1)) rest ->
                             if x0 > x1 || y0 > y1
                             then error ("Invalid coord. rectangle: " ++ show r)
                             else [(x, y) | x <- [x0..x1], y <- [y0..y1]] ++ rest)
                           []
                           (coordListRects cl)

-- | An SGF real value.
type RealValue = Rational

-- | A class for SGF data types that are coercable to and from strings.
--
-- The construction of an SGF value with 'stringToSgf' may process the input,
-- such that the resulting stringlike value does not represent the same string
-- as the input.  In other words, the following does *not* necessarily hold:
--
-- > sgfToString . stringToSgf = id
--
-- The following does hold, however:
--
-- > stringToSgf . sgfToString = id
class Stringlike a where
  -- | Extracts the string value from an SGF value.
  sgfToString :: a -> String

  -- | Creates an SGF value from a string value.
  stringToSgf :: String -> a

-- | An SGF text value.
newtype Text = Text { fromText :: String }
             deriving (Eq, Show)

instance Stringlike Text where
  sgfToString = fromText
  stringToSgf = toText

toText :: String -> Text
toText = Text

-- | An SGF SimpleText value.
newtype SimpleText = SimpleText { fromSimpleText :: String }
                   deriving (Eq, Show)

instance Stringlike SimpleText where
  sgfToString = fromSimpleText
  stringToSgf = toSimpleText

sanitizeSimpleText :: String -> String
sanitizeSimpleText = map (\c -> if isSpace c then ' ' else c)

-- | Converts a string to an SGF 'SimpleText', replacing all whitespaces
-- (including newlines) with spaces.
toSimpleText :: String -> SimpleText
toSimpleText = SimpleText . sanitizeSimpleText

-- | An SGF double value: either 1 or 2, nothing else.
data DoubleValue = Double1
                 | Double2
                 deriving (Eq, Show)

-- | Stone color: black or white.
data Color = Black
           | White
           deriving (Eq, Show)

-- | Returns the logical negation of a stone color, yang for yin and
-- yin for yang.
cnot :: Color -> Color
cnot Black = White
cnot White = Black

data VariationMode = VariationMode { variationModeSource :: VariationModeSource
                                   , variationModeBoardOverlay :: Bool
                                   } deriving (Eq, Show)

data VariationModeSource = ShowChildVariations
                         | ShowCurrentVariations
                         deriving (Eq, Show)

defaultVariationMode :: VariationMode
defaultVariationMode = VariationMode ShowChildVariations True

toVariationMode :: Int -> Maybe VariationMode
toVariationMode n = case n of
  0 -> Just $ VariationMode ShowChildVariations True
  1 -> Just $ VariationMode ShowCurrentVariations True
  2 -> Just $ VariationMode ShowChildVariations False
  3 -> Just $ VariationMode ShowCurrentVariations False
  _ -> Nothing

fromVariationMode :: VariationMode -> Int
fromVariationMode mode = case mode of
  VariationMode ShowChildVariations True -> 0
  VariationMode ShowCurrentVariations True -> 1
  VariationMode ShowChildVariations False -> 2
  VariationMode ShowCurrentVariations False -> 3

-- | A list of arrows, each specified as @(startCoord, endCoord)@.
type ArrowList = [(Coord, Coord)]

-- | A list of lines, each specified as @(startCoord, endCoord)@.
type LineList = [(Coord, Coord)]

-- | A list of labels, each specified with a string and a coordinate about which
-- to center the string.
type LabelList = [(Coord, SimpleText)]

-- | The markings that SGF supports annotating coordinates with.
data Mark = MarkCircle | MarkSquare | MarkTriangle | MarkX | MarkSelected
          deriving (Eq, Show)

data GameResult = GameResultWin Color WinReason
                | GameResultDraw
                | GameResultVoid
                | GameResultUnknown
                | GameResultOther String
                deriving (Eq, Show)

fromGameResult :: GameResult -> String
fromGameResult (GameResultWin color reason) =
  (case color of { Black -> 'B'; White -> 'W' }) : '+' :
  (case reason of
      WinByScore diff -> show diff
      WinByResignation -> "R"
      WinByTime -> "T"
      WinByForfeit -> "F")
fromGameResult GameResultDraw = "0"
fromGameResult GameResultVoid = "Void"
fromGameResult GameResultUnknown = "?"
fromGameResult (GameResultOther string) = string

data WinReason = WinByScore RealValue
               | WinByResignation
               | WinByTime
               | WinByForfeit
               deriving (Eq, Show)

-- | A ruleset used for a Go game.  Can be one of the rulesets defined by the
-- SGF specification, or a custom string.
data Ruleset = KnownRuleset RulesetType
             | UnknownRuleset String
             deriving (Eq, Show)

-- | The rulesets defined by the SGF specification, for use with 'Ruleset'.
data RulesetType = RulesetAga
                 | RulesetIng
                 | RulesetJapanese
                 | RulesetNewZealand
                 deriving (Bounded, Enum, Eq, Show)

-- | Returns the string representation for a ruleset.
fromRuleset :: Ruleset -> String
fromRuleset ruleset = case ruleset of
  KnownRuleset RulesetAga -> "AGA"
  KnownRuleset RulesetIng -> "Goe"
  KnownRuleset RulesetJapanese -> "Japanese"
  KnownRuleset RulesetNewZealand -> "NZ"
  UnknownRuleset str -> str

-- | Parses a string representation of a ruleset.
toRuleset :: String -> Ruleset
toRuleset str = case str of
  "AGA" -> KnownRuleset RulesetAga
  "Goe" -> KnownRuleset RulesetIng
  "Japanese" -> KnownRuleset RulesetJapanese
  "NZ" -> KnownRuleset RulesetNewZealand
  _ -> UnknownRuleset str
