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
  supportedFormatVersions, defaultFormatVersion, supportedGameTypes, maxBoardSize,
  Coord, CoordList(coordListSingles, coordListRects), coord1, coords, coords',
  emptyCoordList, expandCoordList, buildCoordList,
  RealValue,
  Stringlike(..),
  Text(fromText), toText,
  SimpleText(fromSimpleText), toSimpleText,
  UnknownPropertyValue(fromUnknownPropertyValue), toUnknownPropertyValue,
  DoubleValue(Double1, Double2),
  Color(Black, White), cnot,
  VariationMode(..), VariationModeSource(..), defaultVariationMode,
  toVariationMode, fromVariationMode,
  ArrowList, LineList, LabelList, Mark(..),
  GameResult(..),
  WinReason(..),
  Ruleset(..), RulesetType(..), fromRuleset, toRuleset,
  ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (delete, groupBy, partition, sort)

-- TODO Support FF versions 1-4.
supportedFormatVersions :: [Int]
supportedFormatVersions = [4]

-- | The default SGF version to use when @FF[]@ is not specified in a root node.
--
-- This value is actually INCORRECT: SGF defines it to be 1, but because we
-- don't support version 1 yet, for the sake of ignoring this issue (for now!)
-- in tests, we fix the default to be 4.
--
-- TODO Fix the default version to be 1 as SGF mandates.
defaultFormatVersion :: Int
defaultFormatVersion = 4

supportedGameTypes :: [Int]
supportedGameTypes = [1 {- Go -}]

-- | The maximum board size allowed by FF[4].
maxBoardSize :: Int
maxBoardSize = 52

-- | A coordinate on a Go board.  @(0, 0)@ refers to the upper-left corner of
-- the board.  The first component is the horizontal position; the second
-- component is the vertical position.
type Coord = (Int, Int)

-- | A structure for compact representation of a list of coordinates.  Contains
-- a list of individual points, as well as a list of rectangles of points
-- denoted by an ordered pair of the upper-left point and the lower-right point.
-- The union of the single points and points contained within rectangles make up
-- all of the points a @CoordList@ represents.  There is no rule saying that
-- adjacent points have to be grouped into rectangles; it's perfectly valid
-- (although possibly inefficient) to never use rectangles.
--
-- For any @CoordList@, all of the following hold:
--
-- 1. Any point may be specified at most once, either in the singles list or in
-- a single rectangle.
--
-- 2. For a rectangle @((x0,y0), (x1,y1))@, @x0 <= x1@ and @y0 <= y1@ and
-- @(x0,y0) /= (x1,y1)@ (otherwise the point belongs in the singles list).
data CoordList = CoordList { coordListSingles :: [Coord]
                           , coordListRects :: [(Coord, Coord)]
                           } deriving (Show)

-- | Equality is based on unordered, set equality of the underlying points.
instance Eq CoordList where
  (==) = (==) `on` sort . expandCoordList

-- | Constructs a 'CoordList' containing a single point.
coord1 :: Coord -> CoordList
coord1 xy = CoordList { coordListSingles = [xy]
                      , coordListRects = []
                      }

-- | Constructs a 'CoordList' containing the given single points.  For rectangle
-- detection, use 'buildCoordList'.
coords :: [Coord] -> CoordList
coords singles = CoordList { coordListSingles = singles
                           , coordListRects = []
                           }

-- | Constructs a 'CoordList' containing the given single points and rectangles.
coords' :: [Coord] -> [(Coord, Coord)] -> CoordList
coords' singles rects = CoordList { coordListSingles = singles
                                  , coordListRects = rects
                                  }

-- | A 'CoordList' that contains no points.
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

-- | Constructs a 'CoordList' from a list of 'Coord's, doing some
-- not-completely-stupid rectangle detection.  The order of data in the result
-- is unspecified.
buildCoordList :: [Coord] -> CoordList
-- This algorithm doesn't generate the smallest result.  For the following
-- input:
--
-- F F T T
-- T T T T
-- T T F F
--
-- It will return [ca:da][ab:db][ac:bc] rather than the shorter [ca:db][ab:bc].
buildCoordList = toCoordList . generateRects 0 [] . buildTruePairs . toGrid
  where -- | Constructs a row-major grid of booleans where an coordinate is true
        -- iff it is in the given list.
        toGrid :: [Coord] -> [[Bool]]
        toGrid [] = []
        toGrid coords = let x1 = maximum $ map fst coords
                            y1 = maximum $ map snd coords
                        in [[(x,y) `elem` coords | x <- [0..x1]] | y <- [0..y1]]

        -- | For each row, converts a list of booleans into a list of pairs
        -- where each pair represents a consecutive run of true values.  The
        -- pair indicates the indices of the first and last boolean in each run.
        buildTruePairs :: [[Bool]] -> [[(Int, Int)]]
        buildTruePairs = map $
                         concatMap extractTrueGroups .
                         groupBy ((==) `on` snd) .
                         zip [0..]

        -- | Given a run of indexed booleans with the same boolean value within
        -- a row, returns @[(startIndex, endIndex)]@ if the value is true, and
        -- @[]@ if the value is false.
        extractTrueGroups :: [(Int, Bool)] -> [(Int, Int)]
        extractTrueGroups list@((start, True):_) = [(start, fst (last list))]
        extractTrueGroups _ = []

        -- | Converts the lists of true pairs for all rows into a list of
        -- @(Coord, Coord)@ rectangles.  We repeatedly grab the first span in
        -- the first row, and see how many leading rows contain that exact span.
        -- Then we build a (maybe multi-row) rectangle for the span, remove the
        -- span from all leading rows, and repeat.  When the first row becomes
        -- empty, we drop it and increment a counter that keeps track of our
        -- first row's y-position.
        generateRects :: Int -> [(Coord, Coord)] -> [[(Int, Int)]] -> [(Coord, Coord)]
        generateRects _ acc [] = acc
        generateRects topRowOffset acc ([]:rows) = generateRects (topRowOffset + 1) acc rows
        generateRects topRowOffset acc rows@((span:_):_) =
          let rowsWithSpan = matchRowsWithSpan span rows
              rowsWithSpanCount = length rowsWithSpan
          in generateRects topRowOffset
                           (((fst span, topRowOffset),
                             (snd span, topRowOffset + rowsWithSpanCount - 1)) : acc)
                           (rowsWithSpan ++ drop rowsWithSpanCount rows)

        -- | Determines how many leading rows contain the given span.  A list of
        -- all the matching rows is returned, with the span deleted from each.
        matchRowsWithSpan :: (Int, Int) -> [[(Int, Int)]] -> [[(Int, Int)]]
        matchRowsWithSpan span (row:rows)
          | span `elem` row = delete span row : matchRowsWithSpan span rows
          | otherwise = []
        matchRowsWithSpan _ [] = []

        -- | Builds a 'CoordList' from simple @(Coord, Coord)@ rectangles.
        toCoordList :: [(Coord, Coord)] -> CoordList
        toCoordList rects =
          let (singles, properRects) = partition (uncurry (==)) rects
          in coords' (map fst singles) properRects

-- | An SGF real value.
type RealValue = Rational

-- | A class for SGF data types that are coercable to and from strings.
--
-- The construction of an SGF value with 'stringToSgf' may process the input,
-- such that the resulting stringlike value does not represent the same string
-- as the input.  In other words, the following does *not* necessarily hold:
--
-- > sgfToString . stringToSgf = id   (does not necessarily hold!)
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

data UnknownPropertyValue = UnknownPropertyValue { fromUnknownPropertyValue :: String }
                            deriving (Eq, Show)

instance Stringlike UnknownPropertyValue where
  sgfToString = fromUnknownPropertyValue
  stringToSgf = toUnknownPropertyValue

toUnknownPropertyValue :: String -> UnknownPropertyValue
toUnknownPropertyValue = UnknownPropertyValue

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
                                   , variationModeBoardMarkup :: Bool
                                   } deriving (Eq, Show)

data VariationModeSource = ShowChildVariations
                         | ShowCurrentVariations
                         deriving (Bounded, Enum, Eq, Show)

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
          deriving (Bounded, Enum, Eq, Show)

data GameResult = GameResultWin Color WinReason
                | GameResultDraw
                | GameResultVoid
                | GameResultUnknown
                | GameResultOther SimpleText
                deriving (Eq, Show)

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
