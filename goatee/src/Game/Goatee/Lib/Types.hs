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

-- | Constants and data types for property values used in SGF game trees.
module Game.Goatee.Lib.Types (
  -- * Constants
  supportedFormatVersions, defaultFormatVersion, supportedGameTypes,
  boardSizeDefault, boardSizeMin, boardSizeMax,
  -- * Board coordinates
  Coord, CoordList, coordListSingles, coordListRects, coord1, coords, coords',
  emptyCoordList, expandCoordList, buildCoordList,
  -- ** Star points and handicap stones
  starLines,
  isStarPoint,
  handicapStones,
  -- * Property values
  -- ** Text values
  Stringlike (..), convertStringlike,
  Text, fromText, toText,
  SimpleText, fromSimpleText, toSimpleText,
  -- ** Other values
  UnknownPropertyValue, fromUnknownPropertyValue, toUnknownPropertyValue,
  RealValue,
  DoubleValue (..),
  Color (..), cnot,
  VariationMode (..), VariationModeSource (..), defaultVariationMode,
  toVariationMode, fromVariationMode,
  ArrowList, LineList, LabelList, Mark (..),
  GameResult (..),
  WinReason (..),
  Ruleset (..), RulesetType (..), fromRuleset, toRuleset,
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (delete, groupBy, partition, sort)
import Data.Maybe (fromMaybe)
import Game.Goatee.Common
import qualified Game.Goatee.Common.Bigfloat as BF

-- | The FF versions supported by Goatee.  Currently only 4.
supportedFormatVersions :: [Int]
supportedFormatVersions = [4]
-- TODO Support FF versions 1-4.

-- | The default SGF version to use when @FF[]@ is not specified in a root node.
--
-- This value is actually INCORRECT: SGF defines it to be 1, but because we
-- don't support version 1 yet, for the sake of ignoring this issue (for now!)
-- in tests, we fix the default to be 4.
defaultFormatVersion :: Int
defaultFormatVersion = 4
-- TODO Fix the default version to be 1 as SGF mandates.

-- | SGF supports multiple game types.  This list contains the game types that
-- Goatee supports, which is only Go (1).
supportedGameTypes :: [Int]
supportedGameTypes = [1 {- Go -}]

-- | The default size of the board.  The FF[4] SGF spec says that the default Go
-- board is 19x19 square.
boardSizeDefault :: Int
boardSizeDefault = 19

-- | The minimum board size allowed by FF[4], 1.
boardSizeMin :: Int
boardSizeMin = 1

-- | The maximum board size allowed by FF[4], 52.
boardSizeMax :: Int
boardSizeMax = 52

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
data CoordList = CoordList
  { coordListSingles :: [Coord]
  -- ^ Returns the single points in a 'CoordList'.
  , coordListRects :: [(Coord, Coord)]
    -- ^ Returns the rectangles in a 'CoordList'.
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

-- | @starLines width height@ returns 'Just' a list of row/column indices that
-- have star points on a board of the given size, or 'Nothing' if the board size
-- does not have star points defined.
starLines :: Int -> Int -> Maybe [Int]
starLines 19 19 = Just [3, 9, 15]
starLines 13 13 = Just [3, 6, 9]
starLines 9 9 = Just [2, 4, 6]
starLines _ _ = Nothing

-- | @isStarPoint width height x y@ determines whether @(x, y)@ is a known star
-- point on a board of the given width and height.
isStarPoint :: Int -> Int -> Int -> Int -> Bool
isStarPoint width height x y =
  fromMaybe False $
  ((&&) <$> elem x <*> elem y) <$> starLines width height

-- | @handicapStoneIndices !! k@ the positions of the handicap stones for @k@
-- handicap stones, betweeen 0 and 9.  In the pairs, 0 indicates the first star
-- point along an axis, 1 indicates the second, and 2 indicates the third, in
-- the normal 'Coord' ordering.
handicapStoneIndices :: [[(Int, Int)]]
handicapStoneIndices =
  [ []
  , []  -- No single handicap stone for Black; black goes first instead.
  , [(2,0), (0,2)]
  , (2,2) : handicapStoneIndices !! 2
  , (0,0) : handicapStoneIndices !! 3
  , (1,1) : handicapStoneIndices !! 4
  , (0,1) : (2,1) : handicapStoneIndices !! 4
  , (1,1) : handicapStoneIndices !! 6
  , (1,0) : (1,2) : handicapStoneIndices !! 6
  , (1,1) : handicapStoneIndices !! 8
  ]

-- | @handicapStones width height handicap@ returns a list of points where
-- handicap stones should be placed for the given handicap, if handicap points
-- are defined for the given board size, otherwise 'Nothing'.
handicapStones :: Int -> Int -> Int -> Maybe [Coord]
handicapStones width height handicap =
  if handicap < 0 || handicap >= length handicapStoneIndices
  then Nothing
  else do positions <- starLines width height
          return $ map (mapTuple (positions !!)) (handicapStoneIndices !! handicap)

-- | A class for SGF data types that are coercable to and from strings.
--
-- The construction of an SGF value with 'stringToSgf' may process the input,
-- such that the resulting stringlike value does not represent the same string
-- as the input.  In other words, the following does *not* necessarily hold:
--
-- > sgfToString . stringToSgf = id   (does not necessarily hold!)
--
-- The following does hold, however, for a single stringlike type:
--
-- > stringToSgf . sgfToString = id
--
-- The 'String' instance is defined with @sgfToString = stringToSgf =
-- id@.  For other types, the string returned by 'sgfToString' is in a
-- raw, user-editable format: characters that need to be escaped in
-- serialized SGF aren't escaped, but the returned value is otherwise
-- similar to SGF format.
class Stringlike a where
  -- | Extracts the string value from an SGF value.
  sgfToString :: a -> String

  -- | Creates an SGF value from a string value.
  stringToSgf :: String -> a

instance Stringlike String where
  sgfToString = id
  stringToSgf = id

-- | Converts between 'Stringlike' types via a string.
--
-- > convertStringlike = stringToSgf . sgfToString
convertStringlike :: (Stringlike a, Stringlike b) => a -> b
convertStringlike = stringToSgf . sgfToString

-- | An SGF text value.
newtype Text = Text
  { fromText :: String
    -- ^ Converts an SGF 'Text' to a string.
  } deriving (Eq, Show)

instance Stringlike Text where
  sgfToString = fromText
  stringToSgf = toText

-- | Converts a string to an SGF 'Text'.
toText :: String -> Text
toText = Text

-- | An SGF SimpleText value.
newtype SimpleText = SimpleText
  { fromSimpleText :: String
    -- ^ Converts an SGF 'SimpleText' to a string.
  } deriving (Eq, Show)

instance Stringlike SimpleText where
  sgfToString = fromSimpleText
  stringToSgf = toSimpleText

sanitizeSimpleText :: String -> String
sanitizeSimpleText = map (\c -> if isSpace c then ' ' else c)

-- | Converts a string to an SGF 'SimpleText', replacing all whitespaces
-- (including newlines) with spaces.
toSimpleText :: String -> SimpleText
toSimpleText = SimpleText . sanitizeSimpleText

-- | The value type for an 'UnknownProperty'.  Currently represented as a
-- string.
data UnknownPropertyValue = UnknownPropertyValue
  { fromUnknownPropertyValue :: String
    -- ^ Returns the string contained within the 'UnknownProperty' this value is
    -- from.
  } deriving (Eq, Show)

instance Stringlike UnknownPropertyValue where
  sgfToString = fromUnknownPropertyValue
  stringToSgf = toUnknownPropertyValue

-- | Constructs a value for a 'UnknownProperty'.
toUnknownPropertyValue :: String -> UnknownPropertyValue
toUnknownPropertyValue = UnknownPropertyValue

-- | An SGF real value is a decimal number of unspecified precision.
type RealValue = BF.Bigfloat

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

-- | SGF flags that control how move variations are to be presented while
-- displaying the game.
data VariationMode = VariationMode
  { variationModeSource :: VariationModeSource
    -- ^ Which moves to display as variations.
  , variationModeBoardMarkup :: Bool
    -- ^ Whether to overlay variations on the board.
  } deriving (Eq, Show)

-- | An enumeration that describes which variations are shown.
data VariationModeSource =
  ShowChildVariations
  -- ^ Show children of the current move.
  | ShowCurrentVariations
    -- ^ Show alternatives to the current move.
  deriving (Bounded, Enum, Eq, Show)

-- | The default variation mode as defined by the SGF spec is @VariationMode
-- ShowChildVariations True@.
defaultVariationMode :: VariationMode
defaultVariationMode = VariationMode ShowChildVariations True

-- | Parses a numeric variation mode, returning nothing if the number is
-- invalid.
toVariationMode :: Int -> Maybe VariationMode
toVariationMode n = case n of
  0 -> Just $ VariationMode ShowChildVariations True
  1 -> Just $ VariationMode ShowCurrentVariations True
  2 -> Just $ VariationMode ShowChildVariations False
  3 -> Just $ VariationMode ShowCurrentVariations False
  _ -> Nothing

-- | Returns the integer value for a variation mode.
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

instance Stringlike GameResult where
  sgfToString result = case result of
    GameResultWin color reason ->
      (case color of { Black -> 'B'; White -> 'W' }) : '+' :
      (case reason of
          WinByScore diff -> show diff
          WinByResignation -> "R"
          WinByTime -> "T"
          WinByForfeit -> "F")
    GameResultDraw -> "0"
    GameResultVoid -> "Void"
    GameResultUnknown -> "?"
    GameResultOther text -> sgfToString text

  stringToSgf str = case str of
    "0" -> GameResultDraw
    "Draw" -> GameResultDraw
    "Void" -> GameResultVoid
    "?" -> GameResultUnknown
    _ ->
      let result = case str of
            'B':'+':winReasonStr -> parseWin (GameResultWin Black) winReasonStr
            'W':'+':winReasonStr -> parseWin (GameResultWin White) winReasonStr
            _ -> unknownResult
          parseWin builder winReasonStr = case winReasonStr of
            "R" -> builder WinByResignation
            "Resign" -> builder WinByResignation
            "T" -> builder WinByTime
            "Time" -> builder WinByTime
            "F" -> builder WinByForfeit
            "Forfeit" -> builder WinByForfeit
            _ -> case reads winReasonStr of
              (score, ""):_ -> builder $ WinByScore score
              _ -> unknownResult
          unknownResult = GameResultOther $ toSimpleText str
      in result

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

instance Stringlike Ruleset where
  sgfToString = fromRuleset
  stringToSgf = toRuleset

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
