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

{-# LANGUAGE CPP #-}

-- | Renderers of property values.
--
-- Import "Game.Goatee.Lib.Property" rather than importing this module.
module Game.Goatee.Lib.Property.Renderer (
  renderColorBracketed,
  renderColorPretty,
  renderCoordElistBracketed,
  renderCoordElistPretty,
  renderCoordListBracketed,
  renderCoordListPretty,
  renderCoordPairListBracketed,
  renderCoordPairListPretty,
  renderDoubleBracketed,
  renderDoublePretty,
  renderGameResultBracketed,
  renderGameResultPretty,
  renderGameResultPretty',
  renderIntegralBracketed,
  renderIntegralPretty,
  renderLabelListBracketed,
  renderLabelListPretty,
  renderMoveBracketed,
  renderMovePretty,
  renderNoneBracketed,
  renderNonePretty,
  renderRealBracketed,
  renderRealPretty,
  renderRulesetBracketed,
  renderRulesetPretty,
  renderSimpleTextBracketed,
  renderSimpleTextPairBracketed,
  renderSimpleTextPairPretty,
  renderSimpleTextPretty,
  renderSizeBracketed,
  renderSizePretty,
  renderTextBracketed,
  renderTextPretty,
  renderUnknownPropertyBracketed,
  renderUnknownPropertyPretty,
  renderVariationModeBracketed,
  renderVariationModePretty,
  ) where

import Control.Monad (forM_, void, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (throwError)
#else
import Control.Monad.Error (throwError)
#endif
import Control.Monad.Writer (tell)
import Data.Char (chr, ord)
import Data.List (intersperse)
import qualified Game.Goatee.Common.Bigfloat as BF
import Game.Goatee.Lib.Renderer
import Game.Goatee.Lib.Types

{-# ANN module "HLint: ignore Use <$>" #-}

-- Internal renderers not corresponding to any particular value type.

bracketed :: Render () -> Render ()
bracketed x = tell "[" >> x >> tell "]"

renderLine :: Int -> Render ()
renderLine = rendererOf "line" $ \x ->
  if x >= 0 && x < 52
  then tell [chr $ x + (if x < 26 then ord 'a' else ord 'A' - 26)]
  else throwError $ "renderLine: Index not in [0, 52): " ++ show x

renderCoord :: Coord -> Render ()
renderCoord = rendererOf "coord" $ \(x, y) -> renderLine x >> renderLine y

renderCoordBracketed :: Coord -> Render ()
renderCoordBracketed = fmap bracketed renderCoord

renderCoordPretty :: Coord -> Render ()
renderCoordPretty = rendererOf "coord pretty" $ \(x, y) ->
  mapM_ tell ["(", show x, ", ", show y, ")"]

renderCoordPairPretty :: (Coord, Coord) -> Render ()
renderCoordPairPretty = rendererOf "coord pair pretty" $ \(a, b) -> do
  renderCoordPretty a
  tell "-"
  renderCoordPretty b

renderCoordPair :: (Coord, Coord) -> Render ()
renderCoordPair = rendererOf "coord pair" $ \(a, b) ->
  renderCoord a >> tell ":" >> renderCoord b

renderCoordPairBracketed :: (Coord, Coord) -> Render ()
renderCoordPairBracketed = fmap bracketed renderCoordPair

renderShowable :: Show a => a -> Render ()
renderShowable = tell . show

renderStringlike :: (Show a, Stringlike a) => Bool -> a -> Render ()
renderStringlike isComposed =
  rendererOf (if isComposed then "composed stringlike" else "stringlike") $ \str ->
  tell $ escape $ sgfToString str
  where escape [] = []
        escape (first:rest) | first `elem` specialChars = '\\':first:escape rest
                            | otherwise = first:escape rest
        -- TODO Deduplicate these characters with the parser:
        specialChars = if isComposed then ":]\\" else "]\\"

-- Note that unlike the serialized SGF version, we don't care about escaping
-- characters in composed strings.
renderStringlikePretty :: (Show a, Stringlike a) => a -> Render ()
renderStringlikePretty = rendererOf "stringlike pretty" $ tell . sgfToString

-- Public renderers.

renderColorBracketed :: Color -> Render ()
renderColorBracketed color = bracketed $ tell $ case color of
  Black -> "B"
  White -> "W"

renderColorPretty :: Color -> Render ()
renderColorPretty Black = tell "Black"
renderColorPretty White = tell "White"

renderCoordElistBracketed :: CoordList -> Render ()
renderCoordElistBracketed = rendererOf "coord elist bracketed" $ \list ->
  if null $ expandCoordList list
  then tell "[]"
  else renderCoordListNonempty list

renderCoordElistPretty :: CoordList -> Render ()
renderCoordElistPretty = rendererOf "coord elist pretty" $ \list ->
  if null $ expandCoordList list
  then tell "empty"
  else renderCoordListNonemptyPretty list

renderCoordListBracketed :: CoordList -> Render ()
renderCoordListBracketed = rendererOf "coord list bracketed" $ \list ->
  if null $ expandCoordList list
  then throwError "renderCoordListBracketed: Unexpected empty CoordList."
  else renderCoordListNonempty list

renderCoordListPretty :: CoordList -> Render ()
renderCoordListPretty = rendererOf "coord list pretty" $ \list ->
  if null $ expandCoordList list
  then throwError "renderCoordListPretty: Unexpected empty CoordList."
  else renderCoordListNonemptyPretty list

renderCoordListNonempty :: CoordList -> Render ()
renderCoordListNonempty = rendererOf "coord list nonempty" $ \list -> do
  mapM_ renderCoordPairBracketed $ coordListRects list
  mapM_ renderCoordBracketed $ coordListSingles list

renderCoordListNonemptyPretty :: CoordList -> Render ()
renderCoordListNonemptyPretty = rendererOf "coord list nonempty pretty" $ \list ->
  sequence_ $ intersperse (tell ", ") $
  map renderCoordPairPretty (coordListRects list) ++
  map renderCoordPretty (coordListSingles list)

renderCoordPairListBracketed :: [(Coord, Coord)] -> Render ()
renderCoordPairListBracketed = rendererOf "coord pair list bracketed" $ \list ->
  if null list
  then throwError "renderCoordPairListBracketed: Unexpected empty list."
  else mapM_ renderCoordPairBracketed list

renderCoordPairListPretty :: [(Coord, Coord)] -> Render ()
renderCoordPairListPretty list =
  if null list
  then throwError "renderCoordPairListPretty: Unexpected empty list."
  else sequence_ $ intersperse (tell ", ") $ map renderCoordPairPretty list

renderDoubleBracketed :: DoubleValue -> Render ()
renderDoubleBracketed = fmap bracketed $ rendererOf "double" $ \double -> tell $ case double of
  Double1 -> "1"
  Double2 -> "2"

renderDoublePretty :: DoubleValue -> Render ()
renderDoublePretty = rendererOf "double pretty" $ tell . \double -> case double of
  Double1 -> "1"
  Double2 -> "2"

renderGameResultBracketed :: GameResult -> Render ()
renderGameResultBracketed = fmap bracketed $ rendererOf "game result" $ \result -> case result of
  GameResultWin color reason ->
    tell $
    (case color of { Black -> 'B'; White -> 'W' }) : '+' :
    (case reason of
        WinByScore diff -> show diff
        WinByResignation -> "R"
        WinByTime -> "T"
        WinByForfeit -> "F")
  GameResultDraw -> tell "0"
  GameResultVoid -> tell "Void"
  GameResultUnknown -> tell "?"
  GameResultOther text -> renderStringlike False text

renderGameResultPretty :: GameResult -> Render ()
renderGameResultPretty =
  rendererOf "game result pretty" $ void . tell . renderGameResultPretty'

renderGameResultPretty' :: GameResult -> String
renderGameResultPretty' result = case result of
  GameResultWin color reason ->
    (case color of { Black -> 'B'; White -> 'W' }) : '+' :
    (case reason of
        WinByScore diff -> show diff
        WinByResignation -> "Resign"
        WinByTime -> "Time"
        WinByForfeit -> "Forfeit")
  GameResultDraw -> "Draw"
  GameResultVoid -> "Void"
  GameResultUnknown -> "Unknown"
  GameResultOther text -> sgfToString text

renderIntegralBracketed :: (Integral a, Show a) => a -> Render ()
renderIntegralBracketed = bracketed . rendererOf "integral" renderShowable

renderIntegralPretty :: (Integral a, Show a) => a -> Render ()
renderIntegralPretty = rendererOf "integral pretty" renderShowable

renderLabelListBracketed :: [(Coord, SimpleText)] -> Render ()
renderLabelListBracketed = rendererOf "label list" $ \list ->
  if null list
  then throwError "renderLabelListBracketed: Unexpected empty list."
  else forM_ list $ bracketed . \(coord, text) -> do
    renderCoord coord
    tell ":"
    renderStringlike True text

renderLabelListPretty :: [(Coord, SimpleText)] -> Render ()
renderLabelListPretty = rendererOf "label list pretty" $ \list ->
  if null list
  then throwError "renderLabelListPretty: Unexpected empty list."
  else sequence_ $ intersperse (tell ", ") $
       map (\(coord, text) -> do
               renderCoordPretty coord
               tell ":"
               renderStringlikePretty text)
       list

renderMoveBracketed :: Maybe Coord -> Render ()
renderMoveBracketed = rendererOf "move bracketed" $ maybe (tell "[]") renderCoordBracketed

renderMovePretty :: Maybe Coord -> Render ()
renderMovePretty = rendererOf "move pretty" $ maybe (tell "Pass") renderCoordPretty

renderNoneBracketed :: () -> Render ()
renderNoneBracketed = rendererOf "none bracketed" $ tell . const "[]"

renderNonePretty :: () -> Render ()
renderNonePretty = rendererOf "none pretty" $ tell . const ""

renderRealBracketed :: RealValue -> Render ()
renderRealBracketed =
  fmap bracketed $ rendererOf "real" (renderShowable :: BF.Bigfloat -> Render ())

renderRealPretty :: RealValue -> Render ()
renderRealPretty = rendererOf "real pretty" (renderShowable :: BF.Bigfloat -> Render ())

renderRulesetBracketed :: Ruleset -> Render ()
renderRulesetBracketed = fmap bracketed $ rendererOf "ruleset" $ tell . fromRuleset

renderRulesetPretty :: Ruleset -> Render ()
renderRulesetPretty = rendererOf "ruleset pretty" $ tell . fromRuleset

renderSimpleTextPairBracketed :: (SimpleText, SimpleText) -> Render ()
renderSimpleTextPairBracketed = fmap bracketed $ rendererOf "simple text pair" $ \(a, b) -> do
  renderStringlike True a
  tell ":"
  renderStringlike True b

renderSimpleTextPairPretty :: (SimpleText, SimpleText) -> Render ()
renderSimpleTextPairPretty = fmap bracketed $ rendererOf "simple text pair pretty" $ \(a, b) -> do
  renderStringlike True a
  tell " "
  renderStringlike True b

renderSimpleTextBracketed :: SimpleText -> Render ()
renderSimpleTextBracketed = fmap bracketed $ rendererOf "simple text" $ renderStringlike False

renderSimpleTextPretty :: SimpleText -> Render ()
renderSimpleTextPretty = fmap bracketed $ rendererOf "simple text pretty" $ tell . fromSimpleText

renderSizeBracketed :: (Int, Int) -> Render ()
renderSizeBracketed = fmap bracketed $ rendererOf "size" $ \(x, y) -> do
  tell $ show x
  when (x /= y) $ tell $ ':' : show y

renderSizePretty :: (Int, Int) -> Render ()
renderSizePretty = rendererOf "size pretty" renderCoordPretty

renderTextBracketed :: Text -> Render ()
renderTextBracketed = fmap bracketed $ rendererOf "text" $ renderStringlike False

renderTextPretty :: Text -> Render ()
renderTextPretty = fmap bracketed $ rendererOf "text pretty" $ tell . fromText

renderUnknownPropertyBracketed :: UnknownPropertyValue -> Render ()
renderUnknownPropertyBracketed =
  fmap bracketed $ rendererOf "unknown property" $ renderStringlike False

renderUnknownPropertyPretty :: UnknownPropertyValue -> Render ()
renderUnknownPropertyPretty =
  rendererOf "unknown property pretty" $ tell . fromUnknownPropertyValue

renderVariationModeBracketed :: VariationMode -> Render ()
renderVariationModeBracketed =
  fmap bracketed $ rendererOf "variation mode" $ tell . show . fromVariationMode

renderVariationModePretty :: VariationMode -> Render ()
renderVariationModePretty = rendererOf "variation mode pretty" $ \(VariationMode source markup) ->
  tell $
  (case source of
      ShowChildVariations -> "Children"
      ShowCurrentVariations -> "Current") ++ " " ++
  (if markup then "Shown" else "Hidden")
