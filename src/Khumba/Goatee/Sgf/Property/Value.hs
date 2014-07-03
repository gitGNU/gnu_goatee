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

-- | Metadata about the types of property values.
module Khumba.Goatee.Sgf.Property.Value (
  PropertyValueType (pvtParser, pvtRenderer, pvtRendererPretty),
  colorPvt,
  coordElistPvt,
  coordListPvt,
  coordPairListPvt,
  doublePvt,
  gameResultPvt,
  integralPvt,
  labelListPvt,
  movePvt,
  nonePvt,
  realPvt,
  rulesetPvt,
  simpleTextPairPvt,
  simpleTextPvt,
  sizePvt,
  textPvt,
  unknownPropertyPvt,
  variationModePvt,
  ) where

import qualified Khumba.Goatee.Sgf.Property.Parser as P
import qualified Khumba.Goatee.Sgf.Property.Renderer as R
import Khumba.Goatee.Sgf.Renderer
import Khumba.Goatee.Sgf.Types
import Text.ParserCombinators.Parsec (Parser)

data PropertyValueType a = PropertyValueType {
  pvtParser :: Parser a
  , pvtRenderer :: a -> Render ()
  , pvtRendererPretty :: a -> Render ()
  }

colorPvt :: PropertyValueType Color
colorPvt = PropertyValueType {
  pvtParser = P.colorParser
  , pvtRenderer = R.renderColorBracketed
  , pvtRendererPretty = R.renderColorPretty
  }

coordElistPvt :: PropertyValueType CoordList
coordElistPvt = PropertyValueType {
  pvtParser = P.coordElistParser
  , pvtRenderer = R.renderCoordElistBracketed
  , pvtRendererPretty = R.renderCoordElistPretty
  }

coordListPvt :: PropertyValueType CoordList
coordListPvt = PropertyValueType {
  pvtParser = P.coordListParser
  , pvtRenderer = R.renderCoordListBracketed
  , pvtRendererPretty = R.renderCoordListPretty
  }

coordPairListPvt :: PropertyValueType [(Coord, Coord)]
coordPairListPvt = PropertyValueType {
  pvtParser = P.coordPairListParser
  , pvtRenderer = R.renderCoordPairListBracketed
  , pvtRendererPretty = R.renderCoordPairListPretty
  }

doublePvt :: PropertyValueType DoubleValue
doublePvt = PropertyValueType {
  pvtParser = P.doubleParser
  , pvtRenderer = R.renderDoubleBracketed
  , pvtRendererPretty = R.renderDoublePretty
  }

gameResultPvt :: PropertyValueType GameResult
gameResultPvt = PropertyValueType {
  pvtParser = P.gameResultParser
  , pvtRenderer = R.renderGameResultBracketed
  , pvtRendererPretty = R.renderGameResultPretty
  }

integralPvt :: (Integral a, Read a, Show a) => PropertyValueType a
integralPvt = PropertyValueType {
  pvtParser = P.integralParser
  , pvtRenderer = R.renderIntegralBracketed
  , pvtRendererPretty = R.renderIntegralPretty
  }

labelListPvt :: PropertyValueType [(Coord, SimpleText)]
labelListPvt = PropertyValueType {
  pvtParser = P.labelListParser
  , pvtRenderer = R.renderLabelListBracketed
  , pvtRendererPretty = R.renderLabelListPretty
  }

movePvt :: PropertyValueType (Maybe Coord)
movePvt = PropertyValueType {
  pvtParser = P.moveParser
  , pvtRenderer = R.renderMoveBracketed
  , pvtRendererPretty = R.renderMovePretty
  }

nonePvt :: PropertyValueType ()
nonePvt = PropertyValueType {
  pvtParser = P.noneParser
  , pvtRenderer = R.renderNoneBracketed
  , pvtRendererPretty = R.renderNonePretty
  }

realPvt :: PropertyValueType RealValue
realPvt = PropertyValueType {
  pvtParser = P.realParser
  , pvtRenderer = R.renderRealBracketed
  , pvtRendererPretty = R.renderRealPretty
  }

rulesetPvt :: PropertyValueType Ruleset
rulesetPvt = PropertyValueType {
  pvtParser = P.rulesetParser
  , pvtRenderer = R.renderRulesetBracketed
  , pvtRendererPretty = R.renderRulesetPretty
  }

simpleTextPairPvt :: PropertyValueType (SimpleText, SimpleText)
simpleTextPairPvt = PropertyValueType {
  pvtParser = P.simpleTextPairParser
  , pvtRenderer = R.renderSimpleTextPairBracketed
  , pvtRendererPretty = R.renderSimpleTextPairPretty
  }

simpleTextPvt :: PropertyValueType SimpleText
simpleTextPvt = PropertyValueType {
  pvtParser = P.simpleTextParser
  , pvtRenderer = R.renderSimpleTextBracketed
  , pvtRendererPretty = R.renderSimpleTextPretty
  }

sizePvt :: PropertyValueType (Int, Int)
sizePvt = PropertyValueType {
  pvtParser = P.sizeParser
  , pvtRenderer = R.renderSizeBracketed
  , pvtRendererPretty = R.renderSizePretty
  }

textPvt :: PropertyValueType Text
textPvt = PropertyValueType {
  pvtParser = P.textParser
  , pvtRenderer = R.renderTextBracketed
  , pvtRendererPretty = R.renderTextPretty
  }

unknownPropertyPvt :: PropertyValueType UnknownPropertyValue
unknownPropertyPvt = PropertyValueType {
  pvtParser = P.unknownPropertyParser
  , pvtRenderer = R.renderUnknownPropertyBracketed
  , pvtRendererPretty = R.renderUnknownPropertyPretty
  }

variationModePvt :: PropertyValueType VariationMode
variationModePvt = PropertyValueType {
  pvtParser = P.variationModeParser
  , pvtRenderer = R.renderVariationModeBracketed
  , pvtRendererPretty = R.renderVariationModePretty
  }
