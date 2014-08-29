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

module Game.Goatee.Ui.Gtk.Tool (
  createTools,
  ) where

import Control.Applicative ((<$>))
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.Map (Map)
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Tool.Play
import Game.Goatee.Ui.Gtk.Tool.Null

-- | Instantiates 'UiTool' instances for all of the 'ToolType's, and returns a
-- map for looking up tools by their type.
createTools :: forall go ui. UiCtrl go ui => ui -> IO (Map ToolType (AnyTool go ui))
createTools ui =
  foldrM
  (\toolType tools -> do
     tool <- case toolType of
       ToolPlay -> AnyTool <$> (toolCreate ui toolType "Play" :: IO (PlayTool ui))
       ToolJump -> AnyTool <$> (toolCreate ui toolType "Jump to move" :: IO (NullTool ui))
       ToolScore -> AnyTool <$> (toolCreate ui toolType "Score" :: IO (NullTool ui))
       ToolBlack -> AnyTool <$> (toolCreate ui toolType "Paint black stones" :: IO (NullTool ui))
       ToolWhite -> AnyTool <$> (toolCreate ui toolType "Paint white stones" :: IO (NullTool ui))
       ToolErase -> AnyTool <$> (toolCreate ui toolType "Erase stones" :: IO (NullTool ui))
       ToolArrow -> AnyTool <$> (toolCreate ui toolType "Draw arrows" :: IO (NullTool ui))
       ToolMarkCircle -> AnyTool <$> (toolCreate ui toolType "Mark circles" :: IO (NullTool ui))
       ToolLabel -> AnyTool <$> (toolCreate ui toolType "Label points" :: IO (NullTool ui))
       ToolLine -> AnyTool <$> (toolCreate ui toolType "Draw lines" :: IO (NullTool ui))
       ToolMarkX -> AnyTool <$> (toolCreate ui toolType "Mark Xs" :: IO (NullTool ui))
       ToolMarkSelected -> AnyTool <$> (toolCreate ui toolType "Mark selected" :: IO (NullTool ui))
       ToolMarkSquare -> AnyTool <$> (toolCreate ui toolType "Mark squares" :: IO (NullTool ui))
       ToolMarkTriangle -> AnyTool <$> (toolCreate ui toolType "Mark trianges" :: IO (NullTool ui))
       ToolVisible ->
         AnyTool <$> (toolCreate ui toolType "Toggle points visible" :: IO (NullTool ui))
       ToolDim -> AnyTool <$> (toolCreate ui toolType "Toggle points dimmed" :: IO (NullTool ui))
     return $ Map.insert toolType tool tools)
  Map.empty
  [minBound..]
