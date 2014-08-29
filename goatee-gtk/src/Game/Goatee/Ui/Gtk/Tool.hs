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
import qualified Game.Goatee.Ui.Gtk.Tool.Null as Null
import qualified Game.Goatee.Ui.Gtk.Tool.Play as Play

-- | Instantiates 'UiTool' instances for all of the 'ToolType's, and returns a
-- map for looking up tools by their type.
createTools :: UiCtrl go ui => ui -> IO (Map ToolType (AnyTool go ui))
createTools ui =
  foldrM
  (\toolType tools -> do
    let newState = toolStateNew toolType
    tool <- case toolType of
      ToolPlay -> AnyTool <$> (Play.create ui =<< newState "Play")
      ToolJump -> AnyTool <$> (Null.create ui =<< newState "Jump to move")
      ToolScore -> AnyTool <$> (Null.create ui =<< newState "Score")
      ToolBlack -> AnyTool <$> (Null.create ui =<< newState "Paint black stones")
      ToolWhite -> AnyTool <$> (Null.create ui =<< newState "Paint white stones")
      ToolErase -> AnyTool <$> (Null.create ui =<< newState "Erase stones")
      ToolArrow -> AnyTool <$> (Null.create ui =<< newState "Draw arrows")
      ToolMarkCircle -> AnyTool <$> (Null.create ui =<< newState "Mark circles")
      ToolLabel -> AnyTool <$> (Null.create ui =<< newState "Label points")
      ToolLine -> AnyTool <$> (Null.create ui =<< newState "Draw lines")
      ToolMarkX -> AnyTool <$> (Null.create ui =<< newState "Mark Xs")
      ToolMarkSelected -> AnyTool <$> (Null.create ui =<< newState "Mark selected")
      ToolMarkSquare -> AnyTool <$> (Null.create ui =<< newState "Mark squares")
      ToolMarkTriangle -> AnyTool <$> (Null.create ui =<< newState "Mark triangles")
      ToolVisible -> AnyTool <$> (Null.create ui =<< newState "Toggle points visible")
      ToolDim -> AnyTool <$> (Null.create ui =<< newState "Toggle points dimmed")
    return $ Map.insert toolType tool tools)
  Map.empty
  [minBound..]
