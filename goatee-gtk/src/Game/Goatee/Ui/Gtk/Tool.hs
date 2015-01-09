-- This file is part of Goatee.
--
-- Copyright 2014-2015 Bryan Gardiner
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

import qualified Data.Map as Map
import Data.Map (Map)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import qualified Game.Goatee.Ui.Gtk.Tool.AssignStone as AssignStone
import qualified Game.Goatee.Ui.Gtk.Tool.Line as Line
import qualified Game.Goatee.Ui.Gtk.Tool.Mark as Mark
import qualified Game.Goatee.Ui.Gtk.Tool.Null as Null
import qualified Game.Goatee.Ui.Gtk.Tool.Play as Play
import qualified Game.Goatee.Ui.Gtk.Tool.Visibility as Visibility

-- | Instantiates 'UiTool' instances for all of the 'ToolType's, and returns a
-- map for looking up tools by their type.
createTools :: UiCtrl go ui => ui -> IO (Map ToolType (AnyTool go ui))
createTools ui = do
  toolArrow <- toolStateNew ToolArrow "Draw arrows" >>= Line.create ui Line.arrowDescriptor
  toolAssignBlack <- toolStateNew ToolAssignBlack "Paint black stones" >>=
                     AssignStone.create ui (Just Black) Nothing
  toolAssignEmpty <- toolStateNew ToolAssignEmpty "Paint empty stones" >>=
                     AssignStone.create ui Nothing (Just toolAssignBlack)
  toolAssignWhite <- toolStateNew ToolAssignWhite "Paint white stones" >>=
                     AssignStone.create ui (Just White) (Just toolAssignBlack)
  toolDim <- toolStateNew ToolDim "Toggle points dimmed" >>=
             Visibility.create ui propertyDD boardHasDimmed "dimming"
  toolJump <- toolStateNew ToolJump "Jump to move" >>= Null.create ui
  toolLabel <- toolStateNew ToolLabel "Label points" >>= Null.create ui
  toolLine <- toolStateNew ToolLine "Draw lines" >>= Line.create ui Line.lineDescriptor
  toolMarkCircle <- toolStateNew ToolMarkCircle "Mark circles" >>=
                    Mark.create ui MarkCircle Nothing
  toolMarkSelected <- toolStateNew ToolMarkSelected "Mark selected" >>=
                      Mark.create ui MarkSelected (Just toolMarkCircle)
  toolMarkSquare <- toolStateNew ToolMarkSquare "Mark squares" >>=
                    Mark.create ui MarkSquare (Just toolMarkCircle)
  toolMarkTriangle <- toolStateNew ToolMarkTriangle "Mark trianges" >>=
                      Mark.create ui MarkTriangle (Just toolMarkCircle)
  toolMarkX <- toolStateNew ToolMarkX "Mark Xs" >>=
               Mark.create ui MarkX (Just toolMarkCircle)
  toolPlay <- toolStateNew ToolPlay "Play" >>= Play.create ui
  toolScore <- toolStateNew ToolScore "Score" >>= Null.create ui
  toolVisible <- toolStateNew ToolVisible "Toggle points visible" >>=
                 Visibility.create ui propertyVW boardHasInvisible "visibilities"
  return $ Map.fromList $ map
    (\tool@(AnyTool tool') -> (toolType tool', tool))
    [ AnyTool toolArrow
    , AnyTool toolAssignBlack
    , AnyTool toolAssignEmpty
    , AnyTool toolAssignWhite
    , AnyTool toolDim
    , AnyTool toolJump
    , AnyTool toolLabel
    , AnyTool toolLine
    , AnyTool toolMarkCircle
    , AnyTool toolMarkSelected
    , AnyTool toolMarkSquare
    , AnyTool toolMarkTriangle
    , AnyTool toolMarkX
    , AnyTool toolPlay
    , AnyTool toolScore
    , AnyTool toolVisible
    ]
