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

module Game.Goatee.Ui.Gtk.Tool.AssignStone (AssignStoneTool, create) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Game.Goatee.Lib.Board
import qualified Game.Goatee.Lib.Monad as Monad
import Game.Goatee.Lib.Monad
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common

-- | A 'UiTool' that toggles assigned stones in rectangles on the board.
data AssignStoneTool ui = AssignStoneTool
  { myUi :: ui
  , myToolState :: ToolState
  , myStone :: Maybe Color
  }

instance UiCtrl go ui => UiTool go ui (AssignStoneTool ui) where
  toolState = myToolState

  toolGobanClickComplete me (Just from) (Just to) = do
    stoneToAssign <- oppositeOfAssignmentAtPoint me from
    doUiGo (myUi me) $ do
      modifyAssignedStones (coordRange from to) $ const stoneToAssign
      -- If the node ends up with no properties, and is a child node, then
      -- move up and delete the node.
      cursor <- getCursor
      when (null $ cursorProperties cursor) $
        case (cursorParent cursor, cursorChildIndex cursor) of
          (Just _, childIndex) -> do
            Monad.goUp
            deleteChildAt childIndex
            return ()
          _ -> return ()

  toolGobanClickComplete _ _ _ = return ()

  toolGobanRenderGetBoard me cursor = do
    state <- toolGetGobanState me
    case (toolGobanStateStartCoord state, toolGobanStateCurrentCoord state) of
      (Just startCoord, Just endCoord) -> do
        stoneToAssign <- oppositeOfAssignmentAtPoint me startCoord
        return $ cursorBoard $ flip execGo cursor $
          modifyAssignedStones (coordRange startCoord endCoord) $ const stoneToAssign
      _ -> return $ cursorBoard cursor

-- | Creates a 'AssignStoneTool' that will modify regions of the given mark on the
-- board.
create :: UiCtrl go ui => ui -> Maybe Color -> ToolState -> IO (AssignStoneTool ui)
create ui stone toolState =
  return AssignStoneTool
    { myUi = ui
    , myToolState = toolState
    , myStone = stone
    }

oppositeOfAssignmentAtPoint :: UiCtrl go ui
                            => AssignStoneTool ui
                            -> Coord
                            -> IO (Maybe (Maybe Color))
oppositeOfAssignmentAtPoint me coord =
  toggle (myStone me) <$> doUiGo (myUi me) (getAssignedStone coord)
