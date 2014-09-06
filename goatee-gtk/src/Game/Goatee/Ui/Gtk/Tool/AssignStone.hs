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
import Control.Monad (forM_, when)
import Game.Goatee.Lib.Board
import qualified Game.Goatee.Lib.Monad as Monad
import Game.Goatee.Lib.Monad (
  deleteChildAt, execGo, getAssignedStone, getCursor, modifyAssignedStones,
  )
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Latch
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  HBox,
  RadioButton,
  containerAdd,
  get,
  hBoxNew,
  on,
  radioButtonNewWithLabel, radioButtonNewWithLabelFromWidget,
  set,
  toggleButtonActive, toggled,
  toWidget,
  )

-- | A 'UiTool' that toggles assigned stones in rectangles on the board.
data AssignStoneTool ui = AssignStoneTool
  { myUi :: ui
  , myToolState :: ToolState
  , myStone :: Maybe Color
  , myWidgets :: Widgets
  }

-- | Widgets that are shared between 'AssignStone' instances for a common panel.
data Widgets = Widgets
  { myBox :: HBox
  , myBlackButton :: RadioButton
  , myWhiteButton :: RadioButton
  , myEmptyButton :: RadioButton
  , myViewUpdateLatch :: Latch
    -- ^ This latch should be held on whenever updating the radio buttons above.
  }

instance UiCtrl go ui => UiTool go ui (AssignStoneTool ui) where
  toolState = myToolState

  toolPanelWidget = Just . toWidget . myBox . myWidgets

  toolOnActivating me = do
    let latch = myViewUpdateLatch $ myWidgets me
    withLatchOn latch $ set (myRadioButton me) [toggleButtonActive := True]

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

-- | Creates a 'AssignStoneTool' that will modify regions of the given mark on
-- the board.  If given another 'AssignStoneTool', then this tool will share
-- 'Widgets' with the existing tool, otherwise it will create new 'Widgets' from
-- scratch.  All instances of 'AssignStoneTool' are meant to share a single
-- instance of 'Widgets'.
create :: UiCtrl go ui
       => ui
       -> Maybe Color
       -> Maybe (AssignStoneTool ui)
       -> ToolState
       -> IO (AssignStoneTool ui)
create ui stone existingTool toolState = do
  widgets <- maybe (createWidgets ui) (return . myWidgets) existingTool
  return AssignStoneTool
    { myUi = ui
    , myToolState = toolState
    , myStone = stone
    , myWidgets = widgets
    }

-- | Creates a 'Widgets' for 'AssignStoneTool's, and configures the widgets
-- within to activate different tools.
createWidgets :: UiCtrl go ui => ui -> IO Widgets
createWidgets ui = do
  box <- hBoxNew True 0
  blackButton <- radioButtonNewWithLabel "Black"
  whiteButton <- radioButtonNewWithLabelFromWidget blackButton "White"
  emptyButton <- radioButtonNewWithLabelFromWidget blackButton "Empty"
  latch <- newLatch
  forM_ [ (blackButton, ToolAssignBlack)
        , (whiteButton, ToolAssignWhite)
        , (emptyButton, ToolAssignEmpty)
        ] $ \(button, toolType) -> do
    containerAdd box button
    on button toggled $ do
      active <- get button toggleButtonActive
      when active $ whenLatchOff latch $ setTool ui toolType
  return Widgets
    { myBox = box
    , myBlackButton = blackButton
    , myWhiteButton = whiteButton
    , myEmptyButton = emptyButton
    , myViewUpdateLatch = latch
    }

-- | Picks the 'RadioButton' corresponding to an 'AssignStoneTool' from the
-- tool's 'Widgets'.
myRadioButton :: AssignStoneTool ui -> RadioButton
myRadioButton me =
  (case myStone me of
    Just Black -> myBlackButton
    Just White -> myWhiteButton
    Nothing -> myEmptyButton) $ myWidgets me

oppositeOfAssignmentAtPoint :: UiCtrl go ui
                            => AssignStoneTool ui
                            -> Coord
                            -> IO (Maybe (Maybe Color))
oppositeOfAssignmentAtPoint me coord =
  toggle (myStone me) <$> doUiGo (myUi me) (getAssignedStone coord)
