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

module Game.Goatee.Ui.Gtk.Tool.Mark (MarkTool, create) where

import Control.Monad (forM_, when)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad (getMark, modifyMark)
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

-- | A 'UiTool' that toggles 'Mark's in rectangles on the board.
data MarkTool ui = MarkTool
  { myUi :: ui
  , myToolState :: ToolState
  , myMark :: Mark
  , myWidgets :: Widgets
  }

-- | Widgets that are shared between 'MarkTool' instances for a common panel.
data Widgets = Widgets
  { myBox :: HBox
  , myCircleButton :: RadioButton
  , mySelectedButton :: RadioButton
  , mySquareButton :: RadioButton
  , myTriangleButton :: RadioButton
  , myXButton :: RadioButton
  , myViewUpdateLatch :: Latch
    -- ^ This latch should be held on whenever updating the radio buttons above.
  }

instance UiCtrl go ui => UiTool go ui (MarkTool ui) where
  toolState = myToolState

  toolPanelWidget = Just . toWidget . myBox . myWidgets

  toolOnActivating me = do
    let latch = myViewUpdateLatch $ myWidgets me
    withLatchOn latch $ set (myRadioButton me) [toggleButtonActive := True]

  toolGobanClickComplete me (Just from) (Just to) = do
    let ui = myUi me
        mark = myMark me
    oldMark <- doUiGo ui $ getMark from
    let newMark = case oldMark of
          Just mark' | mark' == mark -> Nothing
          _ -> Just mark
    doUiGo ui $ mapM_ (modifyMark $ const newMark) $ coordRange from to

  toolGobanClickComplete _ _ _ = return ()

  toolGobanRenderGetBoard me cursor = do
    let board = cursorBoard cursor
    state <- toolGetGobanState me
    return $ case toolGobanStateStartCoord state of
      Nothing -> board
      Just startCoord -> do
        let mark = myMark me
            applyMark = setMarkToOppositeOf mark $
                        boardCoordState startCoord board
        foldr (\coord board' -> boardCoordModify board' coord applyMark)
              board
              (case state of
                ToolGobanHovering (Just coord) -> [coord]
                ToolGobanDragging _ (Just from) (Just to) -> coordRange from to
                _ -> [])

-- | Creates a 'MarkTool' that will modify regions of the given mark on the
-- board.  If given another 'MarkTool', then this tool will share 'Widgets' with
-- the existing tool, otherwise it will create new 'Widgets' from scratch.  All
-- instances of 'MarkTool' are meant to share a single instance of 'Widgets'.
create :: UiCtrl go ui => ui -> Mark -> Maybe (MarkTool ui) -> ToolState -> IO (MarkTool ui)
create ui mark existingTool toolState = do
  widgets <- maybe (createWidgets ui) (return . myWidgets) existingTool
  return MarkTool
    { myUi = ui
    , myToolState = toolState
    , myMark = mark
    , myWidgets = widgets
    }

-- | Creates a 'Widgets' for 'MarkTool's, and configures the widgets within to
-- activate different tools.
createWidgets :: UiCtrl go ui => ui -> IO Widgets
createWidgets ui = do
  box <- hBoxNew True 0
  crButton <- radioButtonNewWithLabel "Cr"
  slButton <- radioButtonNewWithLabelFromWidget crButton "Sl"
  sqButton <- radioButtonNewWithLabelFromWidget crButton "Sq"
  trButton <- radioButtonNewWithLabelFromWidget crButton "Tr"
  xButton <- radioButtonNewWithLabelFromWidget crButton "X"
  latch <- newLatch
  forM_ [ (crButton, ToolMarkCircle)
        , (slButton, ToolMarkSelected)
        , (sqButton, ToolMarkSquare)
        , (trButton, ToolMarkTriangle)
        , (xButton, ToolMarkX)
        ] $ \(button, toolType) -> do
    containerAdd box button
    on button toggled $ do
      active <- get button toggleButtonActive
      when active $ whenLatchOff latch $ setTool ui toolType
  return Widgets
    { myBox = box
    , myCircleButton = crButton
    , mySelectedButton = slButton
    , mySquareButton = sqButton
    , myTriangleButton = trButton
    , myXButton = xButton
    , myViewUpdateLatch = latch
    }

-- | Picks the 'RadioButton' corresponding to a 'MarkTool' from the tool's
-- 'Widgets'.
myRadioButton :: MarkTool ui -> RadioButton
myRadioButton me =
  (case myMark me of
    MarkCircle -> myCircleButton
    MarkSelected -> mySelectedButton
    MarkSquare -> mySquareButton
    MarkTriangle -> myTriangleButton
    MarkX -> myXButton) $ myWidgets me

-- | @setMarkToOppositeOf mark baseCoord targetCoord@ returns @targetCoord@ with
-- its mark modified to be nothing, if @baseCoord@ has @mark@, or @mark@, if
-- @baseCoord@ has something other than @mark@ (possibly no mark).
setMarkToOppositeOf :: Mark -> CoordState -> CoordState -> CoordState
setMarkToOppositeOf mark baseCoord =
  setMark $ case coordMark baseCoord of
              Just mark' | mark' == mark -> Nothing
              _ -> Just mark

-- | Changes the mark in a 'CoordState'.  Returns the initial 'CoordState' if
-- the given mark is already set.
setMark :: Maybe Mark -> CoordState -> CoordState
setMark maybeMark coord =
  if coordMark coord == maybeMark
  then coord
  else coord { coordMark = maybeMark }
