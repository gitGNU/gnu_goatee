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

import Control.Applicative ((<$>))
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common

-- | A 'UiTool' that toggles 'Mark's in rectangles on the board.
data MarkTool ui = MarkTool
  { myUi :: ui
  , myToolState :: ToolState
  , myMark :: Mark
  }

instance UiCtrl go ui => UiTool go ui (MarkTool ui) where
  toolState = myToolState

  toolGobanClickComplete me (Just from) (Just to) = do
    let ui = myUi me
        mark = myMark me
    board <- cursorBoard <$> readCursor ui
    let newMark = case coordMark $ boardCoordState from board of
          Just mark' | mark' == mark -> Nothing
          _ -> Just mark
    doUiGo ui $ mapM_ (modifyMark $ const newMark) $ range from to

  toolGobanClickComplete _ _ _ = return ()

  toolGobanRenderModifyBoard me board = do
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
                ToolGobanDragging _ (Just from) (Just to) -> range from to
                _ -> [])

-- | Creates a 'MarkTool' that will modify regions of the given mark on the
-- board.
create :: UiCtrl go ui => ui -> Mark -> ToolState -> IO (MarkTool ui)
create ui mark toolState =
  return MarkTool
    { myUi = ui
    , myToolState = toolState
    , myMark = mark
    }

-- | @range coord0 coord1@ returns a list of all the coordinates in the
-- rectangle with @coord0@ and @coord1@ as opposite corners.
range :: Coord -> Coord -> [Coord]
range (x0, y0) (x1, y1) =
  [(x, y) | x <- [min x0 x1 .. max x0 x1]
          , y <- [min y0 y1 .. max y0 y1]]

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
