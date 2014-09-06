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

module Game.Goatee.Ui.Gtk.Tool.Line (
  LineTool, create,
  LinelikeDescriptor, arrowDescriptor, lineDescriptor,
  ) where

import Data.List (delete)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common

-- | A 'UiTool' that toggles lines between points on the board.
data LineTool ui = LineTool
  { myUi :: ui
  , myViewState :: ViewState
  , myToolState :: ToolState
  , myDescriptor :: AnyLinelikeDescriptor
  }

-- | A descriptor for a line-like property: a valued property that contains a
-- list of connections between two points on the board.
data LinelikeDescriptor v = LinelikeDescriptor
  { linelikeDescriptor :: ValuedPropertyInfo [v]
  , linelikeLift :: (Coord, Coord) -> v
  }

-- | An existential type for any linelike descriptor.
data AnyLinelikeDescriptor = forall v. Eq v => AnyLinelikeDescriptor (LinelikeDescriptor v)

-- | A descriptor for directed arrows ('AR').
arrowDescriptor :: AnyLinelikeDescriptor
arrowDescriptor = AnyLinelikeDescriptor LinelikeDescriptor
  { linelikeDescriptor = propertyAR
  , linelikeLift = id
  }

-- | A descriptor for undirected lines ('LN').
lineDescriptor :: AnyLinelikeDescriptor
lineDescriptor = AnyLinelikeDescriptor LinelikeDescriptor
  { linelikeDescriptor = propertyLN
  , linelikeLift = uncurry Line
  }

instance UiCtrl go ui => UiView go ui (LineTool ui) where
  viewName = const "LineTool"

  viewCtrl = myUi

  viewState = myViewState

  viewUpdate = const $ return ()

instance UiCtrl go ui => UiTool go ui (LineTool ui) where
  toolState = myToolState

  toolGobanClickComplete me (Just from) (Just to) | from /= to = case myDescriptor me of
    AnyLinelikeDescriptor linelike ->
      doUiGo (myUi me) $
      modifyPropertyList (linelikeDescriptor linelike) $
      toggleInList $ linelikeLift linelike (from, to)

  toolGobanClickComplete _ _ _ = return ()

  toolGobanRenderGetBoard me cursor = do
    state <- toolGetGobanState me
    case state of
      ToolGobanDragging _ (Just startCoord) (Just endCoord) | startCoord /= endCoord ->
        case myDescriptor me of
          AnyLinelikeDescriptor linelike ->
            return $ cursorBoard $ flip execGo cursor $
            modifyPropertyList (linelikeDescriptor linelike) $
            toggleInList $ linelikeLift linelike (startCoord, endCoord)
      _ -> return $ cursorBoard cursor

-- | Creates a 'LineTool' that will toggle a line-like property between pairs of
-- points on the board.
create :: UiCtrl go ui => ui -> AnyLinelikeDescriptor -> ToolState -> IO (LineTool ui)
create ui descriptor toolState = do
  viewState <- viewStateNew
  return LineTool
    { myUi = ui
    , myViewState = viewState
    , myToolState = toolState
    , myDescriptor = descriptor
    }

-- | Toggles the presense of an item in a list.
toggleInList :: Eq a => a -> [a] -> [a]
toggleInList x xs = (if x `elem` xs then delete else (:)) x xs
