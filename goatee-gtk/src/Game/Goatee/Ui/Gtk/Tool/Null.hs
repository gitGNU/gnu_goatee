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

module Game.Goatee.Ui.Gtk.Tool.Null (NullTool, create) where

import Game.Goatee.Ui.Gtk.Common

-- | A 'UiTool' that does nothing and is not selectable in the UI.  'ToolType's
-- that are not yet implemented should be bound to this implementation.
data NullTool ui = NullTool
  { myUi :: ui
  , myToolState :: ToolState
  }

instance UiCtrl go ui => UiTool go ui (NullTool ui) where
  toolState = myToolState

  toolIsImplemented _ = False

create :: UiCtrl go ui => ui -> ToolState -> IO (NullTool ui)
create ui toolState =
  return NullTool
    { myUi = ui
    , myToolState = toolState
    }
