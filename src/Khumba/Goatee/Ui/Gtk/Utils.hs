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

-- | General GTK utilities that don't exist in the Gtk2Hs.
module Khumba.Goatee.Ui.Gtk.Utils (
  onEntryChange,
  ) where

import Graphics.UI.Gtk (
  EntryClass, entryBufferDeletedText, entryBufferInsertedText, entryGetBuffer, entryGetText,
  on,
  )

-- | Registers a handler to be called when the value contained in the entry's
-- buffer changes.  The handler is called with the new value.
onEntryChange :: EntryClass self => self -> (String -> IO ()) -> IO ()
onEntryChange entry handler = do
  buffer <- entryGetBuffer entry
  on buffer entryBufferInsertedText $ \_ _ _ -> runHandler
  on buffer entryBufferDeletedText $ \_ _ -> runHandler
  return ()
  where runHandler = entryGetText entry >>= handler
