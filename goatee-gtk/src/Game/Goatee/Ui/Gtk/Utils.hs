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
module Game.Goatee.Ui.Gtk.Utils (
  onEntryChange,
  spinButtonGetValueAsRational,
  textViewConfigure
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Game.Goatee.Ui.Gtk.Latch
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  EditableClass,
  EntryClass,
  SpinButtonClass,
  TextViewClass,
  bufferChanged,
  editableChanged,
  entryGetText,
  get,
  on,
  set,
  spinButtonGetDigits, spinButtonGetValue,
  textBufferText,
  textViewBuffer,
  )

-- | Registers a handler to be called when the value contained in the entry's
-- buffer changes.  The handler is called with the new value.
onEntryChange :: (EditableClass self, EntryClass self)
              => self
              -> (String -> IO ())
              -> IO ()
onEntryChange entry handler =
  void $ on entry editableChanged runHandler
  where runHandler = entryGetText entry >>= handler

-- | Retrieves the current value of a spin button as a rational that's rounded
-- to the number of digits the spin button is configured for.
spinButtonGetValueAsRational :: SpinButtonClass self => self -> IO Rational
spinButtonGetValueAsRational spin = do
  powerOfTen <- (10 ^) <$> spinButtonGetDigits spin :: IO Rational
  (/ powerOfTen) . toRational . round . (* fromRational powerOfTen) <$>
    spinButtonGetValue spin

-- | Configures event handlers on a 'TextView'.
textViewConfigure :: TextViewClass self => self -> (String -> IO ()) -> IO (String -> IO ())
textViewConfigure textView onViewChange = do
  -- When a 'TextBuffer' is programatically assigned to, two change events are
  -- fired, one to delete the old text and one to insert the new text.  For text
  -- views connected to models, we don't want to handle the intermediate value
  -- by writing it back to the model because this triggers dirtyness (for
  -- example, when moving between two adjacent game nodes with different
  -- comments, firing the change handler with an empty comment will change the
  -- node and make the UI dirty).  So for convenience, we hold a latch on while
  -- we are doing a model-to-view update in order to prevent the handler from
  -- firing while we're doing the assignment, then manually fire the handler
  -- afterward.  This avoids the double-assignment problem.
  buffer <- get textView textViewBuffer
  latch <- newLatch
  on buffer bufferChanged $ whenLatchOff latch $ do
    newValue <- get buffer textBufferText
    onViewChange newValue
  let setValue value = do
        oldValue <- get buffer textBufferText
        when (value /= oldValue) $ do
          withLatchOn latch $ set buffer [textBufferText := value]
          -- Like other model-view widgets, we keep firing handlers in the view
          -- and model, writing back and forth until synchronized.
          onViewChange value
  return setValue
