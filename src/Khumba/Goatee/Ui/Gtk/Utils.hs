-- | General GTK utilities that don't exist in the Gtk2Hs.
module Khumba.Goatee.Ui.Gtk.Utils (
  onEntryChange
  ) where

import Graphics.UI.Gtk

-- | Registers a handler to be called when the value contained in the entry's
-- buffer changes.  The handler is called with the new value.
onEntryChange :: EntryClass self => self -> (String -> IO ()) -> IO ()
onEntryChange entry handler = do
  buffer <- entryGetBuffer entry
  on buffer entryBufferInsertedText $ \_ _ _ -> runHandler
  on buffer entryBufferDeletedText $ \_ _ -> runHandler
  return ()
  where runHandler = entryGetText entry >>= handler
