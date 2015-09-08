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

{-# LANGUAGE CPP #-}

-- | Provides wrappers around regular GTK+ widgets, adding support for assigning
-- to widget values without firing signal handlers.
module Game.Goatee.Ui.Gtk.Widget (
  -- * Entry
  GoateeEntry, goateeEntryWidget, goateeEntryNew, goateeEntryGetText, goateeEntrySetText,
  goateeEntryOnChange,
  -- * Spin button
  GoateeSpinButton, goateeSpinButtonNewWithRange, goateeSpinButtonWidget, goateeSpinButtonGetValue,
  goateeSpinButtonSetValue, goateeSpinButtonOnSpinned,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (void)
import qualified Game.Goatee.Common.Bigfloat as BF
import Game.Goatee.Ui.Gtk.Latch
import Game.Goatee.Ui.Gtk.Utils
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  Entry,
  SpinButton,
  entryNew, entryText,
  get,
  onValueSpinned,
  set,
  spinButtonNewWithRange, spinButtonSetValue,
  )

data GoateeEntry = GoateeEntry Entry Latch

goateeEntryNew :: IO GoateeEntry
goateeEntryNew = GoateeEntry <$> entryNew <*> newLatch

goateeEntryWidget :: GoateeEntry -> Entry
goateeEntryWidget (GoateeEntry entry _) = entry

goateeEntryGetText :: GoateeEntry -> IO String
goateeEntryGetText (GoateeEntry entry _) = get entry entryText

-- | Sets an entry's value without firing handlers registered through
-- 'goateeEntryOnChange'.
goateeEntrySetText :: GoateeEntry -> String -> IO ()
goateeEntrySetText (GoateeEntry entry latch) value =
  withLatchOn latch $ set entry [entryText := value]

goateeEntryOnChange :: GoateeEntry -> (String -> IO ()) -> IO ()
goateeEntryOnChange (GoateeEntry entry latch) handler =
  onEntryChange entry $ \value -> whenLatchOff latch $ handler value

data GoateeSpinButton = GoateeSpinButton SpinButton Latch

goateeSpinButtonNewWithRange :: Double -> Double -> Double -> IO GoateeSpinButton
goateeSpinButtonNewWithRange min max step =
  GoateeSpinButton <$> spinButtonNewWithRange min max step <*> newLatch

goateeSpinButtonWidget :: GoateeSpinButton -> SpinButton
goateeSpinButtonWidget (GoateeSpinButton spinButton _) = spinButton

goateeSpinButtonGetValue :: GoateeSpinButton -> IO BF.Bigfloat
goateeSpinButtonGetValue (GoateeSpinButton spinButton _) =
  spinButtonGetValueAsBigfloat spinButton

-- | Sets a spin button's value without firing handlers registered through
-- 'goateeSpinButtonOnSpinned'.
goateeSpinButtonSetValue :: GoateeSpinButton -> BF.Bigfloat -> IO ()
goateeSpinButtonSetValue (GoateeSpinButton spinButton latch) value =
  withLatchOn latch $ spinButtonSetValue spinButton $ BF.toDouble value

goateeSpinButtonOnSpinned :: GoateeSpinButton -> (BF.Bigfloat -> IO ()) -> IO ()
goateeSpinButtonOnSpinned (GoateeSpinButton spinButton latch) handler =
  void $ onValueSpinned spinButton $ whenLatchOff latch $
  spinButtonGetValueAsBigfloat spinButton >>= handler
