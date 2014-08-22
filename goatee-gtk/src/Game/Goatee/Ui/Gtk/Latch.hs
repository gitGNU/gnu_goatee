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

module Game.Goatee.Ui.Gtk.Latch (
  Latch,
  newLatch,
  withLatchOn,
  whenLatch, whenLatchOff, whenLatchOn,
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A binary switch that is off unless held on during the execution of some IO
-- process.  The state of a latch can be read at any time, but can be held on by
-- at most one thread at a time.
data Latch = Latch
  { latchValue :: IORef Bool
  , latchLock :: MVar ()
  }

-- | Creates a new latch that is off.
newLatch :: IO Latch
newLatch = do
  value <- newIORef False
  lock <- newMVar ()
  return Latch { latchValue = value
               , latchLock = lock
               }

-- | Flips the latch on, executes the action, and finally flips the latch off
-- again (even if the action throws an exception).  A latch may be held on by at
-- most one thread at a time.  If a second thread tries to turn the latch on, it
-- will block.
withLatchOn :: Latch -> IO a -> IO a
withLatchOn (Latch value lock) io = do
  takeMVar lock
  finally (writeIORef value True >> io) $ do
    writeIORef value False
    putMVar lock ()

-- | Executes an action only when the latch is on (if given true) or off (if
-- given false).
whenLatch :: Bool -> Latch -> IO () -> IO ()
whenLatch value latch io = do
  actual <- readIORef $ latchValue latch
  when (value == actual) io

-- | Executes an action only when a latch is off.
whenLatchOff :: Latch -> IO () -> IO ()
whenLatchOff = whenLatch False

-- | Executes an action only when a latch is on.
whenLatchOn :: Latch -> IO () -> IO ()
whenLatchOn = whenLatch True
