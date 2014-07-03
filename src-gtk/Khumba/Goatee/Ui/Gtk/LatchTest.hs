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

module Khumba.Goatee.Ui.Gtk.LatchTest (tests) where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Khumba.Goatee.Ui.Gtk.Latch
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

{-# ANN module "HLint: ignore Reduce duplication" #-}

tests = testGroup "Khumba.Goatee.Ui.Gtk.Latch" [
  testCase "a new latch is off" $ do
     latch <- newLatch
     ref <- newIORef 0
     whenLatchOff latch $ modifyIORef ref (+ 1)
     whenLatchOn latch $ modifyIORef ref (+ 2)
     (1 @=?) =<< readIORef ref,

  testCase "a latch can be held on" $ do
     latch <- newLatch
     ref <- newIORef 0
     withLatchOn latch $ do
       whenLatchOff latch $ modifyIORef ref (+ 1)
       whenLatchOn latch $ modifyIORef ref (+ 2)
     (2 @=?) =<< readIORef ref,

  testCase "a new latch returns to being off after it is released" $ do
     latch <- newLatch
     ref <- newIORef 0
     withLatchOn latch $ do
       whenLatchOff latch $ modifyIORef ref (+ 1)
       whenLatchOn latch $ modifyIORef ref (+ 2)
     whenLatchOff latch $ modifyIORef ref (+ 4)
     whenLatchOn latch $ modifyIORef ref (+ 8)
     (6 @=?) =<< readIORef ref
  ]
