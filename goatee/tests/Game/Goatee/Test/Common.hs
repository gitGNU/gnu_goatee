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

-- | Common utilities for testing code.
module Game.Goatee.Test.Common (
  (@/=?),
  assertElem,
  printableAsciiChars,
  ) where

import Control.Monad (unless, when)
import Test.HUnit (Assertion, assertFailure)

-- | Asserts that two values are not equal.  Typically, the left argument is a
-- constant value that the right, computed argument should not equal, to mirror
-- '@=?'.
(@/=?) :: (Eq a, Show a) => a -> a -> Assertion
notExpected @/=? actual =
  when (notExpected == actual) $ assertFailure $
  let notExpected' = show notExpected
      actual' = show actual
  in if actual' == notExpected'
     then "did not expect: " ++ actual'
     else "      this value: " ++ actual' ++
          "\nshould not equal: " ++ notExpected'

assertElem :: (Eq a, Show a) => a -> [a] -> Assertion
assertElem item list =
  unless (item `elem` list) $ assertFailure $
    "Expected to find " ++ show item ++ " in the list " ++ show list ++ "."

printableAsciiChars :: String
printableAsciiChars = [' '..'~']
