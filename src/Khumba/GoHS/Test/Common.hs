module Khumba.GoHS.Test.Common ( assertElem
                               , printableAsciiChars
                               ) where

import Control.Monad
import Test.HUnit

printableAsciiChars :: String
printableAsciiChars = [' '..'~']

assertElem :: (Eq a, Show a) => a -> [a] -> Assertion
assertElem item list =
  unless (item `elem` list) $ assertFailure $
    "Expected to find " ++ show item ++ " in the list " ++ show list ++ "."
