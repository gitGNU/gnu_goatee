module Main (main) where

import qualified Khumba.GoHS.SgfTest as SgfTest
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

tests = [SgfTest.tests]
  

main :: IO ()
main = defaultMain tests
