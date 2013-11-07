module Main (main) where

import qualified Khumba.GoHS.CommonTest
import qualified Khumba.GoHS.SgfTest
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

tests = [ Khumba.GoHS.CommonTest.tests
        , Khumba.GoHS.SgfTest.tests
        ]

main :: IO ()
main = defaultMain tests
