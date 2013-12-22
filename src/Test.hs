module Main (main) where

import qualified Khumba.GoHS.CommonTest
import qualified Khumba.GoHS.SgfTest
import qualified Khumba.GoHS.Sgf.MonadTest
import qualified Khumba.GoHS.Sgf.ParserTest
import Test.Framework (defaultMain)

tests = [ Khumba.GoHS.CommonTest.tests
        , Khumba.GoHS.SgfTest.tests
        , Khumba.GoHS.Sgf.MonadTest.tests
        , Khumba.GoHS.Sgf.ParserTest.tests
        ]

main :: IO ()
main = defaultMain tests
