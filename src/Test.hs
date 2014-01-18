module Main (main) where

import qualified Khumba.Goatee.CommonTest
import qualified Khumba.Goatee.SgfTest
import qualified Khumba.Goatee.Sgf.MonadTest
import qualified Khumba.Goatee.Sgf.ParserTest
import Test.Framework (defaultMain)

tests = [ Khumba.Goatee.CommonTest.tests
        , Khumba.Goatee.SgfTest.tests
        , Khumba.Goatee.Sgf.MonadTest.tests
        , Khumba.Goatee.Sgf.ParserTest.tests
        ]

main :: IO ()
main = defaultMain tests
