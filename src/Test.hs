module Main (main) where

import qualified Khumba.Goatee.CommonTest
import qualified Khumba.Goatee.Sgf.BoardTest
import qualified Khumba.Goatee.Sgf.MonadTest
import qualified Khumba.Goatee.Sgf.ParserTest
import Test.Framework (defaultMain)

tests = [ Khumba.Goatee.CommonTest.tests
        , Khumba.Goatee.Sgf.BoardTest.tests
        , Khumba.Goatee.Sgf.MonadTest.tests
        , Khumba.Goatee.Sgf.ParserTest.tests
        ]

main :: IO ()
main = defaultMain tests
