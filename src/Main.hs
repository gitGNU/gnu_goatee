module Main (
    main
) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk
import Khumba.GoHS.Common
import Khumba.GoHS.Sgf
import Khumba.GoHS.Ui.Gtk

main :: IO ()
main = do
  args <- initGUI
  openBoard (rootNode 9 9)
  mainGUI
