-----------------------------------------------------------------------------
--
-- Module      :  UiBoard
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module UiBoard (
  GoWindow, getGoWindow, goWindowNew
) where

import Graphics.UI.Gtk.Windows.Window (Window, windowNew)
import qualified Sgf as Sgf

data GoWindow = GoWindow { getGoWindow :: Window }

goWindowNew :: IO GoWindow
goWindowNew = do
  window <- windowNew
  return $ GoWindow window
