{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Graphics.UI.Gtk.Abstract.Widget (widgetShow)
import Graphics.UI.Gtk.General.General (initGUI, mainGUI)
import qualified UiBoard as UiBoard

main :: IO ()
main = do
  args <- initGUI
  wnd <- UiBoard.goWindowNew
  widgetShow $ UiBoard.getGoWindow wnd
  mainGUI
