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

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk
import Khumba.GoHS.Common
import Khumba.GoHS.Sgf
import Khumba.GoHS.UiBoard

main :: IO ()
main = do
  args <- initGUI
  wnd <- windowNew

  cursorRef <- newIORef $
               either (error "Error creating root cursor.") id $
               rootCursor $
               rootNode 9 9
  turnRef <- newIORef Black
  gtkBoardRef <- newIORef Nothing

  gtkBoard0 <- gtkBoardNew 9 9 $ \x y -> do
    turn <- readIORef turnRef

    modifyIORef cursorRef $ \cursor ->
      flip cursorChild 0 $
      flip cursorModifyNode cursor $ \node ->
        flip addChild node $ emptyNode { nodeProperties =
                                            [case turn of
                                                Black -> B (x, y)
                                                White -> W (x, y)]
                                       }
    --putStrLn . ("Now at: " ++) . show =<< readIORef cursorRef

    modifyIORef turnRef cnot

    cursor <- readIORef cursorRef
    gtkBoard <- return . fromJust =<< readIORef gtkBoardRef
    gtkBoard' <- goBoardWidgetUpdate (cursorBoard cursor) gtkBoard
    writeIORef gtkBoardRef $ Just gtkBoard'
    windowSetTitle wnd $ head $ lines $ show $ cursorBoard cursor

  cursor0 <- readIORef cursorRef
  gtkBoard1 <- goBoardWidgetUpdate (cursorBoard cursor0) gtkBoard0
  writeIORef gtkBoardRef $ Just gtkBoard1

  on wnd deleteEvent $ liftIO mainQuit >> return False
  windowSetDefaultSize wnd 800 600
  containerAdd wnd $ gtkBoardTable gtkBoard1
  widgetShowAll wnd
  mainGUI
