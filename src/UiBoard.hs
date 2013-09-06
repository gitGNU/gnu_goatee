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

module UiBoard where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map ((!))
import Data.Maybe
import Graphics.UI.Gtk
import Sgf
import qualified Data.Map as Map

-- | Generic class for implementations of widgets that render boards.
class Monad m => GoBoardWidget w m where
  goBoardWidgetUpdate :: BoardState -> w -> m w

-- | A GTK widget that renders a Go board.
data GtkBoard = GtkBoard { gtkBoardTable :: Table
                         , gtkBoardCells :: Map.Map (Int, Int) Button
                         , gtkBoardWidth :: Int
                         , gtkBoardHeight :: Int
                         }

-- | Creates a 'GtkBoard' for rendering Go boards of the given size.
gtkBoardNew :: Int -- ^ Width
            -> Int -- ^ Height
            -> (Int -> Int -> IO ()) -- ^ Click handler
            -> IO GtkBoard
gtkBoardNew width height clickHandler = do
  table <- tableNew height width False
  cellsRef <- newIORef Map.empty
  forM_ [0..width-1] $ \x -> do
    forM_ [0..height-1] $ \y -> do
      let xy = (x, y)
      button <- buttonNewWithLabel ""
      buttonSetRelief button ReliefNone
      on button buttonActivated $ clickHandler x y
      tableAttachDefaults table button x (x + 1) y (y + 1)
      modifyIORef cellsRef (Map.insert xy button)
  cells <- readIORef cellsRef
  return $ GtkBoard { gtkBoardTable = table
                    , gtkBoardCells = cells
                    , gtkBoardWidth = width
                    , gtkBoardHeight = height
                    }

instance GoBoardWidget GtkBoard IO where
  goBoardWidgetUpdate boardState gtkBoard = do
    let cells = gtkBoardCells gtkBoard
        width = gtkBoardWidth gtkBoard
        height = gtkBoardHeight gtkBoard
    forM_ [0..width-1] $ \x -> do
      forM_ [0..height-1] $ \y -> do
        let xy = (x, y)
            button = cells ! xy
        buttonSetLabel button $ show $ getCoordState xy boardState
    return gtkBoard
