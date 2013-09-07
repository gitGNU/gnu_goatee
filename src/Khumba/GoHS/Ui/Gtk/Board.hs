module Khumba.GoHS.Ui.Gtk.Board where

import Control.Monad (forM_, void, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.IORef
import Data.Map ((!))
import Data.Maybe
import Graphics.UI.Gtk
import Khumba.GoHS.Sgf
import Khumba.GoHS.Ui.Gtk.Common
import qualified Data.Map as Map

-- | If false, then the up and down keys will move toward and away
-- from the game tree root, and left and right will move between
-- siblings.  If true, these are reversed.
useHorizontalKeyNavigation :: Bool
useHorizontalKeyNavigation = True

keyNavActions :: UiState a => Map.Map String (a -> IO Bool)
keyNavActions = Map.fromList $
                if useHorizontalKeyNavigation
                then [("Up", goLeft),
                      ("Down", goRight),
                      ("Left", goUp),
                      ("Right", goDown)]
                else [("Up", goUp),
                      ("Down", goDown),
                      ("Left", goLeft),
                      ("Right", goRight)]

-- | Generic class for implementations of widgets that render boards.
class Monad m => GoBoardWidget w m where
  goBoardWidgetUpdate :: BoardState -> w -> m w

-- | A GTK widget that renders a Go board.
data GtkBoard ui = GtkBoard { gtkBoardUi :: UiRef ui
                            , gtkBoardWindow :: Window
                            , gtkBoardTable :: Table
                            , gtkBoardCells :: Map.Map (Int, Int) Button
                            , gtkBoardWidth :: Int
                            , gtkBoardHeight :: Int
                            }

-- | Creates a 'GtkBoard' for rendering Go boards of the given size.
gtkBoardNew :: UiState ui
            => UiRef ui
            -> Int -- ^ Width
            -> Int -- ^ Height
            -> IO (GtkBoard ui)
gtkBoardNew uiRef width height = do
  let clickHandler = boardClickHandler uiRef

  table <- tableNew height width False
  cellsRef <- newIORef Map.empty
  forM_ [0..width-1] $ \x ->
    forM_ [0..height-1] $ \y -> do
      let xy = (x, y)
      button <- buttonNewWithLabel ""
      buttonSetRelief button ReliefNone
      on button buttonActivated $ clickHandler x y
      tableAttachDefaults table button x (x + 1) y (y + 1)
      modifyIORef cellsRef (Map.insert xy button)
  cells <- readIORef cellsRef

  window <- windowNew
  windowSetDefaultSize window 400 330
  containerAdd window table

  -- TODO Don't quit if other windows are open.
  on window deleteEvent $ liftIO mainQuit >> return False

  on window keyReleaseEvent $ do
    key <- eventKeyName
    mods <- eventModifier
    let maybeAction = Map.lookup key keyNavActions
    when (null mods) $ liftIO $ case maybeAction of
      Nothing -> return ()
      Just action -> void $ readUiRef uiRef >>= action
    return True

  return GtkBoard { gtkBoardUi = uiRef
                  , gtkBoardWindow = window
                  , gtkBoardTable = table
                  , gtkBoardCells = cells
                  , gtkBoardWidth = width
                  , gtkBoardHeight = height
                  }

gtkBoardShow :: UiState ui => GtkBoard ui -> IO ()
gtkBoardShow = widgetShowAll . gtkBoardWindow

instance UiState ui => GoBoardWidget (GtkBoard ui) IO where
  goBoardWidgetUpdate boardState gtkBoard = do
    let cells = gtkBoardCells gtkBoard
        width = gtkBoardWidth gtkBoard
        height = gtkBoardHeight gtkBoard
    forM_ [0..width-1] $ \x ->
      forM_ [0..height-1] $ \y -> do
        let xy = (x, y)
            button = cells ! xy
        buttonSetLabel button $ show $ getCoordState xy boardState
    windowSetTitle (gtkBoardWindow gtkBoard) $ head $ lines $ show boardState
    return gtkBoard

boardClickHandler :: UiState a => UiRef a -> Int -> Int -> IO ()
boardClickHandler uiRef x y = do
  ui <- readUiRef uiRef
  playAt ui (x, y)
