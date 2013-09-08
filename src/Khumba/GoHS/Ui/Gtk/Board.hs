module Khumba.GoHS.Ui.Gtk.Board where

import Control.Monad (forM_, void, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.IORef
import Data.Map ((!))
import Data.Maybe
import Graphics.UI.Gtk hiding (Cursor)
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
  goBoardWidgetUpdate :: Cursor -> w -> m w

-- | A GTK widget that renders a Go board.
data GtkBoard ui = GtkBoard { gtkBoardUi :: UiRef ui
                            , gtkBoardWindow :: Window
                            , gtkBoardInfoLine :: Label
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

  window <- windowNew
  windowSetTitle window "Go"
  windowSetDefaultSize window 440 380

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

  infoBox <- vBoxNew False 0
  containerAdd window infoBox

  infoLine <- labelNew Nothing
  containerAdd infoBox infoLine

  tableTreePaned <- vPanedNew
  containerAdd infoBox tableTreePaned

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
  containerAdd tableTreePaned table

  treeTodoLabel <- labelNew $ Just "TODO: Show the tree view here..."
  containerAdd tableTreePaned treeTodoLabel

  return GtkBoard { gtkBoardUi = uiRef
                  , gtkBoardWindow = window
                  , gtkBoardInfoLine = infoLine
                  , gtkBoardTable = table
                  , gtkBoardCells = cells
                  , gtkBoardWidth = width
                  , gtkBoardHeight = height
                  }

gtkBoardShow :: UiState ui => GtkBoard ui -> IO ()
gtkBoardShow = widgetShowAll . gtkBoardWindow

instance UiState ui => GoBoardWidget (GtkBoard ui) IO where
  goBoardWidgetUpdate cursor gtkBoard = do
    let board = cursorBoard cursor
        cells = gtkBoardCells gtkBoard
        width = gtkBoardWidth gtkBoard
        height = gtkBoardHeight gtkBoard
    forM_ [0..width-1] $ \x ->
      forM_ [0..height-1] $ \y -> do
        let xy = (x, y)
            button = cells ! xy
        buttonSetLabel button $ show $ getCoordState xy board
    let siblingMsg = case cursorParent cursor of
                     Nothing -> "Start of game."
                     Just parent ->
                       let parentChildCount = cursorChildCount parent
                       in if parentChildCount > 1
                          then "Variation " ++ show (cursorChildIndex cursor + 1)
                               ++ "/" ++ show parentChildCount ++ "."
                          else ""
        childrenMsg = let childCount = cursorChildCount cursor
                      in case childCount of
                        0 -> "End of variation."
                        1 -> ""
                        _ -> "<b>" ++ show childCount ++ " variations from here.</b>"
    labelSetMarkup (gtkBoardInfoLine gtkBoard) $
      "Move " ++ show (boardMoveNumber board) ++ ", " ++ show (boardPlayerTurn board)
      ++ " to play.  Captures: B+" ++ show (boardBlackCaptures board) ++ ", W+"
      ++ show (boardWhiteCaptures board) ++ ".\n" ++ siblingMsg
      ++ (if siblingMsg /= [] && childrenMsg /= [] then "  " else "") ++ childrenMsg
    return gtkBoard

boardClickHandler :: UiState a => UiRef a -> Int -> Int -> IO ()
boardClickHandler uiRef x y = do
  ui <- readUiRef uiRef
  playAt ui (x, y)
