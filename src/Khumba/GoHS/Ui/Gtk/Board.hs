module Khumba.GoHS.Ui.Gtk.Board where

import Control.Monad (forM_, void, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.IORef
import Data.Map ((!))
import Data.Maybe
import Data.Tree (drawTree, unfoldTree)
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
                            , gtkBoardCellButtons :: Map.Map (Int, Int) Button
                            , gtkBoardCellLabels :: Map.Map (Int, Int) Label
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

  on window keyPressEvent $ do
    key <- eventKeyName
    mods <- eventModifier
    let maybeAction = Map.lookup key keyNavActions
    if null mods && isJust maybeAction
      then liftIO $ void $ readUiRef uiRef >>= fromJust maybeAction
      else case key of
        -- Write a list of the current node's properties to the console.
        "t" -> liftIO $ do
          cursor <- readCursor =<< readUiRef uiRef
          print $ nodeProperties $ cursorNode cursor
        -- Draw a tree rooted at the current node to the console.
        "T" -> liftIO $ do
          cursor <- readCursor =<< readUiRef uiRef
          putStrLn $ drawTree $ flip unfoldTree (cursorNode cursor) $ \node ->
            (show $ nodeProperties node, nodeChildren node)
        _ -> return ()
    return True

  infoBox <- vBoxNew False 0
  containerAdd window infoBox

  infoLine <- labelNew Nothing
  containerAdd infoBox infoLine

  tableTreePaned <- vPanedNew
  containerAdd infoBox tableTreePaned

  table <- tableNew height width False
  cellButtonsRef <- newIORef Map.empty
  cellLabelsRef <- newIORef Map.empty
  forM_ [0..width-1] $ \x ->
    forM_ [0..height-1] $ \y -> do
      let xy = (x, y)
      button <- buttonNew
      label <- labelNew Nothing
      containerAdd button label
      buttonSetRelief button ReliefNone
      on button buttonActivated $ clickHandler x y
      tableAttachDefaults table button x (x + 1) y (y + 1)
      modifyIORef cellButtonsRef (Map.insert xy button)
      modifyIORef cellLabelsRef (Map.insert xy label)
  cellButtons <- readIORef cellButtonsRef
  cellLabels <- readIORef cellLabelsRef
  containerAdd tableTreePaned table

  treeTodoLabel <- labelNew $ Just "TODO: Show the tree view here..."
  containerAdd tableTreePaned treeTodoLabel

  return GtkBoard { gtkBoardUi = uiRef
                  , gtkBoardWindow = window
                  , gtkBoardInfoLine = infoLine
                  , gtkBoardTable = table
                  , gtkBoardCellButtons = cellButtons
                  , gtkBoardCellLabels = cellLabels
                  , gtkBoardWidth = width
                  , gtkBoardHeight = height
                  }

gtkBoardShow :: UiState ui => GtkBoard ui -> IO ()
gtkBoardShow = widgetShowAll . gtkBoardWindow

instance UiState ui => GoBoardWidget (GtkBoard ui) IO where
  goBoardWidgetUpdate cursor gtkBoard = do
    let board = cursorBoard cursor
        width = gtkBoardWidth gtkBoard
        height = gtkBoardHeight gtkBoard
    forM_ [0..width-1] $ \x ->
      forM_ [0..height-1] $ \y -> do
        let xy = (x, y)
            label = gtkBoardCellLabels gtkBoard ! xy
            coordState = getCoordState xy board
            text = show coordState
        labelSetMarkup label $ case coordStone coordState of
          Nothing -> text
          Just Black -> "<span weight=\"bold\" foreground=\"red\">" ++ text ++ "</span>"
          Just White -> "<span weight=\"bold\" foreground=\"blue\">" ++ text ++ "</span>"
    let gameInfoMsg = fromMaybe "" $ do
          let info = boardGameInfo board
          black <- gameInfoBlackName info
          white <- gameInfoWhiteName info
          let renderRank = maybe "" (\x -> " (" ++ x ++ ")")
              blackRank = renderRank $ gameInfoBlackRank info
              whiteRank = renderRank $ gameInfoWhiteRank info
          return $ white ++ whiteRank ++ " vs. " ++ black ++ blackRank ++ "\n"
        siblingMsg = case cursorParent cursor of
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
      gameInfoMsg
      ++ "Move " ++ show (boardMoveNumber board) ++ ", " ++ show (boardPlayerTurn board)
      ++ " to play.  Captures: B+" ++ show (boardBlackCaptures board) ++ ", W+"
      ++ show (boardWhiteCaptures board) ++ ".\n" ++ siblingMsg
      ++ (if siblingMsg /= [] && childrenMsg /= [] then "  " else "") ++ childrenMsg
    return gtkBoard

boardClickHandler :: UiState a => UiRef a -> Int -> Int -> IO ()
boardClickHandler uiRef x y = do
  ui <- readUiRef uiRef
  playAt ui (x, y)
