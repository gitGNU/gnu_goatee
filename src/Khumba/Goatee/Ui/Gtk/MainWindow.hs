-- | Implementation of the main window that contains the game board.
module Khumba.Goatee.Ui.Gtk.MainWindow (
  MainWindow
  , create
  , initialize
  , display
  , myWindow
  ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Tree (drawTree, unfoldTree)
import Graphics.UI.Gtk
import Khumba.Goatee.Sgf
import Khumba.Goatee.Common
import Khumba.Goatee.Ui.Gtk.Common
import qualified Khumba.Goatee.Ui.Gtk.Actions as Actions
import Khumba.Goatee.Ui.Gtk.Actions (Actions)
import qualified Khumba.Goatee.Ui.Gtk.Goban as Goban
import Khumba.Goatee.Ui.Gtk.Goban (Goban)
import qualified Khumba.Goatee.Ui.Gtk.InfoLine as InfoLine
import Khumba.Goatee.Ui.Gtk.InfoLine (InfoLine)

-- | If false, then the up and down keys will move toward and away
-- from the game tree root, and left and right will move between
-- siblings.  If true, these are reversed.
useHorizontalKeyNavigation :: Bool
useHorizontalKeyNavigation = True

-- Key handler code requires that these keys don't use modifiers.
keyNavActions :: UiCtrl a => Map String (a -> IO Bool)
keyNavActions = Map.fromList $
                if useHorizontalKeyNavigation
                then [("Up", goLeft),
                      ("Down", goRight),
                      ("Left", goUp),
                      ("Right", flip goDown 0)]
                else [("Up", goUp),
                      ("Down", flip goDown 0),
                      ("Left", goLeft),
                      ("Right", goRight)]

data MainWindow ui = MainWindow { myUi :: UiRef ui
                                , myWindow :: Window
                                , myActions :: Actions
                                , myInfoLine :: InfoLine ui
                                , myGoban :: Goban ui
                                }

create :: UiCtrl ui => UiRef ui -> IO (MainWindow ui)
create uiRef = do
  window <- windowNew
  windowSetDefaultSize window 640 480

  -- TODO Don't quit if other windows are open.
  -- TODO Return false??
  on window deleteEvent $ liftIO mainQuit >> return False

  on window keyPressEvent $ do
    key <- eventKeyName
    mods <- eventModifier
    let km = (key, mods)
    let maybeAction = Map.lookup key keyNavActions
    cond (return False)
      [(null mods && isJust maybeAction,
        liftIO $ readUiRef uiRef >>= fromJust maybeAction >> return True),

        -- Write a list of the current node's properties to the console.
       (km == ("t", []), liftIO $ do
           cursor <- readCursor =<< readUiRef uiRef
           print $ nodeProperties $ cursorNode cursor
           return True),

        -- Draw a tree rooted at the current node to the console.
       (km == ("T", [Shift]), liftIO $ do
           cursor <- readCursor =<< readUiRef uiRef
           putStrLn $ drawTree $ flip unfoldTree (cursorNode cursor) $ \node ->
             (show $ nodeProperties node, nodeChildren node)
           return True)]

  actions <- Actions.create uiRef

  boardBox <- vBoxNew False 0
  containerAdd window boardBox

  menuBar <- menuBarNew
  boxPackStart boardBox menuBar PackNatural 0

  menuFile <- menuItemNewWithMnemonic "_File"
  menuFileMenu <- menuNew
  menuShellAppend menuBar menuFile
  menuItemSetSubmenu menuFile menuFileMenu
  addActionsToMenu menuFileMenu actions
    [Actions.myFileNewAction, Actions.myFileOpenAction]

  menuTool <- menuItemNewWithMnemonic "_Tool"
  menuToolMenu <- menuNew
  menuShellAppend menuBar menuTool
  menuItemSetSubmenu menuTool menuToolMenu

  toolbar <- toolbarNew
  boxPackStart boardBox toolbar PackNatural 0

  sequence_ $
    intersperse (do menuSep <- separatorMenuItemNew
                    toolSep <- separatorToolItemNew
                    containerAdd menuToolMenu menuSep
                    containerAdd toolbar toolSep) $
    flip map toolOrdering $ \toolGroup ->
    forM_ toolGroup $ \tool -> do
      action <- fmap (fromMaybe $ error $ "Tool has no action: " ++ show tool) $
                actionGroupGetAction (Actions.myToolActions actions) (show tool)
      menuItem <- actionCreateMenuItem action
      toolItem <- actionCreateToolItem action
      containerAdd menuToolMenu menuItem
      containerAdd toolbar toolItem

  infoLine <- InfoLine.create uiRef
  boxPackStart boardBox (InfoLine.myLabel infoLine) PackNatural 0

  goban <- Goban.create uiRef
  boxPackStart boardBox (Goban.myDrawingArea goban) PackGrow 0

  return MainWindow { myUi = uiRef
                    , myWindow = window
                    , myActions = actions
                    , myInfoLine = infoLine
                    , myGoban = goban
                    }

-- | Initialization that must be done after the 'UiCtrl' is available.
initialize :: UiCtrl ui => MainWindow ui -> IO ()
initialize window = do
  Actions.initialize $ myActions window
  Goban.initialize $ myGoban window
  InfoLine.initialize $ myInfoLine window

-- | Makes a 'MainWindow' visible.
display :: MainWindow ui -> IO ()
display = widgetShowAll . myWindow

-- | Takes a object of generic type, extracts a bunch of actions from it, and
-- adds those actions to a menu.
addActionsToMenu :: Menu -> a -> [a -> Action] -> IO ()
addActionsToMenu menu actions accessors =
  forM_ accessors $ \accessor ->
  containerAdd menu =<< actionCreateMenuItem (accessor actions)
