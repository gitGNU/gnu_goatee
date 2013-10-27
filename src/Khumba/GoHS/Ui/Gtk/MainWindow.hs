module Khumba.GoHS.Ui.Gtk.MainWindow ( MainWindow
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
import Khumba.GoHS.Sgf
import Khumba.GoHS.Common
import Khumba.GoHS.Ui.Gtk.Common
import qualified Khumba.GoHS.Ui.Gtk.Actions as Actions
import Khumba.GoHS.Ui.Gtk.Actions (Actions)
import qualified Khumba.GoHS.Ui.Gtk.Goban as Goban
import Khumba.GoHS.Ui.Gtk.Goban (Goban)
import qualified Khumba.GoHS.Ui.Gtk.InfoLine as InfoLine
import Khumba.GoHS.Ui.Gtk.InfoLine (InfoLine)

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
                      ("Right", goDown)]
                else [("Up", goUp),
                      ("Down", goDown),
                      ("Left", goLeft),
                      ("Right", goRight)]

data MainWindow ui = MainWindow { myUi :: UiRef ui
                                , myWindow :: Window
                                , myActions :: Actions
                                , myInfoLine :: InfoLine ui
                                , myGoban :: Goban ui
                                }

instance UiCtrl ui => UiView (MainWindow ui) where
  viewChildren mainWindow = [View (myInfoLine mainWindow),
                             View (myGoban mainWindow)]

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
initialize :: MainWindow ui -> IO ()
initialize = Actions.activateInitialTool . myActions

-- | Makes a 'MainWindow' visible.
display :: MainWindow ui -> IO ()
display = widgetShowAll . myWindow
