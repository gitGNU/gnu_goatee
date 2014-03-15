-- This file is part of Goatee.
--
-- Copyright 2014 Bryan Gardiner
--
-- Goatee is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Goatee is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Goatee.  If not, see <http://www.gnu.org/licenses/>.

-- | Implementation of the main window that contains the game board.
module Khumba.Goatee.Ui.Gtk.MainWindow (
  MainWindow
  , create
  , initialize
  , destruct
  , display
  , myWindow
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Tree (drawTree, unfoldTree)
import Graphics.UI.Gtk
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Ui.Gtk.Common
import qualified Khumba.Goatee.Ui.Gtk.Actions as Actions
import Khumba.Goatee.Ui.Gtk.Actions (Actions)
import qualified Khumba.Goatee.Ui.Gtk.GamePropertiesPanel as GamePropertiesPanel
import Khumba.Goatee.Ui.Gtk.GamePropertiesPanel (GamePropertiesPanel)
import qualified Khumba.Goatee.Ui.Gtk.Goban as Goban
import Khumba.Goatee.Ui.Gtk.Goban (Goban)
import qualified Khumba.Goatee.Ui.Gtk.InfoLine as InfoLine
import Khumba.Goatee.Ui.Gtk.InfoLine (InfoLine)
import System.FilePath
import System.IO (hPutStrLn, stderr)

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
                                , myGamePropertiesPanel :: GamePropertiesPanel ui
                                , myGoban :: Goban ui
                                , myInfoLine :: InfoLine ui
                                , myFileChangedHandler :: IORef (Maybe Registration)
                                }

create :: UiCtrl ui => UiRef ui -> IO (MainWindow ui)
create uiRef = do
  window <- windowNew
  windowSetDefaultSize window 640 480

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
    [ Actions.myFileNewAction
    , Actions.myFileOpenAction
    , Actions.myFileSaveAction
    , Actions.myFileSaveAsAction
    ]

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
      action <- fromMaybe (error $ "Tool has no action: " ++ show tool) <$>
                actionGroupGetAction (Actions.myToolActions actions) (show tool)
      menuItem <- actionCreateMenuItem action
      toolItem <- actionCreateToolItem action
      containerAdd menuToolMenu menuItem
      containerAdd toolbar toolItem

  infoLine <- InfoLine.create uiRef
  boxPackStart boardBox (InfoLine.myLabel infoLine) PackNatural 0

  hPaned <- hPanedNew
  boxPackStart boardBox hPaned PackGrow 0

  --hPanedMin <- get hPaned panedMinPosition
  --hPanedMax <- get hPaned panedMaxPosition
  --putStrLn $ "Paned position in [" ++ show hPanedMin ++ ", " ++ show hPanedMax ++ "]."
  -- TODO Don't hard-code the pane width.
  panedSetPosition hPaned 400 -- (truncate (fromIntegral hPanedMax * 0.8))

  goban <- Goban.create uiRef
  panedAdd1 hPaned $ Goban.myDrawingArea goban

  controlsBook <- notebookNew
  panedAdd2 hPaned controlsBook

  gamePropertiesPanel <- GamePropertiesPanel.create uiRef
  notebookAppendPage controlsBook (GamePropertiesPanel.myWidget gamePropertiesPanel) "Properties"

  fileChangedHandler <- newIORef Nothing

  let mw = MainWindow { myUi = uiRef
                      , myWindow = window
                      , myActions = actions
                      , myGamePropertiesPanel = gamePropertiesPanel
                      , myGoban = goban
                      , myInfoLine = infoLine
                      , myFileChangedHandler = fileChangedHandler
                      }

  on window deleteEvent $ liftIO $ destruct mw >> return False

  return mw

-- | Initialization that must be done after the 'UiCtrl' is available.
initialize :: UiCtrl ui => MainWindow ui -> IO ()
initialize me = do
  ui <- readUiRef (myUi me)
  windowCountInc ui

  writeIORef (myFileChangedHandler me) =<<
    liftM Just (registerFilePathChangedHandler ui "MainWindow" True $ const $ updateFilePath me)

  Actions.initialize $ myActions me
  GamePropertiesPanel.initialize $ myGamePropertiesPanel me
  Goban.initialize $ myGoban me
  InfoLine.initialize $ myInfoLine me

destruct :: UiCtrl ui => MainWindow ui -> IO ()
destruct me = do
  Actions.destruct $ myActions me
  GamePropertiesPanel.destruct $ myGamePropertiesPanel me
  Goban.destruct $ myGoban me
  InfoLine.destruct $ myInfoLine me

  -- A main window owns a UI controller.  Once a main window is destructed,
  -- there should be no remaining handlers registered.
  -- TODO Revisit this if we have multiple windows under a controller.
  ui <- readUiRef (myUi me)
  registeredHandlers ui >>= \handlers -> unless (null handlers) $ hPutStrLn stderr $
    "MainWindow.destruct: Warning, there are still handler(s) registered:" ++
    concatMap (\handler -> "\n- " ++ show handler) handlers
  registeredModesChangedHandlers ui >>= \handlers -> unless (null handlers) $ hPutStrLn stderr $
    "MainWindow.destruct: Warning, there are still modes changed handler(s) registered:" ++
    concatMap (\handler -> "\n- " ++ show handler) handlers

  windowCountDec ui

-- | Makes a 'MainWindow' visible.
display :: MainWindow ui -> IO ()
display = widgetShowAll . myWindow

-- | Takes a object of generic type, extracts a bunch of actions from it, and
-- adds those actions to a menu.
addActionsToMenu :: Menu -> a -> [a -> Action] -> IO ()
addActionsToMenu menu actions accessors =
  forM_ accessors $ \accessor ->
  containerAdd menu =<< actionCreateMenuItem (accessor actions)

updateFilePath :: UiCtrl ui => MainWindow ui -> Maybe String -> IO ()
updateFilePath me newPath =
  windowSetTitle (myWindow me) $ maybe "(Untitled)" takeFileName newPath ++ " - Goatee"
