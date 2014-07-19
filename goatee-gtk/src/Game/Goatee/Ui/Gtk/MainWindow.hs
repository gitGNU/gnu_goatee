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
module Game.Goatee.Ui.Gtk.MainWindow (
  MainWindow,
  create,
  destroy,
  display,
  myWindow,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, liftM, unless)
import Control.Monad.Trans (liftIO)
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Game.Goatee.Ui.Gtk.Common
import qualified Game.Goatee.Ui.Gtk.Actions as Actions
import Game.Goatee.Ui.Gtk.Actions (Actions)
import qualified Game.Goatee.Ui.Gtk.GamePropertiesPanel as GamePropertiesPanel
import Game.Goatee.Ui.Gtk.GamePropertiesPanel (GamePropertiesPanel)
import qualified Game.Goatee.Ui.Gtk.Goban as Goban
import Game.Goatee.Ui.Gtk.Goban (Goban)
import qualified Game.Goatee.Ui.Gtk.InfoLine as InfoLine
import Game.Goatee.Ui.Gtk.InfoLine (InfoLine)
import Graphics.UI.Gtk (
  Action,
  Menu,
  Packing (PackGrow, PackNatural),
  Window,
  actionCreateMenuItem, actionCreateToolItem, actionGroupGetAction,
  boxPackStart,
  containerAdd,
  deleteEvent,
  hPanedNew,
  menuBarNew, menuItemNewWithMnemonic, menuItemSetSubmenu, menuNew, menuShellAppend,
  notebookAppendPage, notebookNew,
  on,
  panedAdd1, panedAdd2, panedSetPosition,
  separatorMenuItemNew, separatorToolItemNew,
  toolbarNew,
  vBoxNew,
  widgetDestroy, widgetGrabFocus, widgetShowAll,
  windowNew, windowSetDefaultSize, windowSetTitle,
  )
import System.IO (hPutStrLn, stderr)

data MainWindow ui = MainWindow { myUi :: ui
                                , myWindow :: Window
                                , myActions :: Actions ui
                                , myGamePropertiesPanel :: GamePropertiesPanel ui
                                , myGoban :: Goban ui
                                , myInfoLine :: InfoLine ui
                                , myDirtyChangedHandler :: IORef (Maybe Registration)
                                , myFilePathChangedHandler :: IORef (Maybe Registration)
                                }

create :: UiCtrl ui => ui -> IO (MainWindow ui)
create ui = do
  window <- windowNew
  windowSetDefaultSize window 640 480

  actions <- Actions.create ui

  boardBox <- vBoxNew False 0
  containerAdd window boardBox

  menuBar <- menuBarNew
  boxPackStart boardBox menuBar PackNatural 0

  menuFile <- menuItemNewWithMnemonic "_File"
  menuFileMenu <- menuNew
  menuShellAppend menuBar menuFile
  menuItemSetSubmenu menuFile menuFileMenu

  menuFileNew <- menuItemNewWithMnemonic "_New file"
  menuFileNewMenu <- menuNew
  menuItemSetSubmenu menuFileNew menuFileNewMenu
  addActionsToMenu menuFileNewMenu actions
    [ Actions.myFileNew9Action
    , Actions.myFileNew13Action
    , Actions.myFileNew19Action
    , Actions.myFileNewCustomAction
    ]

  containerAdd menuFileMenu menuFileNew
  addActionsToMenu menuFileMenu actions
    [ Actions.myFileOpenAction
    , Actions.myFileSaveAction
    , Actions.myFileSaveAsAction
    ]
  containerAdd menuFileMenu =<< separatorMenuItemNew
  addActionsToMenu menuFileMenu actions
    [ Actions.myFileCloseAction
    , Actions.myFileQuitAction
    ]

  menuGame <- menuItemNewWithMnemonic "_Game"
  menuGameMenu <- menuNew
  menuShellAppend menuBar menuGame
  menuItemSetSubmenu menuGame menuGameMenu
  addActionsToMenu menuGameMenu actions
    [ Actions.myGamePassAction
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

  menuView <- menuItemNewWithMnemonic "_View"
  menuViewMenu <- menuNew
  menuShellAppend menuBar menuView
  menuItemSetSubmenu menuView menuViewMenu

  menuViewVariations <- menuItemNewWithMnemonic "_Variations"
  menuViewVariationsMenu <- menuNew
  containerAdd menuViewMenu menuViewVariations
  menuItemSetSubmenu menuViewVariations menuViewVariationsMenu

  containerAdd menuViewVariationsMenu =<<
    actionCreateMenuItem (Actions.myViewVariationsChildAction actions)
  containerAdd menuViewVariationsMenu =<<
    actionCreateMenuItem (Actions.myViewVariationsCurrentAction actions)
  containerAdd menuViewVariationsMenu =<< separatorMenuItemNew
  containerAdd menuViewVariationsMenu =<<
    actionCreateMenuItem (Actions.myViewVariationsBoardMarkupOnAction actions)
  containerAdd menuViewVariationsMenu =<<
    actionCreateMenuItem (Actions.myViewVariationsBoardMarkupOffAction actions)

  containerAdd menuViewMenu =<<
    actionCreateMenuItem (Actions.myViewHighlightCurrentMovesAction actions)

  menuHelp <- menuItemNewWithMnemonic "_Help"
  menuHelpMenu <- menuNew
  menuShellAppend menuBar menuHelp
  menuItemSetSubmenu menuHelp menuHelpMenu
  addActionsToMenu menuHelpMenu actions [Actions.myHelpAboutAction]

  infoLine <- InfoLine.create ui
  boxPackStart boardBox (InfoLine.myLabel infoLine) PackNatural 0

  hPaned <- hPanedNew
  boxPackStart boardBox hPaned PackGrow 0

  --hPanedMin <- get hPaned panedMinPosition
  --hPanedMax <- get hPaned panedMaxPosition
  --putStrLn $ "Paned position in [" ++ show hPanedMin ++ ", " ++ show hPanedMax ++ "]."
  -- TODO Don't hard-code the pane width.
  panedSetPosition hPaned 400 -- (truncate (fromIntegral hPanedMax * 0.8))

  goban <- Goban.create ui
  panedAdd1 hPaned $ Goban.myWidget goban

  controlsBook <- notebookNew
  panedAdd2 hPaned controlsBook

  gamePropertiesPanel <- GamePropertiesPanel.create ui
  notebookAppendPage controlsBook (GamePropertiesPanel.myWidget gamePropertiesPanel) "Properties"

  dirtyChangedHandler <- newIORef Nothing
  filePathChangedHandler <- newIORef Nothing

  let me = MainWindow { myUi = ui
                      , myWindow = window
                      , myActions = actions
                      , myGamePropertiesPanel = gamePropertiesPanel
                      , myGoban = goban
                      , myInfoLine = infoLine
                      , myDirtyChangedHandler = dirtyChangedHandler
                      , myFilePathChangedHandler = filePathChangedHandler
                      }

  initialize me

  on window deleteEvent $ liftIO $ do
    fileClose ui
    return True

  widgetGrabFocus $ Goban.myWidget goban

  return me

-- | Initialization that must be done after the 'UiCtrl' is available.
initialize :: UiCtrl ui => MainWindow ui -> IO ()
initialize me = do
  let ui = myUi me

  writeIORef (myDirtyChangedHandler me) =<<
    liftM Just (registerDirtyChangedHandler ui "MainWindow" False $ \_ -> updateWindowTitle me)
  writeIORef (myFilePathChangedHandler me) =<<
    liftM Just (registerFilePathChangedHandler ui "MainWindow" True $ \_ _ -> updateWindowTitle me)

destroy :: UiCtrl ui => MainWindow ui -> IO ()
destroy me = do
  Actions.destroy $ myActions me
  GamePropertiesPanel.destroy $ myGamePropertiesPanel me
  Goban.destroy $ myGoban me
  InfoLine.destroy $ myInfoLine me

  let ui = myUi me
  F.mapM_ (unregisterDirtyChangedHandler ui) =<< readIORef (myDirtyChangedHandler me)
  F.mapM_ (unregisterFilePathChangedHandler ui) =<< readIORef (myFilePathChangedHandler me)

  -- A main window owns a UI controller.  Once a main window is destroyed,
  -- there should be no remaining handlers registered.
  -- TODO Revisit this if we have multiple windows under a controller.
  registeredHandlers ui >>= \handlers -> unless (null handlers) $ hPutStrLn stderr $
    "MainWindow.destroy: Warning, there are still handler(s) registered:" ++
    concatMap (\handler -> "\n- " ++ show handler) handlers
  registeredDirtyChangedHandlers ui >>= \handlers -> unless (null handlers) $ hPutStrLn stderr $
    "MainWindow.destroy: Warning, there are still dirty changed handler(s) registered:" ++
    concatMap (\handler -> "\n- " ++ show handler) handlers
  registeredFilePathChangedHandlers ui >>= \handlers -> unless (null handlers) $ hPutStrLn stderr $
    "MainWindow.destroy: Warning, there are still file path changed handler(s) registered:" ++
    concatMap (\handler -> "\n- " ++ show handler) handlers
  registeredModesChangedHandlers ui >>= \handlers -> unless (null handlers) $ hPutStrLn stderr $
    "MainWindow.destroy: Warning, there are still modes changed handler(s) registered:" ++
    concatMap (\handler -> "\n- " ++ show handler) handlers

  widgetDestroy $ myWindow me

-- | Makes a 'MainWindow' visible.
display :: MainWindow ui -> IO ()
display = widgetShowAll . myWindow

-- | Takes a object of generic type, extracts a bunch of actions from it, and
-- adds those actions to a menu.
addActionsToMenu :: Menu -> a -> [a -> Action] -> IO ()
addActionsToMenu menu actions accessors =
  forM_ accessors $ \accessor ->
  containerAdd menu =<< actionCreateMenuItem (accessor actions)

updateWindowTitle :: UiCtrl ui => MainWindow ui -> IO ()
updateWindowTitle me = do
  let ui = myUi me
  fileName <- getFileName ui
  dirty <- getDirty ui
  let title = fileName ++ " - Goatee"
      addDirty = if dirty then ('*':) else id
  windowSetTitle (myWindow me) $ addDirty title
