-- This file is part of Goatee.
--
-- Copyright 2014-2015 Bryan Gardiner
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
import Control.Monad (forM, forM_, join, liftM)
import Control.Monad.Trans (liftIO)
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Game.Goatee.Ui.Gtk.Common
import qualified Game.Goatee.Ui.Gtk.Actions as Actions
import Game.Goatee.Ui.Gtk.Actions (Actions)
import qualified Game.Goatee.Ui.Gtk.GamePropertiesPanel as GamePropertiesPanel
import Game.Goatee.Ui.Gtk.GamePropertiesPanel (GamePropertiesPanel)
import qualified Game.Goatee.Ui.Gtk.Goban as Goban
import Game.Goatee.Ui.Gtk.Goban (Goban)
import qualified Game.Goatee.Ui.Gtk.InfoLine as InfoLine
import Game.Goatee.Ui.Gtk.InfoLine (InfoLine)
import qualified Game.Goatee.Ui.Gtk.NodePropertiesPanel as NodePropertiesPanel
import Game.Goatee.Ui.Gtk.NodePropertiesPanel (NodePropertiesPanel)
import qualified Game.Goatee.Ui.Gtk.PlayPanel as PlayPanel
import Game.Goatee.Ui.Gtk.PlayPanel (PlayPanel)
import Graphics.UI.Gtk (
  Action,
  Menu,
  Packing (PackGrow, PackNatural),
  Window,
  actionCreateMenuItem, actionCreateToolItem, actionGroupGetAction,
  boxPackStart,
  containerAdd,
  deleteEvent,
  eventKeyName, eventModifier,
  hPanedNew,
  keyPressEvent,
  menuBarNew, menuItemNewWithMnemonic, menuItemSetSubmenu, menuNew, menuShellAppend,
  notebookAppendPage, notebookNew,
  on,
  panedPack1, panedPack2, panedSetPosition,
  separatorMenuItemNew, separatorToolItemNew,
  toAction,
  toolbarNew,
  vBoxNew,
  widgetDestroy, widgetGrabFocus, widgetShowAll,
  windowNew, windowSetDefaultSize, windowSetTitle,
  )

data MainWindow ui = MainWindow
  { myUi :: ui
  , myWindow :: Window
  , myActions :: Actions ui
  , myInfoLine :: InfoLine ui
  , myGoban :: Goban ui
  , myPlayPanel :: PlayPanel ui
  , myGamePropertiesPanel :: GamePropertiesPanel ui
  , myNodePropertiesPanel :: NodePropertiesPanel ui
  , myDirtyChangedHandler :: IORef (Maybe Registration)
  , myFilePathChangedHandler :: IORef (Maybe Registration)
  }

create :: UiCtrl go ui => ui -> IO (MainWindow ui)
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

  menuEdit <- menuItemNewWithMnemonic "_Edit"
  menuEditMenu <- menuNew
  menuShellAppend menuBar menuEdit
  menuItemSetSubmenu menuEdit menuEditMenu
  addActionsToMenu menuEditMenu actions
    [ Actions.myEditCutNodeAction
    , Actions.myEditCopyNodeAction
    , Actions.myEditPasteNodeAction
    ]

  menuGame <- menuItemNewWithMnemonic "_Game"
  menuGameMenu <- menuNew
  menuShellAppend menuBar menuGame
  menuItemSetSubmenu menuGame menuGameMenu
  addActionsToMenu menuGameMenu actions
    [ Actions.myGamePassAction
    ]

  menuGameVariations <- menuItemNewWithMnemonic "_Variations"
  menuGameVariationsMenu <- menuNew
  containerAdd menuGameMenu menuGameVariations
  menuItemSetSubmenu menuGameVariations menuGameVariationsMenu

  containerAdd menuGameVariationsMenu =<<
    actionCreateMenuItem (Actions.myGameVariationsChildAction actions)
  containerAdd menuGameVariationsMenu =<<
    actionCreateMenuItem (Actions.myGameVariationsCurrentAction actions)
  containerAdd menuGameVariationsMenu =<< separatorMenuItemNew
  containerAdd menuGameVariationsMenu =<<
    actionCreateMenuItem (Actions.myGameVariationsBoardMarkupOnAction actions)
  containerAdd menuGameVariationsMenu =<<
    actionCreateMenuItem (Actions.myGameVariationsBoardMarkupOffAction actions)

  menuTool <- menuItemNewWithMnemonic "_Tool"
  menuToolMenu <- menuNew
  menuShellAppend menuBar menuTool
  menuItemSetSubmenu menuTool menuToolMenu

  toolbar <- toolbarNew
  boxPackStart boardBox toolbar PackNatural 0

  let addToolSeparator = do
        menuSep <- separatorMenuItemNew
        toolSep <- separatorToolItemNew
        containerAdd menuToolMenu menuSep
        containerAdd toolbar toolSep
      addTool (AnyTool tool) = do
        action <- fromMaybe (error $ "No action for tool with type: " ++ show (toolType tool)) <$>
                  actionGroupGetAction (Actions.myToolActions actions) (show (toolType tool))
        menuItem <- actionCreateMenuItem action
        toolItem <- actionCreateToolItem action
        containerAdd menuToolMenu menuItem
        containerAdd toolbar toolItem
    in join $ fmap (sequence_ . intersperse addToolSeparator . catMaybes) $
       forM toolOrdering $ \toolGroup -> do
         tools <- filter (\(AnyTool tool) -> toolIsImplemented tool) <$>
                  mapM (findTool ui) toolGroup
         return $ if null tools
                  then Nothing
                  else Just $ mapM_ addTool tools

  menuView <- menuItemNewWithMnemonic "_View"
  menuViewMenu <- menuNew
  menuShellAppend menuBar menuView
  menuItemSetSubmenu menuView menuViewMenu

  containerAdd menuViewMenu =<<
    actionCreateMenuItem (Actions.myViewHighlightCurrentMovesAction actions)

  menuViewStones <- menuItemNewWithMnemonic "_Stones"
  menuViewStonesMenu <- menuNew
  containerAdd menuViewMenu menuViewStones
  menuItemSetSubmenu menuViewStones menuViewStonesMenu

  addActionsToMenu menuViewStonesMenu actions
    [ toAction . Actions.myViewStonesRegularModeAction
    , toAction . Actions.myViewStonesOneColorModeAction
    , toAction . Actions.myViewStonesBlindModeAction
    ]

  menuHelp <- menuItemNewWithMnemonic "_Help"
  menuHelpMenu <- menuNew
  menuShellAppend menuBar menuHelp
  menuItemSetSubmenu menuHelp menuHelpMenu
  addActionsToMenu menuHelpMenu actions [Actions.myHelpAboutAction]

  infoLine <- InfoLine.create ui
  boxPackStart boardBox (InfoLine.myWidget infoLine) PackNatural 0

  hPaned <- hPanedNew
  boxPackStart boardBox hPaned PackGrow 0

  --hPanedMin <- get hPaned panedMinPosition
  --hPanedMax <- get hPaned panedMaxPosition
  --putStrLn $ "Paned position in [" ++ show hPanedMin ++ ", " ++ show hPanedMax ++ "]."
  -- TODO Don't hard-code the pane width.
  panedSetPosition hPaned 400 -- (truncate (fromIntegral hPanedMax * 0.8))

  goban <- Goban.create ui
  panedPack1 hPaned (Goban.myWidget goban) True True

  controlsBook <- notebookNew
  panedPack2 hPaned controlsBook False True

  playPanel <- PlayPanel.create ui
  gamePropertiesPanel <- GamePropertiesPanel.create ui
  nodePropertiesPanel <- NodePropertiesPanel.create ui
  notebookAppendPage controlsBook (PlayPanel.myWidget playPanel) "Play"
  notebookAppendPage controlsBook (GamePropertiesPanel.myWidget gamePropertiesPanel) "Game"
  notebookAppendPage controlsBook (NodePropertiesPanel.myWidget nodePropertiesPanel) "Properties"

  dirtyChangedHandler <- newIORef Nothing
  filePathChangedHandler <- newIORef Nothing

  let me = MainWindow { myUi = ui
                      , myWindow = window
                      , myActions = actions
                      , myInfoLine = infoLine
                      , myGoban = goban
                      , myPlayPanel = playPanel
                      , myGamePropertiesPanel = gamePropertiesPanel
                      , myNodePropertiesPanel = nodePropertiesPanel
                      , myDirtyChangedHandler = dirtyChangedHandler
                      , myFilePathChangedHandler = filePathChangedHandler
                      }

  initialize me

  on window keyPressEvent $ do
    key <- eventKeyName
    mods <- eventModifier
    let km = (key, mods)
    case km of
      -- Escape focuses the goban.
      ("Escape", []) -> do
        liftIO $ widgetGrabFocus $ Goban.myWidget goban
        return True
      _ -> return False

  on window deleteEvent $ liftIO $ do
    fileClose ui
    return True

  widgetGrabFocus $ Goban.myWidget goban

  return me

-- | Initialization that must be done after the 'UiCtrl' is available.
initialize :: UiCtrl go ui => MainWindow ui -> IO ()
initialize me = do
  let ui = myUi me

  writeIORef (myDirtyChangedHandler me) =<<
    liftM Just (registerDirtyChangedHandler ui "MainWindow" False $ \_ -> updateWindowTitle me)
  writeIORef (myFilePathChangedHandler me) =<<
    liftM Just (registerFilePathChangedHandler ui "MainWindow" True $ \_ _ -> updateWindowTitle me)

destroy :: UiCtrl go ui => MainWindow ui -> IO ()
destroy me = do
  Actions.destroy $ myActions me
  InfoLine.destroy $ myInfoLine me
  Goban.destroy $ myGoban me
  PlayPanel.destroy $ myPlayPanel me
  GamePropertiesPanel.destroy $ myGamePropertiesPanel me
  NodePropertiesPanel.destroy $ myNodePropertiesPanel me

  let ui = myUi me
  F.mapM_ (unregisterDirtyChangedHandler ui) =<< readIORef (myDirtyChangedHandler me)
  F.mapM_ (unregisterFilePathChangedHandler ui) =<< readIORef (myFilePathChangedHandler me)

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

updateWindowTitle :: UiCtrl go ui => MainWindow ui -> IO ()
updateWindowTitle me = do
  let ui = myUi me
  fileName <- getFileName ui
  dirty <- getDirty ui
  let title = fileName ++ " - Goatee"
      addDirty = if dirty then ('*':) else id
  windowSetTitle (myWindow me) $ addDirty title
