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

module Game.Goatee.Ui.Gtk.PlayPanel (
  PlayPanel,
  create,
  destroy,
  myWidget,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, void, when)
import Data.Foldable (forM_, mapM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Game.Goatee.Common
import Game.Goatee.Lib.Board
import qualified Game.Goatee.Lib.Monad as Monad
import Game.Goatee.Lib.Monad (
  AnyEvent (..), getCursor, modifyPropertyString, navigationEvent, propertiesModifiedEvent,
  )
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Utils
import Graphics.UI.Gtk (
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic),
  TextView,
  Widget,
  WrapMode (WrapWord),
  afterShow,
  boxPackStart,
  buttonActivated, buttonNewWithLabel,
  containerAdd,
  hBoxNew,
  on,
  scrolledWindowNew, scrolledWindowSetPolicy,
  textViewNew, textViewSetWrapMode,
  toWidget,
  vBoxNew,
  widgetHide, widgetShow,
  )
import Prelude hiding (mapM_)

data PlayPanel ui = PlayPanel
  { myUi :: ui
  , myState :: ViewState
  , myWidget :: Widget
  , myComment :: TextView
  , myCommentSetter :: String -> IO ()
  , myModesChangedHandler :: IORef (Maybe Registration)
  }

instance UiCtrl go ui => UiView go ui (PlayPanel ui) where
  viewName = const "PlayPanel"
  viewCtrl = myUi
  viewState = myState
  viewUpdate = update

create :: UiCtrl go ui => ui -> IO (PlayPanel ui)
create ui = do
  box <- vBoxNew False 0

  navBox <- hBoxNew True 0
  boxPackStart box navBox PackNatural 0
  startButton <- buttonNewWithLabel "<<"
  prevButton <- buttonNewWithLabel "<"
  nextButton <- buttonNewWithLabel ">"
  endButton <- buttonNewWithLabel ">>"
  mapM_ (\b -> boxPackStart navBox b PackGrow 0)
    [startButton, prevButton, nextButton, endButton]
  on startButton buttonActivated $ doUiGo ui Monad.goToRoot
  on prevButton buttonActivated $ void $ goUp ui
  on nextButton buttonActivated $ void $ goDown ui 0
  on endButton buttonActivated $ doUiGo ui $
    -- TODO This is duplicated in Goban's End keybinding.  These will be a lot
    -- cleaner when the Monad go* functions return bools (bug #43149).
    whileM (not . null . cursorChildren <$> getCursor) $ Monad.goDown 0

  -- Add the widgets of all of the tools.  Deduplicate the widgets so those that
  -- are shared between tools only get added once; GTK+ doesn't like having a
  -- widget added multiple times.
  toolWidgets <- fmap catMaybes $
                 forM [minBound..] $
                 fmap (\(AnyTool tool) -> toolPanelWidget tool) .
                 findTool ui
  forM_ (Set.toList $ Set.fromList toolWidgets) $ \widget ->
    boxPackStart box widget PackNatural 0

  comment <- textViewNew
  textViewSetWrapMode comment WrapWord
  commentScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy commentScroll PolicyAutomatic PolicyAutomatic
  containerAdd commentScroll comment
  boxPackStart box commentScroll PackGrow 0

  commentSetter <- textViewConfigure comment $ \value ->
    doUiGo ui $ modifyPropertyString propertyC $ const value

  state <- viewStateNew
  modesChangedHandler <- newIORef Nothing

  let me = PlayPanel
        { myUi = ui
        , myState = state
        , myWidget = toWidget box
        , myComment = comment
        , myCommentSetter = commentSetter
        , myModesChangedHandler = modesChangedHandler
        }

  -- After the panel is shown, we only want the tool widget for the active tool
  -- to be visible.
  afterShow (myWidget me) $ updateVisibleToolWidget me

  initialize me
  return me

initialize :: UiCtrl go ui => PlayPanel ui -> IO ()
initialize me = do
  let ui = myUi me

  register me
    [ AnyEvent navigationEvent
    , AnyEvent propertiesModifiedEvent
    ]

  writeIORef (myModesChangedHandler me) =<<
    fmap Just (registerModesChangedHandler ui "PlayPanel" $ checkForToolChange me)

  viewUpdate me

destroy :: UiCtrl go ui => PlayPanel ui -> IO ()
destroy me = do
  let ui = myUi me
  mapM_ (unregisterModesChangedHandler ui) =<< readIORef (myModesChangedHandler me)
  viewDestroy me

update :: UiCtrl go ui => PlayPanel ui -> IO ()
update me =
  readCursor (myUi me) >>=
  myCommentSetter me . maybe "" fromText . findPropertyValue propertyC . cursorNode

-- | Updates the visibility of all tool widgets, hiding all widgets of inactive
-- tools and showing the widget of the active tool.
updateVisibleToolWidget :: UiCtrl go ui => PlayPanel ui -> IO ()
updateVisibleToolWidget me = do
  let ui = myUi me
  activeToolType <- (\(AnyTool tool) -> toolType tool) <$> readTool ui
  forM_ [minBound..] $ \toolType ->
    findTool ui toolType >>= \(AnyTool tool) ->
    forM_ (toolPanelWidget tool) $ \widget ->
    (if toolType == activeToolType then widgetShow else widgetHide) widget

-- | Checks for a change in active tool between the two modes; if one is found,
-- the deactivating tool's widget is hidden and the activating tool's widget is
-- shown.
checkForToolChange :: UiCtrl go ui => PlayPanel ui -> UiModes -> UiModes -> IO ()
checkForToolChange me oldModes newModes = do
  let ui = myUi me
      oldTool = uiToolType oldModes
      newTool = uiToolType newModes
  when (newTool /= oldTool) $ do
    findTool ui oldTool >>= \(AnyTool tool) -> mapM_ widgetHide $ toolPanelWidget tool
    findTool ui newTool >>= \(AnyTool tool) -> mapM_ widgetShow $ toolPanelWidget tool
