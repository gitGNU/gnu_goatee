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

module Game.Goatee.Ui.Gtk.PlayPanel (
  PlayPanel,
  create,
  destroy,
  myWidget,
  ) where

import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad hiding (on)
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import qualified Game.Goatee.Ui.Gtk.Actions as Actions
import Game.Goatee.Ui.Gtk.Actions (Actions)
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Utils
import Graphics.UI.Gtk (
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic),
  TextView,
  Widget,
  WrapMode (WrapWord),
  actionActivate,
  boxPackStart,
  buttonActivated, buttonNewWithLabel,
  containerAdd,
  on,
  scrolledWindowNew, scrolledWindowSetPolicy,
  textViewNew, textViewSetWrapMode,
  toWidget,
  vBoxNew,
  )

data PlayPanel ui = PlayPanel {
  myUi :: ui
  , myRegistrations :: ViewRegistrations
  , myWidget :: Widget
  , myComment :: TextView
  , myCommentSetter :: String -> IO ()
  }

instance UiCtrl ui => UiView (PlayPanel ui) ui where
  viewName = const "PlayPanel"
  viewCtrl = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => ui -> Actions ui -> IO (PlayPanel ui)
create ui actions = do
  box <- vBoxNew False 0

  passButton <- buttonNewWithLabel "Pass"
  boxPackStart box passButton PackNatural 0
  on passButton buttonActivated $ actionActivate $ Actions.myGamePassAction actions

  comment <- textViewNew
  textViewSetWrapMode comment WrapWord
  commentScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy commentScroll PolicyAutomatic PolicyAutomatic
  containerAdd commentScroll comment
  boxPackStart box commentScroll PackGrow 0

  commentSetter <- textViewConfigure comment $ \value ->
    runUiGo ui $ modifyPropertyString propertyC $ const value

  registrations <- viewNewRegistrations

  let me = PlayPanel {
        myUi = ui
        , myRegistrations = registrations
        , myWidget = toWidget box
        , myComment = comment
        , myCommentSetter = commentSetter
        }

  initialize me
  return me

initialize :: UiCtrl ui => PlayPanel ui -> IO ()
initialize me = do
  let ui = myUi me

  -- Watch for node changes.
  let onNodeChange = do cursor <- getCursor
                        afterGo $ updateFromCursor me cursor
  viewRegister me navigationEvent $ const onNodeChange
  viewRegister me propertiesModifiedEvent $ const $ const onNodeChange

  updateFromCursor me =<< readCursor ui

destroy :: UiCtrl ui => PlayPanel ui -> IO ()
destroy = viewUnregisterAll

updateFromCursor :: UiCtrl ui => PlayPanel ui -> Cursor -> IO ()
updateFromCursor me cursor =
  myCommentSetter me $ maybe "" fromText $ findPropertyValue propertyC $ cursorNode cursor
