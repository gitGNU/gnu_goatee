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

import Control.Monad (when)
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad hiding (on)
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.Tree
import Game.Goatee.Sgf.Types
import qualified Game.Goatee.Ui.Gtk.Actions as Actions
import Game.Goatee.Ui.Gtk.Actions (Actions)
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Latch
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic),
  TextBuffer,
  TextView,
  Widget,
  WrapMode (WrapWord),
  actionActivate,
  boxPackStart,
  bufferChanged,
  buttonActivated, buttonNewWithLabel,
  containerAdd,
  get,
  on,
  scrolledWindowNew, scrolledWindowSetPolicy,
  set,
  textBufferText,
  textViewGetBuffer, textViewNew, textViewSetWrapMode,
  toWidget,
  vBoxNew,
  )

data PlayPanel ui = PlayPanel {
  myUi :: ui
  , myRegistrations :: ViewRegistrations
  , myWidget :: Widget
  , myComment :: TextView
  , myCommentLatch :: Latch
    -- ^ When a 'TextBuffer' is programatically assigned to, two change events
    -- are fired, one to delete the old text and one to insert the new text.  We
    -- don't want to handle the intermediate value by writing it back to the
    -- model because this triggers dirtyness.  So we hold this latch on while we
    -- are doing a model-to-view update in order to avoid this problem.
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
  commentLatch <- newLatch
  textViewSetWrapMode comment WrapWord
  commentScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy commentScroll PolicyAutomatic PolicyAutomatic
  containerAdd commentScroll comment
  boxPackStart box commentScroll PackGrow 0

  registrations <- viewNewRegistrations

  let me = PlayPanel {
        myUi = ui
        , myRegistrations = registrations
        , myWidget = toWidget box
        , myComment = comment
        , myCommentLatch = commentLatch
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

  commentBuffer <- textViewGetBuffer $ myComment me
  on commentBuffer bufferChanged $ handleCommentBufferChanged me commentBuffer
  return ()

destroy :: UiCtrl ui => PlayPanel ui -> IO ()
destroy = viewUnregisterAll

updateFromCursor :: UiCtrl ui => PlayPanel ui -> Cursor -> IO ()
updateFromCursor me cursor = do
  let newText = maybe "" fromText $ findPropertyValue propertyC $ cursorNode cursor
  buffer <- textViewGetBuffer $ myComment me
  oldText <- get buffer textBufferText
  when (oldText /= newText) $ do
    withLatchOn (myCommentLatch me) $ set buffer [textBufferText := newText]
    -- It's not necessary to call this handler, but we do for consistency, since
    -- all other widgets currently behave this way (write back and forth until
    -- synchronized).
    handleCommentBufferChanged me buffer

handleCommentBufferChanged :: UiCtrl ui => PlayPanel ui -> TextBuffer -> IO ()
handleCommentBufferChanged me buffer =
  -- Don't push the new comment value back to the model if we're already
  -- updating the view from the model.
  whenLatchOff (myCommentLatch me) $ do
    newComment <- get buffer textBufferText
    runUiGo (myUi me) $ modifyPropertyString propertyC $ const newComment
