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

import Control.Applicative ((<$>))
import Control.Monad (void)
import Game.Goatee.Common
import Game.Goatee.Lib.Board
import qualified Game.Goatee.Lib.Monad as Monad
import Game.Goatee.Lib.Monad (
  AnyEvent (..), getCursor, modifyPropertyString, navigationEvent, propertiesModifiedEvent,
  )
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
  hBoxNew,
  on,
  scrolledWindowNew, scrolledWindowSetPolicy,
  textViewNew, textViewSetWrapMode,
  toWidget,
  vBoxNew,
  )

data PlayPanel ui = PlayPanel
  { myUi :: ui
  , myState :: ViewState
  , myWidget :: Widget
  , myComment :: TextView
  , myCommentSetter :: String -> IO ()
  }

instance UiCtrl go ui => UiView go ui (PlayPanel ui) where
  viewName = const "PlayPanel"
  viewCtrl = myUi
  viewState = myState
  viewUpdate = update

create :: UiCtrl go ui => ui -> Actions ui -> IO (PlayPanel ui)
create ui actions = do
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
    whileM ((> 0) . length . cursorChildren <$> getCursor) $ Monad.goDown 0

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
    doUiGo ui $ modifyPropertyString propertyC $ const value

  state <- viewStateNew

  let me = PlayPanel
        { myUi = ui
        , myState = state
        , myWidget = toWidget box
        , myComment = comment
        , myCommentSetter = commentSetter
        }

  initialize me
  return me

initialize :: UiCtrl go ui => PlayPanel ui -> IO ()
initialize me = do
  register me
    [ AnyEvent navigationEvent
    , AnyEvent propertiesModifiedEvent
    ]
  viewUpdate me

destroy :: UiCtrl go ui => PlayPanel ui -> IO ()
destroy = viewDestroy

update :: UiCtrl go ui => PlayPanel ui -> IO ()
update me =
  readCursor (myUi me) >>=
  myCommentSetter me . maybe "" fromText . findPropertyValue propertyC . cursorNode
