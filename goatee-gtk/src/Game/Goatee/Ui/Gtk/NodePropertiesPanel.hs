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

-- | A list widget that displays the current node's properties for viewing and editing.
module Game.Goatee.Ui.Gtk.NodePropertiesPanel (
  NodePropertiesPanel,
  create,
  destroy,
  myWidget,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.Renderer
import Game.Goatee.Ui.Gtk.Common
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  ListStore,
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic),
  TreeViewColumnSizing (TreeViewColumnAutosize),
  Widget,
  boxPackStart,
  buttonNewWithLabel,
  cellLayoutSetAttributes, cellRendererTextNew, cellText,
  containerAdd,
  hBoxNew,
  hButtonBoxNew,
  listStoreAppend, listStoreClear, listStoreNew, listStoreToList,
  scrolledWindowNew, scrolledWindowSetPolicy,
  set,
  toWidget,
  treeViewAppendColumn, treeViewColumnNew, treeViewColumnSizing, treeViewColumnPackStart,
  treeViewColumnTitle, treeViewNewWithModel,
  vBoxNew,
  )

data NodePropertiesPanel ui = NodePropertiesPanel {
  myUi :: ui
  , myRegistrations :: ViewRegistrations
  , myWidget :: Widget
  , myModel :: ListStore Property
  }

instance UiCtrl ui => UiView (NodePropertiesPanel ui) ui where
  viewName = const "NodePropertiesPanel"
  viewCtrl = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => ui -> IO (NodePropertiesPanel ui)
create ui = do
  vBox <- vBoxNew False 0

  buttonBox <- hBoxNew True 0
  boxPackStart vBox buttonBox PackNatural 0
  addButton <- buttonNewWithLabel "Add"
  editButton <- buttonNewWithLabel "Edit"
  deleteButton <- buttonNewWithLabel "Del"
  mapM_ (containerAdd buttonBox) [addButton, editButton, deleteButton]

  model <- listStoreNew []
  column <- treeViewColumnNew
  set column [treeViewColumnSizing := TreeViewColumnAutosize,
              treeViewColumnTitle := "Property"]
  renderer <- cellRendererTextNew
  treeViewColumnPackStart column renderer True
  cellLayoutSetAttributes column renderer model $ \property ->
    let name = propertyName property
        value = case runRender $ propertyValueRendererPretty property property of
          Left _ -> "(render error)" -- TODO Better error handling.
          Right result -> result
    in [cellText := name ++ " " ++ value]
  view <- treeViewNewWithModel model
  treeViewAppendColumn view column
  viewScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy viewScroll PolicyAutomatic PolicyAutomatic
  containerAdd viewScroll view
  boxPackStart vBox viewScroll PackGrow 0

  registrations <- viewNewRegistrations

  let me = NodePropertiesPanel { myUi = ui
                               , myRegistrations = registrations
                               , myWidget = toWidget vBox
                               , myModel = model
                               }

  let onChange = afterGo $ updateViewModel me
  viewRegister me propertiesModifiedEvent $ const $ const onChange
  viewRegister me navigationEvent $ const onChange

  updateViewModel me

  return me

destroy :: UiCtrl ui => NodePropertiesPanel ui -> IO ()
destroy = viewUnregisterAll

-- | Updates the 'ListStore' backing the view from the properties on the cursor.
updateViewModel :: UiCtrl ui => NodePropertiesPanel ui -> IO ()
updateViewModel me = do
  let ui = myUi me
      model = myModel me
  oldProperties <- listStoreToList model
  newProperties <- cursorProperties <$> readCursor ui
  when (newProperties /= oldProperties) $ do
    listStoreClear model
    forM_ newProperties $ listStoreAppend model
