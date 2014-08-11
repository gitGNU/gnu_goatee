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

import Control.Arrow ((+++))
import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad (forM_, unless, when)
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad hiding (on)
import Game.Goatee.Lib.Parser
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Renderer
import Game.Goatee.Lib.Renderer.Tree
import Game.Goatee.Ui.Gtk.Common
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  ListStore,
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic),
  ResponseId (ResponseCancel, ResponseOk),
  TreeViewColumnSizing (TreeViewColumnAutosize),
  Widget,
  WrapMode (WrapWord),
  boxPackStart,
  bufferChanged,
  buttonActivated, buttonNewWithLabel,
  cellLayoutSetAttributes, cellRendererTextNew, cellText,
  containerAdd,
  dialogAddButton, dialogGetUpper, dialogNew, dialogRun, dialogSetDefaultResponse,
  get,
  hBoxNew,
  labelNew, labelText,
  listStoreAppend, listStoreClear, listStoreNew, listStoreToList,
  on,
  scrolledWindowNew, scrolledWindowSetPolicy,
  set,
  stockAdd, stockCancel, stockEdit,
  textBufferText,
  textViewGetBuffer, textViewNew, textViewSetWrapMode,
  toWidget,
  treeSelectionGetSelectedRows,
  treeViewAppendColumn, treeViewColumnNew, treeViewColumnSizing, treeViewColumnPackStart,
  treeViewColumnTitle, treeViewGetSelection, treeViewNewWithModel,
  vBoxNew,
  widgetDestroy, widgetSetSensitive, widgetShowAll,
  windowSetDefaultSize, windowSetTitle,
  )
import Text.ParserCombinators.Parsec (eof, parse, spaces)

data NodePropertiesPanel ui = NodePropertiesPanel {
  myUi :: ui
  , myRegistrations :: ViewRegistrations
  , myWidget :: Widget
  , myModel :: ListStore Property
  , myModelProperties :: IORef [Property]
    -- ^ A list of properties in the same order as the rows in 'myModel'.
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
  modelProperties <- newIORef []
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
  selection <- treeViewGetSelection view
  viewScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy viewScroll PolicyAutomatic PolicyAutomatic
  containerAdd viewScroll view
  boxPackStart vBox viewScroll PackGrow 0

  registrations <- viewNewRegistrations

  let me = NodePropertiesPanel { myUi = ui
                               , myRegistrations = registrations
                               , myWidget = toWidget vBox
                               , myModel = model
                               , myModelProperties = modelProperties
                               }

  on addButton buttonActivated $ do
    maybeProperty <- runPropertyEditDialog "Add property" stockAdd Nothing
    Foldable.forM_ maybeProperty $ runUiGo ui . putProperty

  on editButton buttonActivated $ do
    rows <- map head <$> treeSelectionGetSelectedRows selection
    case rows of
      [] -> return ()
      row:_ -> do
        oldProperty <- (!! row) <$> readIORef modelProperties
        maybeNewProperty <- runPropertyEditDialog "Edit property" stockEdit $ Just oldProperty
        case maybeNewProperty of
          Nothing -> return ()
          Just newProperty -> runUiGo ui $ do
            -- Need to delete the old property when the property type has
            -- changed.
            deleteProperty oldProperty
            putProperty newProperty

  on deleteButton buttonActivated $ do
    rows <- map head <$> treeSelectionGetSelectedRows selection
    properties <- readIORef modelProperties
    unless (null rows) $
      runUiGo ui $ forM_ rows $ deleteProperty . (properties !!)

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
      modelProperties = myModelProperties me
  oldProperties <- listStoreToList model
  newProperties <- sortBy (compare `Function.on` propertyName) .
                   cursorProperties <$>
                   readCursor ui
  when (newProperties /= oldProperties) $ do
    listStoreClear model
    forM_ newProperties $ listStoreAppend model
    writeIORef modelProperties newProperties

-- | Opens a dialog for editing a property in serialized SGF format.  The
-- initial property may be absent, in which case the input box will start empty.
-- This function will eiter return 'Nothing' if the edit was cancelled, or
-- 'Just' a property if the user entered a valid property and chose to accept.
runPropertyEditDialog :: String -- ^ Dialog title.
                      -> String -- ^ Accept button label.
                      -> Maybe Property -- ^ Initial property value.
                      -> IO (Maybe Property)
runPropertyEditDialog dialogTitle acceptButtonLabel initialProperty = do
  dialog <- dialogNew
  windowSetTitle dialog dialogTitle
  windowSetDefaultSize dialog 500 225
  upper <- dialogGetUpper dialog

  helpLabel <- labelNew $ Just "Enter a property in SGF notation."
  boxPackStart upper helpLabel PackNatural 0

  textView <- textViewNew
  textViewSetWrapMode textView WrapWord
  textScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy textScroll PolicyAutomatic PolicyAutomatic
  containerAdd textScroll textView
  boxPackStart upper textScroll PackGrow 0
  textBuffer <- textViewGetBuffer textView

  errorLabel <- labelNew Nothing
  boxPackStart upper errorLabel PackNatural 0

  dialogAddButton dialog stockCancel ResponseCancel
  acceptButton <- dialogAddButton dialog acceptButtonLabel ResponseOk
  dialogSetDefaultResponse dialog ResponseOk

  -- Either a parse error (an empty string if the input box is empty) or a
  -- parsed property.
  currentState <- newIORef (Left "" :: Either String Property)

  let setState errorOrProperty =
        case errorOrProperty of
          Left errorMsg -> do
            set errorLabel [labelText := errorMsg]
            writeIORef currentState $ Left errorMsg
            widgetSetSensitive acceptButton False
          Right property -> do
            set errorLabel [labelText := ""]
            writeIORef currentState $ Right property
            widgetSetSensitive acceptButton True
      parseInput = do
        text <- get textBuffer textBufferText
        setState $
          if null text
          then Left ""
          else (show +++ id) $
               parse (spaces *> propertyParser <* spaces <* eof) "<property>" text

  set textBuffer [textBufferText :=
                  maybe "" (either (const "") id . runRender . renderProperty) initialProperty]
  on textBuffer bufferChanged parseInput
  parseInput

  widgetShowAll dialog
  response <- dialogRun dialog
  widgetDestroy dialog
  case response of
    ResponseOk -> do
      finalState <- readIORef currentState
      case finalState of
        Left _ -> return Nothing
        Right property -> return $ Just property
    _ -> return Nothing
