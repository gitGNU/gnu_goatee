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

-- | GTK+ 'Action' definitions.
module Khumba.Goatee.Ui.Gtk.Actions (
  Actions,
  create,
  initialize,
  destruct,
  myFileNewAction,
  myFileOpenAction,
  myFileSaveAction,
  myFileSaveAsAction,
  myToolActions,
  myHelpAboutAction,
  ) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (void, when)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Graphics.UI.Gtk (
  Action,
  ActionGroup,
  AttrOp ((:=)),
  ButtonsType (ButtonsOk),
  FileChooserAction (FileChooserActionOpen, FileChooserActionSave),
  MessageType (MessageError),
  RadioActionEntry (
    RadioActionEntry,
    radioActionAccelerator, radioActionLabel, radioActionName, radioActionStockId,
    radioActionTooltip, radioActionValue
    ),
  ResponseId (ResponseCancel, ResponseOk),
  aboutDialogAuthors, aboutDialogCopyright, aboutDialogLicense, aboutDialogNew,
  aboutDialogProgramName, aboutDialogWebsite,
  actionActivate, actionActivated, actionGroupAddActionWithAccel, actionGroupAddRadioActions,
  actionGroupGetAction, actionGroupNew, actionNew,
  fileChooserAddFilter, fileChooserDialogNew, fileChooserGetFilename,
  dialogRun,
  messageDialogNew,
  on,
  radioActionGetCurrentValue,
  set,
  stockCancel, stockOpen, stockSave,
  widgetDestroy, widgetHide,
  )
import Khumba.Goatee.App
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Printer
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Ui.Gtk.Common
import qualified Paths_goatee as Paths

data Actions = Actions { myFileNewAction :: Action
                       , myFileOpenAction :: Action
                       , myFileSaveAction :: Action
                       , myFileSaveAsAction :: Action
                       , myToolActions :: ActionGroup
                       , myHelpAboutAction :: Action
                       }

create :: UiCtrl ui => UiRef ui -> IO Actions
create uiRef = do
  let tools = enumFrom minBound

  -- File actions.
  fileActions <- actionGroupNew "File"

  -- TODO Accelerators aren't working.
  fileNewAction <- actionNew "FileNew" "New file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNewAction $ Just "<Control>n"
  on fileNewAction actionActivated $ do
    ui <- readUiRef uiRef
    void $ openNewBoard (Just ui) Nothing

  fileOpenAction <- actionNew "FileOpen" "Open file..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileOpenAction $ Just "<Control>o"
  on fileOpenAction actionActivated $ fileOpen uiRef

  fileSaveAsAction <- actionNew "FileSaveAs" "Save file as..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAsAction $ Just "<Control><Shift>s"
  on fileSaveAsAction actionActivated $ fileSaveAs uiRef

  fileSaveAction <- actionNew "FileSave" "Save file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAction $ Just "<Control>s"
  on fileSaveAction actionActivated $ fileSave uiRef

  -- Tool actions.
  toolActions <- actionGroupNew "Tools"
  actionGroupAddRadioActions toolActions
    (flip map tools $ \tool ->
      RadioActionEntry { radioActionName = show tool
                       , radioActionLabel = toolLabel tool
                       , radioActionStockId = Nothing
                       , radioActionAccelerator = Nothing
                       , radioActionTooltip = Nothing
                       , radioActionValue = fromEnum tool
                       })
    (fromEnum initialTool)
    (\radioAction -> do ui <- readUiRef uiRef
                        setTool ui =<< fmap toEnum (radioActionGetCurrentValue radioAction))

  helpAboutAction <- actionNew "About" "About" Nothing Nothing
  on helpAboutAction actionActivated helpAbout

  return Actions { myFileNewAction = fileNewAction
                 , myFileOpenAction = fileOpenAction
                 , myFileSaveAction = fileSaveAction
                 , myFileSaveAsAction = fileSaveAsAction
                 , myToolActions = toolActions
                 , myHelpAboutAction = helpAboutAction
                 }

initialize :: Actions -> IO ()
initialize actions =
  -- Activate 'initialTool' (requires the controller, so we can't do it in the
  -- construction phase).
  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialTool ++ ".")
         (actionGroupGetAction (myToolActions actions) $ show initialTool)

destruct :: Actions -> IO ()
destruct _ = return ()

fileOpen :: UiCtrl ui => UiRef ui -> IO ()
fileOpen uiRef = do
  ui <- readUiRef uiRef
  dialog <- fileChooserDialogNew (Just "Open a file")
                                 Nothing
                                 FileChooserActionOpen
                                 [(stockOpen, ResponseOk),
                                  (stockCancel, ResponseCancel)]
  mapM_ (fileChooserAddFilter dialog) =<< fileFiltersForSgf
  response <- dialogRun dialog
  widgetHide dialog
  finally
    (when (response == ResponseOk) $ do
        maybePath <- fileChooserGetFilename dialog
        when (isJust maybePath) $ do
          let path = fromJust maybePath
          loadResult <- openFile (Just ui) path
          case loadResult of
            Left parseError -> do
              errorDialog <- messageDialogNew
                             Nothing
                             []
                             MessageError
                             ButtonsOk
                             ("Error loading " ++ path ++ ".\n\n" ++ show parseError)
              dialogRun errorDialog
              widgetDestroy errorDialog
            Right _ -> return ())
    (widgetDestroy dialog)

fileSaveAs :: UiCtrl ui => UiRef ui -> IO ()
fileSaveAs uiRef = do
  ui <- readUiRef uiRef
  dialog <- fileChooserDialogNew (Just "Save file as")
                                 Nothing
                                 FileChooserActionSave
                                 [(stockSave, ResponseOk),
                                  (stockCancel, ResponseCancel)]
  mapM_ (fileChooserAddFilter dialog) =<< fileFiltersForSgf
  response <- dialogRun dialog
  finally
    (when (response == ResponseOk) $ do
        maybePath <- fileChooserGetFilename dialog
        whenMaybe maybePath $ \path -> do
          setFilePath ui $ Just path
          fileSave uiRef)
    (widgetDestroy dialog)

fileSave :: UiCtrl ui => UiRef ui -> IO ()
fileSave uiRef = do
  ui <- readUiRef uiRef
  cursor <- readCursor ui
  maybePath <- getFilePath ui
  case maybePath of
    Nothing -> fileSaveAs uiRef
    Just path -> do
      -- TODO Exception handling when the write fails.
      -- TODO Don't just write a single tree.
      -- TODO Only save when dirty?  (Be careful not to break Save As on a non-dirty game.)
      writeFile path $
        printCollection Collection { collectionTrees = [cursorNode $ cursorRoot cursor] }
      setDirty ui False

helpAbout :: IO ()
helpAbout = do
  about <- aboutDialogNew
  license <- fmap (fromMaybe fallbackLicense) readLicense
  set about [ aboutDialogProgramName := applicationName
            , aboutDialogCopyright := applicationCopyright
            , aboutDialogLicense := Just license
            , aboutDialogWebsite := applicationWebsite
            , aboutDialogAuthors := applicationAuthors
            ]
  dialogRun about
  widgetDestroy about
  return ()

readLicense :: IO (Maybe String)
readLicense = do
  path <- Paths.getDataFileName "LICENSE"
  fmap Just (readFile path) `catch` \(_ :: IOException) -> return Nothing

fallbackLicense :: String
fallbackLicense =
  "Could not read the license file." ++
  "\n" ++
  "\nGoatee is free software: you can redistribute it and/or modify" ++
  "\nit under the terms of the GNU Affero General Public License as published by" ++
  "\nthe Free Software Foundation, either version 3 of the License, or" ++
  "\n(at your option) any later version." ++
  "\n" ++
  "\nGoatee is distributed in the hope that it will be useful," ++
  "\nbut WITHOUT ANY WARRANTY; without even the implied warranty of" ++
  "\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" ++
  "\nGNU Affero General Public License for more details." ++
  "\n" ++
  "\nYou should have received a copy of the GNU Affero General Public License" ++
  "\nalong with Goatee.  If not, see <http://www.gnu.org/licenses/>."
