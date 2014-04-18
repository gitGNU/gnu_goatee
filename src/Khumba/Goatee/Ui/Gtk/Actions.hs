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
  myFileNewAction,
  myFileOpenAction,
  myFileSaveAction,
  myFileSaveAsAction,
  myFileCloseAction,
  myToolActions,
  myHelpAboutAction,
  fileSave,
  fileClose,
  ) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (void, when)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Graphics.UI.Gtk (
  Action,
  ActionGroup,
  AttrOp ((:=)),
  ButtonsType (ButtonsNone, ButtonsOk, ButtonsYesNo),
  DialogFlags (DialogDestroyWithParent, DialogModal),
  FileChooserAction (FileChooserActionOpen, FileChooserActionSave),
  MessageType (MessageError, MessageQuestion),
  RadioActionEntry (
    RadioActionEntry,
    radioActionAccelerator, radioActionLabel, radioActionName, radioActionStockId,
    radioActionTooltip, radioActionValue
    ),
  ResponseId (ResponseCancel, ResponseNo, ResponseOk, ResponseYes),
  aboutDialogAuthors, aboutDialogCopyright, aboutDialogLicense, aboutDialogNew,
  aboutDialogProgramName, aboutDialogWebsite,
  actionActivate, actionActivated, actionGroupAddActionWithAccel, actionGroupAddRadioActions,
  actionGroupGetAction, actionGroupNew, actionNew,
  fileChooserAddFilter, fileChooserDialogNew, fileChooserGetFilename,
  dialogAddButton, dialogRun,
  messageDialogNew,
  on,
  radioActionGetCurrentValue,
  set,
  stockCancel, stockOpen, stockSave, stockSaveAs,
  widgetDestroy, widgetHide,
  )
import Khumba.Goatee.App
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Printer
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Ui.Gtk.Common
import qualified Paths_goatee as Paths
import System.Directory (doesFileExist)

data Actions = Actions { myFileNewAction :: Action
                       , myFileOpenAction :: Action
                       , myFileSaveAction :: Action
                       , myFileSaveAsAction :: Action
                       , myFileCloseAction :: Action
                       , myToolActions :: ActionGroup
                       , myHelpAboutAction :: Action
                       }

create :: UiCtrl ui => ui -> IO Actions
create ui = do
  let tools = enumFrom minBound

  -- File actions.
  fileActions <- actionGroupNew "File"

  -- TODO Accelerators aren't working.
  fileNewAction <- actionNew "FileNew" "New file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNewAction $ Just "<Control>n"
  on fileNewAction actionActivated $ void $ openNewBoard (Just ui) Nothing

  fileOpenAction <- actionNew "FileOpen" "Open file..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileOpenAction $ Just "<Control>o"
  on fileOpenAction actionActivated $ fileOpen ui

  fileSaveAsAction <- actionNew "FileSaveAs" "Save file as..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAsAction $ Just "<Control><Shift>s"
  on fileSaveAsAction actionActivated $ void $ fileSaveAs ui

  fileSaveAction <- actionNew "FileSave" "Save file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAction $ Just "<Control>s"
  on fileSaveAction actionActivated $ void $ fileSave ui

  fileCloseAction <- actionNew "FileClose" "Close" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileCloseAction $ Just "<Control>w"
  on fileCloseAction actionActivated $ void $ fileClose ui

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
    (\radioAction -> setTool ui =<< fmap toEnum (radioActionGetCurrentValue radioAction))

  helpAboutAction <- actionNew "About" "About" Nothing Nothing
  on helpAboutAction actionActivated helpAbout

  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialTool ++ ".")
         (actionGroupGetAction toolActions $ show initialTool)

  return Actions { myFileNewAction = fileNewAction
                 , myFileOpenAction = fileOpenAction
                 , myFileSaveAction = fileSaveAction
                 , myFileSaveAsAction = fileSaveAsAction
                 , myFileCloseAction = fileCloseAction
                 , myToolActions = toolActions
                 , myHelpAboutAction = helpAboutAction
                 }

fileOpen :: UiCtrl ui => ui -> IO ()
fileOpen ui = do
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

-- | Presents a file save dialog for the user to specify a file to write the
-- current game to.  If the user provides a filename, then the game is written.
-- If the user names an existing file, then another dialog confirms overwriting
-- the existing file.  Returns true iff the user accepted the dialog(s) and the
-- game was saved.
fileSaveAs :: UiCtrl ui => ui -> IO Bool
fileSaveAs ui = do
  dialog <- fileChooserDialogNew (Just "Save file as")
                                 Nothing
                                 FileChooserActionSave
                                 [(stockSave, ResponseOk),
                                  (stockCancel, ResponseCancel)]
  mapM_ (fileChooserAddFilter dialog) =<< fileFiltersForSgf
  response <- dialogRun dialog
  finally
    (case response of
        ResponseOk -> do
          maybePath <- fileChooserGetFilename dialog
          case maybePath of
            Just path -> do
              confirm <- confirmSaveIfAlreadyExists path
              if confirm
                then do setFilePath ui $ Just path
                        fileSave ui
                else return False
            _ -> return False
        _ -> return False)
    (widgetDestroy dialog)

-- | If the given file already exists, then the user is shown a dialog box
-- asking whether the file should be overwritten.  Returns true if the user says
-- yes, or if the file doesn't exist.
confirmSaveIfAlreadyExists :: FilePath -> IO Bool
confirmSaveIfAlreadyExists path = do
  exists <- doesFileExist path
  if exists
    then do dialog <- messageDialogNew
                      Nothing
                      []
                      MessageQuestion
                      ButtonsYesNo
                      (path ++ " already exists.  Overwrite?")
            response <- dialogRun dialog
            widgetDestroy dialog
            return $ response == ResponseYes
    else return True

-- | Saves the current game to a file.  If the current game is not currently
-- tied to a file, then this will act identically to 'fileSaveAs'.  Returns true
-- iff the game was saved.
fileSave :: UiCtrl ui => ui -> IO Bool
fileSave ui = do
  cursor <- readCursor ui
  maybePath <- getFilePath ui
  case maybePath of
    Nothing -> fileSaveAs ui
    Just path -> do
      -- TODO Exception handling when the write fails.
      -- TODO Don't just write a single tree.
      -- TODO Only save when dirty?  (Be careful not to break Save As on a non-dirty game.)
      writeFile path $
        printCollection Collection { collectionTrees = [cursorNode $ cursorRoot cursor] }
      setDirty ui False
      return True

-- | Closes the game and all UI windows, etc. attached to the given controller.
-- If the game is dirty, then a dialog first prompts the user whether to save,
-- throw away changes, or abort the closing.
fileClose :: UiCtrl ui => ui -> IO Bool
fileClose ui = do
  close <- confirmFileClose ui
  when close $ closeBoard ui
  return close

-- | Should be called before destroying the main window.  Checks the dirty
-- state of UI; if dirty, then a dialog prompts the user whether the game
-- should be saved before closing, and giving the option to cancel closing.
-- Returns true if the window should be closed.
confirmFileClose :: UiCtrl ui => ui -> IO Bool
confirmFileClose ui = do
  dirty <- getDirty ui
  if dirty
    then do filePath <- getFilePath ui
            fileName <- getFileName ui
            window <- getMainWindow ui
            dialog <- messageDialogNew
                      (Just window)
                      [DialogModal, DialogDestroyWithParent]
                      MessageQuestion
                      ButtonsNone
                      (fileName ++ " has unsaved changes.  Save before closing?")
            dialogAddButton dialog (maybe stockSaveAs (const stockSave) filePath) ResponseYes
            dialogAddButton dialog "Close without saving" ResponseNo
            dialogAddButton dialog stockCancel ResponseCancel
            result <- dialogRun dialog
            widgetDestroy dialog
            case result of
              ResponseYes -> fileSave ui
              ResponseNo -> return True
              _ -> return False
    else return True

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
