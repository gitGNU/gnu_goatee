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
  myFileQuitAction,
  myToolActions,
  myHelpAboutAction,
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk (
  Action,
  ActionGroup,
  RadioActionEntry (
    RadioActionEntry,
    radioActionAccelerator, radioActionLabel, radioActionName, radioActionStockId,
    radioActionTooltip, radioActionValue
    ),
  actionActivate, actionActivated, actionGroupAddActionWithAccel, actionGroupAddRadioActions,
  actionGroupGetAction, actionGroupNew, actionNew,
  on,
  radioActionGetCurrentValue,
  )
import Khumba.Goatee.Ui.Gtk.Common

data Actions = Actions { myFileNewAction :: Action
                       , myFileOpenAction :: Action
                       , myFileSaveAction :: Action
                       , myFileSaveAsAction :: Action
                       , myFileCloseAction :: Action
                       , myFileQuitAction :: Action
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

  fileSaveAction <- actionNew "FileSave" "Save file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAction $ Just "<Control>s"
  on fileSaveAction actionActivated $ void $ fileSave ui

  fileSaveAsAction <- actionNew "FileSaveAs" "Save file as..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAsAction $ Just "<Control><Shift>s"
  on fileSaveAsAction actionActivated $ void $ fileSaveAs ui

  fileCloseAction <- actionNew "FileClose" "Close" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileCloseAction $ Just "<Control>w"
  on fileCloseAction actionActivated $ void $ fileClose ui

  fileQuitAction <- actionNew "FileQuit" "Quit" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileQuitAction $ Just "<Control>q"
  on fileQuitAction actionActivated $ void $ fileQuit ui

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
  on helpAboutAction actionActivated $ helpAbout ui

  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialTool ++ ".")
         (actionGroupGetAction toolActions $ show initialTool)

  return Actions { myFileNewAction = fileNewAction
                 , myFileOpenAction = fileOpenAction
                 , myFileSaveAction = fileSaveAction
                 , myFileSaveAsAction = fileSaveAsAction
                 , myFileCloseAction = fileCloseAction
                 , myFileQuitAction = fileQuitAction
                 , myToolActions = toolActions
                 , myHelpAboutAction = helpAboutAction
                 }
