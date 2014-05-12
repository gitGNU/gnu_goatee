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
  destroy,
  myFileNewAction,
  myFileOpenAction,
  myFileSaveAction,
  myFileSaveAsAction,
  myFileCloseAction,
  myFileQuitAction,
  myToolActions,
  myViewVariationsChildAction,
  myViewVariationsCurrentAction,
  myViewVariationsBoardMarkupOnAction,
  myViewVariationsBoardMarkupOffAction,
  myViewShowCurrentMovesAction,
  myHelpAboutAction,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Graphics.UI.Gtk (
  Action,
  ActionGroup,
  AttrOp ((:=)),
  RadioAction,
  RadioActionEntry (
    RadioActionEntry,
    radioActionAccelerator, radioActionLabel, radioActionName, radioActionStockId,
    radioActionTooltip, radioActionValue
    ),
  ToggleAction,
  actionActivate, actionActivated, actionGroupAddActionWithAccel, actionGroupAddRadioActions,
  actionGroupGetAction, actionGroupNew, actionNew, actionToggled,
  get,
  on,
  radioActionChanged, radioActionCurrentValue, radioActionNew, radioActionSetGroup,
  set,
  toggleActionActive, toggleActionNew,
  )
import Khumba.Goatee.Ui.Gtk.Common
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Monad hiding (on)
import Khumba.Goatee.Sgf.Types

data Actions ui = Actions { myUi :: ui
                          , myRegistrations :: ViewRegistrations
                          , myFileNewAction :: Action
                          , myFileOpenAction :: Action
                          , myFileSaveAction :: Action
                          , myFileSaveAsAction :: Action
                          , myFileCloseAction :: Action
                          , myFileQuitAction :: Action
                          , myToolActions :: ActionGroup
                          , myViewVariationsChildAction :: RadioAction
                          , myViewVariationsCurrentAction :: RadioAction
                          , myViewVariationsBoardMarkupOnAction :: RadioAction
                          , myViewVariationsBoardMarkupOffAction :: RadioAction
                          , myViewShowCurrentMovesAction :: ToggleAction
                          , myHelpAboutAction :: Action
                          }

instance UiCtrl ui => UiView (Actions ui) ui where
  viewName = const "Actions"
  viewCtrl = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => ui -> IO (Actions ui)
create ui = do
  let tools = enumFrom minBound
  modes <- readModes ui

  -- File actions.
  fileActions <- actionGroupNew "File"

  -- TODO Accelerators aren't working.
  fileNewAction <- actionNew "FileNew" "_New file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNewAction $ Just "<Control>n"
  on fileNewAction actionActivated $ void $ openNewBoard (Just ui) Nothing

  fileOpenAction <- actionNew "FileOpen" "_Open file..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileOpenAction $ Just "<Control>o"
  on fileOpenAction actionActivated $ fileOpen ui

  fileSaveAction <- actionNew "FileSave" "_Save file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAction $ Just "<Control>s"
  on fileSaveAction actionActivated $ void $ fileSave ui

  fileSaveAsAction <- actionNew "FileSaveAs" "Sa_ve file as..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileSaveAsAction $ Just "<Control><Shift>s"
  on fileSaveAsAction actionActivated $ void $ fileSaveAs ui

  fileCloseAction <- actionNew "FileClose" "_Close" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileCloseAction $ Just "<Control>w"
  on fileCloseAction actionActivated $ void $ fileClose ui

  fileQuitAction <- actionNew "FileQuit" "_Quit" Nothing Nothing
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
    (\radioAction -> setTool ui =<< fmap toEnum (get radioAction radioActionCurrentValue))

  -- Variation mode view actions.
  viewVariationsChildAction <- radioActionNew "viewVariationsChild"
                               "_Child variations"
                               (Just "Show children node as variations")
                               Nothing
                               (fromEnum ShowChildVariations)
  viewVariationsCurrentAction <- radioActionNew "viewVariationsCurrent"
                                 "C_urrent variations"
                                 (Just "Show variations of the current node")
                                 Nothing
                                 (fromEnum ShowCurrentVariations)
  radioActionSetGroup viewVariationsChildAction viewVariationsCurrentAction

  viewVariationsBoardMarkupOnAction <- radioActionNew "viewVariationsBoardMarkupOn"
                                       "_Show on board"
                                       (Just "Show move variations on the board")
                                       Nothing
                                       (fromEnum True)
  viewVariationsBoardMarkupOffAction <- radioActionNew "viewVariationsBoardMarkupOn"
                                        "_Hide on board"
                                        (Just "Hide move variations on the board")
                                        Nothing
                                        (fromEnum False)
  radioActionSetGroup viewVariationsBoardMarkupOnAction viewVariationsBoardMarkupOffAction

  initialVariationMode <-
    rootInfoVariationMode . gameInfoRootInfo . boardGameInfo . cursorBoard <$>
    readCursor ui
  set viewVariationsChildAction
    [radioActionCurrentValue := fromEnum (variationModeSource initialVariationMode)]
  set viewVariationsBoardMarkupOnAction
    [radioActionCurrentValue := fromEnum (variationModeBoardMarkup initialVariationMode)]

  -- This signal is emitted on every action in a radio group when the active
  -- item is changed, so we only need to listen with one action.
  on viewVariationsChildAction radioActionChanged $ \action -> do
    value <- toEnum <$> get action radioActionCurrentValue
    runUiGo ui $ modifyVariationMode $ \mode -> mode { variationModeSource = value }

  on viewVariationsBoardMarkupOnAction radioActionChanged $ \action -> do
    value <- toEnum <$> get action radioActionCurrentValue
    runUiGo ui $ modifyVariationMode $ \mode -> mode { variationModeBoardMarkup = value }

  viewShowCurrentMovesAction <-
    toggleActionNew "ViewShowCurrentMoves" "Show _current moves" Nothing Nothing
  set viewShowCurrentMovesAction [toggleActionActive := uiShowCurrentMovesMode modes]
  on viewShowCurrentMovesAction actionToggled $ do
    active <- get viewShowCurrentMovesAction toggleActionActive
    modifyModes ui $ \modes -> return modes { uiShowCurrentMovesMode = active }

  helpAboutAction <- actionNew "HelpAbout" "_About" Nothing Nothing
  on helpAboutAction actionActivated $ helpAbout ui

  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialTool ++ ".")
         (actionGroupGetAction toolActions $ show initialTool)

  registrations <- viewNewRegistrations

  let me = Actions { myUi = ui
                   , myRegistrations = registrations
                   , myFileNewAction = fileNewAction
                   , myFileOpenAction = fileOpenAction
                   , myFileSaveAction = fileSaveAction
                   , myFileSaveAsAction = fileSaveAsAction
                   , myFileCloseAction = fileCloseAction
                   , myFileQuitAction = fileQuitAction
                   , myToolActions = toolActions
                   , myViewVariationsChildAction = viewVariationsChildAction
                   , myViewVariationsCurrentAction = viewVariationsCurrentAction
                   , myViewVariationsBoardMarkupOnAction = viewVariationsBoardMarkupOnAction
                   , myViewVariationsBoardMarkupOffAction = viewVariationsBoardMarkupOffAction
                   , myViewShowCurrentMovesAction = viewShowCurrentMovesAction
                   , myHelpAboutAction = helpAboutAction
                   }

  initialize me
  return me

initialize :: UiCtrl ui => Actions ui -> IO ()
initialize me =
  viewRegister me variationModeChangedEvent $ \_ new -> afterGo $ do
    let newSource = fromEnum $ variationModeSource new
        newBoardMarkup = fromEnum $ variationModeBoardMarkup new
        sourceAction = myViewVariationsChildAction me
        boardMarkupAction = myViewVariationsBoardMarkupOnAction me

    oldSource <- get sourceAction radioActionCurrentValue
    when (newSource /= oldSource) $
      set sourceAction [radioActionCurrentValue := newSource]

    oldBoardMarkup <- get boardMarkupAction radioActionCurrentValue
    when (newBoardMarkup /= oldBoardMarkup) $
      set boardMarkupAction [radioActionCurrentValue := newBoardMarkup]

destroy :: UiCtrl ui => Actions ui -> IO ()
destroy = viewUnregisterAll
