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
module Game.Goatee.Ui.Gtk.Actions (
  Actions,
  create,
  destroy,
  myFileNew9Action,
  myFileNew13Action,
  myFileNew19Action,
  myFileNewCustomAction,
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
  myViewHighlightCurrentMovesAction,
  myHelpAboutAction,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad hiding (on)
import Game.Goatee.Sgf.Types
import Graphics.UI.Gtk (
  Action,
  ActionGroup,
  AttrOp ((:=)),
  Packing (PackGrow),
  RadioAction,
  RadioActionEntry (
    RadioActionEntry,
    radioActionAccelerator, radioActionLabel, radioActionName, radioActionStockId,
    radioActionTooltip, radioActionValue
    ),
  ResponseId (ResponseCancel, ResponseOk),
  SpinButtonUpdatePolicy (UpdateIfValid),
  ToggleAction,
  actionActivate, actionActivated, actionGroupAddActionWithAccel, actionGroupAddRadioActions,
  actionGroupGetAction, actionGroupNew, actionNew, actionToggled,
  boxPackStart,
  dialogAddButton, dialogGetUpper, dialogNew, dialogRun, dialogSetDefaultResponse,
  get,
  labelNewWithMnemonic, labelSetMnemonicWidget,
  on,
  radioActionChanged, radioActionCurrentValue, radioActionNew, radioActionSetGroup,
  set,
  spinButtonGetValueAsInt, spinButtonNewWithRange, spinButtonSetNumeric, spinButtonSetUpdatePolicy,
  spinButtonSetValue, spinButtonSetWrap,
  stockCancel,
  tableAttachDefaults, tableNew,
  toggleActionActive, toggleActionNew,
  widgetDestroy, widgetShowAll,
  )

data Actions ui = Actions { myUi :: ui
                          , myRegistrations :: ViewRegistrations
                          , myFileNew9Action :: Action
                          , myFileNew13Action :: Action
                          , myFileNew19Action :: Action
                          , myFileNewCustomAction :: Action
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
                          , myViewHighlightCurrentMovesAction :: ToggleAction
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

  fileNew9Action <- actionNew "FileNew9" "New _9x9 board" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNew9Action Nothing
  on fileNew9Action actionActivated $ void $ openNewBoard (Just ui) (Just (9, 9))

  fileNew13Action <- actionNew "FileNew13" "New 1_3x13 board" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNew13Action Nothing
  on fileNew13Action actionActivated $ void $ openNewBoard (Just ui) (Just (13, 13))

  fileNew19Action <- actionNew "FileNew19" "New _19x19 board" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNew19Action $ Just "<Control>n"
  on fileNew19Action actionActivated $ void $ openNewBoard (Just ui) (Just (19, 19))

  fileNewCustomAction <- actionNew "FileNewCustom" "New _custom board..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNewCustomAction Nothing
  on fileNewCustomAction actionActivated $ do
    dialog <- dialogNew
    upper <- dialogGetUpper dialog
    table <- tableNew 2 2 False
    boxPackStart upper table PackGrow 0

    -- SGF only supports boards up to 'boardSizeMax', but Goatee works fine with
    -- larger boards.  The spinner wants an upper bound, so let's at least set
    -- something that isn't too outrageous along a single dimension.
    let arbitraryUpperLimit = 1000
        makeSpinButton = do
          spin <- spinButtonNewWithRange
                  (fromIntegral boardSizeMin)
                  (fromIntegral arbitraryUpperLimit)
                  1
          spinButtonSetUpdatePolicy spin UpdateIfValid
          spinButtonSetNumeric spin True
          spinButtonSetWrap spin False
          return spin

    widthLabel <- labelNewWithMnemonic "_Width"
    widthSpin <- makeSpinButton
    labelSetMnemonicWidget widthLabel widthSpin
    tableAttachDefaults table widthLabel 0 1 0 1
    tableAttachDefaults table widthSpin 1 2 0 1

    heightLabel <- labelNewWithMnemonic "_Height"
    heightSpin <- makeSpinButton
    labelSetMnemonicWidget heightLabel heightSpin
    tableAttachDefaults table heightLabel 0 1 1 2
    tableAttachDefaults table heightSpin 1 2 1 2

    dialogAddButton dialog stockCancel ResponseCancel
    dialogAddButton dialog "C_reate" ResponseOk
    dialogSetDefaultResponse dialog ResponseOk

    spinButtonSetValue widthSpin $ fromIntegral boardSizeDefault
    spinButtonSetValue heightSpin $ fromIntegral boardSizeDefault

    widgetShowAll dialog
    response <- dialogRun dialog
    width <- spinButtonGetValueAsInt widthSpin
    height <- spinButtonGetValueAsInt heightSpin
    widgetDestroy dialog
    when (response == ResponseOk) $
      void $ openNewBoard (Just ui) (Just (width, height))

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

  viewHighlightCurrentMovesAction <-
    toggleActionNew "ViewHighlightCurrentMoves" "Highlight _current moves" Nothing Nothing
  set viewHighlightCurrentMovesAction [toggleActionActive := uiHighlightCurrentMovesMode modes]
  on viewHighlightCurrentMovesAction actionToggled $ do
    active <- get viewHighlightCurrentMovesAction toggleActionActive
    modifyModes ui $ \modes -> return modes { uiHighlightCurrentMovesMode = active }

  helpAboutAction <- actionNew "HelpAbout" "_About" Nothing Nothing
  on helpAboutAction actionActivated $ helpAbout ui

  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialTool ++ ".")
         (actionGroupGetAction toolActions $ show initialTool)

  registrations <- viewNewRegistrations

  let me = Actions { myUi = ui
                   , myRegistrations = registrations
                   , myFileNew9Action = fileNew9Action
                   , myFileNew13Action = fileNew13Action
                   , myFileNew19Action = fileNew19Action
                   , myFileNewCustomAction = fileNewCustomAction
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
                   , myViewHighlightCurrentMovesAction = viewHighlightCurrentMovesAction
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
