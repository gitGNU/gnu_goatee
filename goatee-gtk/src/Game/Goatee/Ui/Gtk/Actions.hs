-- This file is part of Goatee.
--
-- Copyright 2014-2015 Bryan Gardiner
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

{-# LANGUAGE CPP #-}

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
  myEditCutNodeAction,
  myEditCopyNodeAction,
  myEditPasteNodeAction,
  myGamePassAction,
  myGameVariationsChildAction,
  myGameVariationsCurrentAction,
  myGameVariationsBoardMarkupOnAction,
  myGameVariationsBoardMarkupOffAction,
  myToolActions,
  myViewHighlightCurrentMovesAction,
  myViewStonesRegularModeAction,
  myViewStonesOneColorModeAction,
  myViewStonesBlindModeAction,
  myHelpKeyBindingsAction,
  myHelpAboutAction,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM, unless, void, when)
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Utils
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad hiding (on)
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Types
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
  actionActivate, actionActivated, actionGroupAddAction, actionGroupAddRadioActions,
  actionGroupGetAction, actionGroupListActions, actionGroupNew, actionNew, actionSensitive,
  actionToggled,
  boxPackStart,
  castToRadioAction,
  dialogAddButton, dialogGetUpper, dialogNew, dialogRun, dialogSetDefaultResponse,
  get,
  labelNewWithMnemonic, labelSetMnemonicWidget,
  on,
  radioActionChanged, radioActionCurrentValue, radioActionNew, radioActionSetGroup,
  set,
  spinButtonGetValueAsInt, spinButtonNewWithRange, spinButtonSetDigits,
  spinButtonSetNumeric, spinButtonSetUpdatePolicy, spinButtonSetValue, spinButtonSetWrap,
  stockCancel,
  tableAttachDefaults, tableNew,
  toggleActionActive, toggleActionNew,
  widgetDestroy, widgetShowAll,
  windowSetTitle,
  )
import System.Glib (stringToGlib)

data Actions ui = Actions
  { myUi :: ui
  , myState :: ViewState
  , myFileNew9Action :: Action
  , myFileNew13Action :: Action
  , myFileNew19Action :: Action
  , myFileNewCustomAction :: Action
  , myFileOpenAction :: Action
  , myFileSaveAction :: Action
  , myFileSaveAsAction :: Action
  , myFileCloseAction :: Action
  , myFileQuitAction :: Action
  , myEditCutNodeAction :: Action
  , myEditCopyNodeAction :: Action
  , myEditPasteNodeAction :: Action
  , myGamePassAction :: Action
  , myGameVariationsChildAction :: RadioAction
  , myGameVariationsCurrentAction :: RadioAction
  , myGameVariationsBoardMarkupOnAction :: RadioAction
  , myGameVariationsBoardMarkupOffAction :: RadioAction
  , myToolActions :: ActionGroup
  , mySomeToolAction :: RadioAction
    -- ^ An arbitrary action in the 'myToolActions' group.
  , myViewHighlightCurrentMovesAction :: ToggleAction
  , myViewStonesRegularModeAction :: RadioAction
  , myViewStonesOneColorModeAction :: RadioAction
  , myViewStonesBlindModeAction :: RadioAction
  , myHelpKeyBindingsAction :: Action
  , myHelpAboutAction :: Action
  , myModesChangedHandler :: IORef (Maybe Registration)
  }

instance UiCtrl go ui => UiView go ui (Actions ui) where
  viewName = const "Actions"
  viewCtrl = myUi
  viewState = myState
  viewUpdate = update

create :: UiCtrl go ui => ui -> IO (Actions ui)
create ui = do
  let toolTypes = enumFrom minBound
  modes <- readModes ui

  -- File actions.
  fileActions <- actionGroupNew "File"

  fileNew9Action <- actionNew "FileNew9" "New _9x9 board" Nothing Nothing
  actionGroupAddAction fileActions fileNew9Action
  on fileNew9Action actionActivated $ void $ openNewBoard (Just ui) (Just (9, 9))

  fileNew13Action <- actionNew "FileNew13" "New 1_3x13 board" Nothing Nothing
  actionGroupAddAction fileActions fileNew13Action
  on fileNew13Action actionActivated $ void $ openNewBoard (Just ui) (Just (13, 13))

  fileNew19Action <- actionNew "FileNew19" "New _19x19 board" Nothing Nothing
  actionGroupAddAction fileActions fileNew19Action
  on fileNew19Action actionActivated $ void $ openNewBoard (Just ui) (Just (19, 19))

  fileNewCustomAction <- actionNew "FileNewCustom" "New _custom board..." Nothing Nothing
  actionGroupAddAction fileActions fileNewCustomAction
  on fileNewCustomAction actionActivated $ do
    dialog <- dialogNew
    windowSetTitle dialog "New custom board"
    upper <- dialogGetUpper dialog
    table <- tableNew 4 2 False
    boxPackStart upper table PackGrow 0

    -- SGF only supports boards up to 'boardSizeMax', but Goatee works fine with
    -- larger boards.  The spinner wants an upper bound, so let's at least set
    -- something that isn't too outrageous along a single dimension.
    let arbitraryUpperLimit = 1000 :: Int
        makeSpinButton = do
          spin <- spinButtonNewWithRange
                  (fromIntegral boardSizeMin)
                  (fromIntegral arbitraryUpperLimit)
                  1
          configureSpinButton spin
          return spin
        configureSpinButton spin = do
          spinButtonSetUpdatePolicy spin UpdateIfValid
          spinButtonSetNumeric spin True
          spinButtonSetWrap spin False

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

    handicapLabel <- labelNewWithMnemonic "H_andicap"
    handicapSpin <- spinButtonNewWithRange 0 9 1
    labelSetMnemonicWidget handicapLabel handicapSpin
    configureSpinButton handicapSpin
    tableAttachDefaults table handicapLabel 0 1 2 3
    tableAttachDefaults table handicapSpin 1 2 2 3

    komiLabel <- labelNewWithMnemonic "_Komi"
    komiSpin <- spinButtonNewWithRange (-1000) 1000 0.5
    labelSetMnemonicWidget komiLabel komiSpin
    configureSpinButton komiSpin
    spinButtonSetDigits komiSpin 1
    spinButtonSetValue komiSpin 0
    tableAttachDefaults table komiLabel 0 1 3 4
    tableAttachDefaults table komiSpin 1 2 3 4

    dialogAddButton dialog stockCancel ResponseCancel
    dialogAddButton dialog "C_reate" ResponseOk
    dialogSetDefaultResponse dialog ResponseOk

    spinButtonSetValue widthSpin $ fromIntegral boardSizeDefault
    spinButtonSetValue heightSpin $ fromIntegral boardSizeDefault

    widgetShowAll dialog
    response <- dialogRun dialog
    width <- spinButtonGetValueAsInt widthSpin
    height <- spinButtonGetValueAsInt heightSpin
    handicap <- spinButtonGetValueAsInt handicapSpin
    komi <- spinButtonGetValueAsBigfloat komiSpin
    widgetDestroy dialog
    when (response == ResponseOk) $ do
      ui' <- openNewBoard (Just ui) (Just (width, height))
      when (komi /= 0) $ doUiGo ui' $ putProperty $ KM komi
      when (handicap > 0) $ do
        -- If the board size + handicap configuration is known, then set up the
        -- board for the handicap, otherwise, leave the user to do it.
        --
        -- TODO If the configuration is unknown and handicap >= 2, then maybe
        -- set HA at least, anyway?
        let stones = fromMaybe [] $ handicapStones width height handicap
        unless (null stones) $ do
          doUiGo ui' $ do
            putProperty $ HA handicap
            putProperty $ AB $ coords stones
            putProperty $ PL White
          setDirty ui' False

  fileOpenAction <- actionNew "FileOpen" "_Open file..." Nothing Nothing
  actionGroupAddAction fileActions fileOpenAction
  on fileOpenAction actionActivated $ fileOpen ui

  fileSaveAction <- actionNew "FileSave" "_Save file" Nothing Nothing
  actionGroupAddAction fileActions fileSaveAction
  on fileSaveAction actionActivated $ void $ fileSave ui

  fileSaveAsAction <- actionNew "FileSaveAs" "Sa_ve file as..." Nothing Nothing
  actionGroupAddAction fileActions fileSaveAsAction
  on fileSaveAsAction actionActivated $ void $ fileSaveAs ui

  fileCloseAction <- actionNew "FileClose" "_Close" Nothing Nothing
  actionGroupAddAction fileActions fileCloseAction
  on fileCloseAction actionActivated $ void $ fileClose ui

  fileQuitAction <- actionNew "FileQuit" "_Quit" Nothing Nothing
  actionGroupAddAction fileActions fileQuitAction
  on fileQuitAction actionActivated $ void $ fileQuit ui

  -- Edit actions.
  editCutNodeAction <- actionNew "EditCutNode" "Cut current node" Nothing Nothing
  on editCutNodeAction actionActivated $ editCutNode ui

  editCopyNodeAction <- actionNew "EditCopyNode" "Copy current node" Nothing Nothing
  on editCopyNodeAction actionActivated $ editCopyNode ui

  editPasteNodeAction <- actionNew "EditPasteNode" "Paste node as child" Nothing Nothing
  on editPasteNodeAction actionActivated $ editPasteNode ui

  -- Game actions.
  gamePassAction <- actionNew "GamePass" "_Pass" Nothing Nothing
  on gamePassAction actionActivated $ playAt ui Nothing

  gameVariationsChildAction <- radioActionNew "gameVariationsChild"
                               "_Child variations"
                               (Just "Show children node as variations")
                               Nothing
                               (fromEnum ShowChildVariations)
  gameVariationsCurrentAction <- radioActionNew "gameVariationsCurrent"
                                 "C_urrent variations"
                                 (Just "Show variations of the current node")
                                 Nothing
                                 (fromEnum ShowCurrentVariations)
  radioActionSetGroup gameVariationsChildAction gameVariationsCurrentAction

  gameVariationsBoardMarkupOnAction <- radioActionNew "gameVariationsBoardMarkupOn"
                                       "_Show on board"
                                       (Just "Show move variations on the board")
                                       Nothing
                                       (fromEnum True)
  gameVariationsBoardMarkupOffAction <- radioActionNew "gameVariationsBoardMarkupOn"
                                        "_Hide on board"
                                        (Just "Hide move variations on the board")
                                        Nothing
                                        (fromEnum False)
  radioActionSetGroup gameVariationsBoardMarkupOnAction gameVariationsBoardMarkupOffAction

  initialVariationMode <-
    rootInfoVariationMode . gameInfoRootInfo . boardGameInfo . cursorBoard <$>
    readCursor ui
  set gameVariationsChildAction
    [radioActionCurrentValue := fromEnum (variationModeSource initialVariationMode)]
  set gameVariationsBoardMarkupOnAction
    [radioActionCurrentValue := fromEnum (variationModeBoardMarkup initialVariationMode)]

  -- This signal is emitted on every action in a radio group when the active
  -- item is changed, so we only need to listen with one action.
  on gameVariationsChildAction radioActionChanged $ \action -> do
    value <- toEnum <$> get action radioActionCurrentValue
    doUiGo ui $ modifyVariationMode $ \mode -> mode { variationModeSource = value }

  on gameVariationsBoardMarkupOnAction radioActionChanged $ \action -> do
    value <- toEnum <$> get action radioActionCurrentValue
    doUiGo ui $ modifyVariationMode $ \mode -> mode { variationModeBoardMarkup = value }

  -- Tool actions.
  toolActions <- actionGroupNew "Tools"
  toolActionList <- fmap catMaybes $ forM toolTypes $ \toolType -> do
    AnyTool tool <- findTool ui toolType
    return $ Just RadioActionEntry
      { radioActionName = stringToGlib $ show toolType
      , radioActionLabel = stringToGlib $ toolLabel tool
      , radioActionStockId = Nothing
      , radioActionAccelerator = Nothing
      , radioActionTooltip = Nothing
      , radioActionValue = fromEnum toolType
      }
  actionGroupAddRadioActions toolActions toolActionList (fromEnum initialToolType)
    (\radioAction -> setTool ui =<< fmap toEnum (get radioAction radioActionCurrentValue))
  someToolAction <- actionGroupListActions toolActions >>= \actions -> case actions of
    someAction:_ -> return $ castToRadioAction someAction
    _ -> error "Actions.initialize: Couldn't grab a tool action!?"

  -- View actions.
  viewHighlightCurrentMovesAction <-
    toggleActionNew "ViewHighlightCurrentMoves" "Highlight _current moves" Nothing Nothing
  set viewHighlightCurrentMovesAction [toggleActionActive := uiHighlightCurrentMovesMode modes]
  on viewHighlightCurrentMovesAction actionToggled $ do
    active <- get viewHighlightCurrentMovesAction toggleActionActive
    modifyModes ui $ \modes -> return modes { uiHighlightCurrentMovesMode = active }

  viewStonesRegularModeAction <-
    radioActionNew "ViewStonesRegularMode"
    "_Regular"
    (Just "Regular Go: Render stones on the board normally.")
    Nothing
    (fromEnum ViewStonesRegularMode)
  viewStonesOneColorModeAction <-
    radioActionNew "ViewStonesOneColorMode"
    "_One-color"
    (Just "One-color Go: Both players use the same color stones.")
    Nothing
    (fromEnum ViewStonesOneColorMode)
  viewStonesBlindModeAction <-
    radioActionNew "ViewStonesBlindMode"
    "_Blind"
    (Just "Blind Go: No stones are visible on the board.")
    Nothing
    (fromEnum ViewStonesBlindMode)

  radioActionSetGroup viewStonesOneColorModeAction viewStonesRegularModeAction
  radioActionSetGroup viewStonesBlindModeAction viewStonesRegularModeAction
  initialViewStonesMode <- uiViewStonesMode <$> readModes ui
  set viewStonesRegularModeAction [radioActionCurrentValue := fromEnum initialViewStonesMode]

  on viewStonesRegularModeAction radioActionChanged $ \action -> do
    value <- toEnum <$> get action radioActionCurrentValue
    modifyModes ui $ \modes -> return modes { uiViewStonesMode = value }

  -- Help actions.
  helpKeyBindingsAction <- actionNew "HelpKeyBindings" "_Key bindings" Nothing Nothing
  on helpKeyBindingsAction actionActivated $ helpKeyBindings ui

  helpAboutAction <- actionNew "HelpAbout" "_About" Nothing Nothing
  on helpAboutAction actionActivated $ helpAbout ui

  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialToolType ++ ".")
         (actionGroupGetAction toolActions $ show initialToolType)

  state <- viewStateNew
  modesChangedHandler <- newIORef Nothing

  let me = Actions
        { myUi = ui
        , myState = state
        , myFileNew9Action = fileNew9Action
        , myFileNew13Action = fileNew13Action
        , myFileNew19Action = fileNew19Action
        , myFileNewCustomAction = fileNewCustomAction
        , myFileOpenAction = fileOpenAction
        , myFileSaveAction = fileSaveAction
        , myFileSaveAsAction = fileSaveAsAction
        , myFileCloseAction = fileCloseAction
        , myFileQuitAction = fileQuitAction
        , myEditCutNodeAction = editCutNodeAction
        , myEditCopyNodeAction = editCopyNodeAction
        , myEditPasteNodeAction = editPasteNodeAction
        , myGamePassAction = gamePassAction
        , myGameVariationsChildAction = gameVariationsChildAction
        , myGameVariationsCurrentAction = gameVariationsCurrentAction
        , myGameVariationsBoardMarkupOnAction = gameVariationsBoardMarkupOnAction
        , myGameVariationsBoardMarkupOffAction = gameVariationsBoardMarkupOffAction
        , myToolActions = toolActions
        , mySomeToolAction = someToolAction
        , myViewHighlightCurrentMovesAction = viewHighlightCurrentMovesAction
        , myViewStonesRegularModeAction = viewStonesRegularModeAction
        , myViewStonesOneColorModeAction = viewStonesOneColorModeAction
        , myViewStonesBlindModeAction = viewStonesBlindModeAction
        , myHelpKeyBindingsAction = helpKeyBindingsAction
        , myHelpAboutAction = helpAboutAction
        , myModesChangedHandler = modesChangedHandler
        }

  initialize me
  return me

initialize :: UiCtrl go ui => Actions ui -> IO ()
initialize me = do
  let ui = myUi me
  register me
    [ AnyEvent navigationEvent
    , AnyEvent variationModeChangedEvent
    ]
  writeIORef (myModesChangedHandler me) =<<
    fmap Just (registerModesChangedHandler ui "Actions" $ \_ _ -> update me)

destroy :: UiCtrl go ui => Actions ui -> IO ()
destroy me = do
  let ui = myUi me
  F.mapM_ (unregisterModesChangedHandler ui) =<< readIORef (myModesChangedHandler me)
  viewDestroy me

update :: UiCtrl go ui => Actions ui -> IO ()
update me = do
  cursor <- readCursor $ myUi me

  -- Update the sensitivity of the "Edit > Cut node" action.
  set (myEditCutNodeAction me) [actionSensitive := isJust $ cursorParent cursor]

  updateVariationModeActions me cursor
  updateToolActions me

-- | Updates the selection variation mode radio actions.
updateVariationModeActions :: Actions ui -> Cursor -> IO ()
updateVariationModeActions me cursor = do
  let new = rootInfoVariationMode $ gameInfoRootInfo $ boardGameInfo $
            cursorBoard cursor
      newSource = fromEnum $ variationModeSource new
      newBoardMarkup = fromEnum $ variationModeBoardMarkup new
      sourceAction = myGameVariationsChildAction me
      boardMarkupAction = myGameVariationsBoardMarkupOnAction me

  oldSource <- get sourceAction radioActionCurrentValue
  when (newSource /= oldSource) $
    set sourceAction [radioActionCurrentValue := newSource]

  oldBoardMarkup <- get boardMarkupAction radioActionCurrentValue
  when (newBoardMarkup /= oldBoardMarkup) $
    set boardMarkupAction [radioActionCurrentValue := newBoardMarkup]

-- | Updates the active tool radio action.
updateToolActions :: UiCtrl go ui => Actions ui -> IO ()
updateToolActions me = do
  let ui = myUi me
  tool <- uiToolType <$> readModes ui
  set (mySomeToolAction me) [radioActionCurrentValue := fromEnum tool]
