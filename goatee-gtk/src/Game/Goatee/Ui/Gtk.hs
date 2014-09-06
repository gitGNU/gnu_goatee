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

-- | The main module for the GTK+ UI, used by clients of the UI.  Also
-- implements the UI controller.
module Game.Goatee.Ui.Gtk (
  StdUiCtrlImpl,
  startBoard,
  startNewBoard,
  startFile,
  ) where

import Control.Applicative ((<$>), Applicative)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, readMVar, putMVar, modifyMVar, modifyMVar_)
import Control.Exception (IOException, catch, finally)
import Control.Monad (forM_, join, liftM, unless, void, when)
import Control.Monad.State (MonadState, State, runState, get, put, modify)
import Data.Char (isSpace)
import qualified Data.Foldable as Foldable
import Data.Foldable (foldl')
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Unique (Unique, newUnique)
import Game.Goatee.App
import Game.Goatee.Common
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Parser
import Game.Goatee.Lib.Renderer
import Game.Goatee.Lib.Renderer.Tree
import Game.Goatee.Lib.Tree
import qualified Game.Goatee.Lib.Monad as Monad
import Game.Goatee.Lib.Monad (
  GoT, MonadGo, runGoT,
  AnyEvent (..), on0, childAddedEvent, childDeletedEvent, propertiesModifiedEvent,
  )
import Game.Goatee.Ui.Gtk.Common
import qualified Game.Goatee.Ui.Gtk.MainWindow as MainWindow
import Game.Goatee.Ui.Gtk.MainWindow (MainWindow)
import Game.Goatee.Ui.Gtk.Tool
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  ButtonsType (ButtonsNone, ButtonsOk, ButtonsYesNo),
  Clipboard,
  DialogFlags (DialogDestroyWithParent, DialogModal),
  FileChooserAction (FileChooserActionOpen, FileChooserActionSave),
  MessageType (MessageError, MessageQuestion),
  ResponseId (ResponseCancel, ResponseNo, ResponseOk, ResponseYes),
  aboutDialogAuthors, aboutDialogCopyright, aboutDialogLicense, aboutDialogNew,
  aboutDialogProgramName, aboutDialogWebsite,
  clipboardGet, clipboardRequestText, clipboardSetText,
  dialogAddButton, dialogRun,
  fileChooserAddFilter, fileChooserDialogNew, fileChooserGetFilename,
  mainQuit,
  messageDialogNew,
  selectionClipboard,
  stockCancel, stockOpen, stockSave, stockSaveAs,
  widgetDestroy, widgetHide,
  set,
  )
import qualified Paths_goatee_gtk as Paths
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | A structure for holding global application information about all open
-- boards.
data AppState = AppState
  { appControllers :: MVar (Map CtrlId AnyUiCtrl)
  -- ^ Maps all of the open boards' controllers by their IDs.
  }

-- | Creates an 'AppState' that is holding no controllers.
newAppState :: IO AppState
newAppState = do
  controllers <- newMVar Map.empty
  return AppState { appControllers = controllers }

-- | Registers a 'UiCtrlImpl' with an 'AppState'.
appStateRegister :: MonadUiGo go => AppState -> UiCtrlImpl go -> IO ()
appStateRegister appState ui =
  modifyMVar_ (appControllers appState) $ return . Map.insert (uiCtrlId ui) (AnyUiCtrl ui)

-- | Unregisters a 'UiCtrlImpl' from an 'AppState'.  If the 'AppState' is left
-- with no controllers, then the GTK+ main loop is shut down and the application
-- exits.
appStateUnregister :: AppState -> UiCtrlImpl go -> IO ()
appStateUnregister appState ui = do
  ctrls' <- modifyMVar (appControllers appState) $ \ctrls ->
    let ctrls' = Map.delete (uiCtrlId ui) ctrls
    in return (ctrls', ctrls')
  when (Map.null ctrls') mainQuit

data DirtyChangedHandlerRecord = DirtyChangedHandlerRecord
  { dirtyChangedHandlerOwner :: String
  , dirtyChangedHandlerFn :: DirtyChangedHandler
  }

data FilePathChangedHandlerRecord = FilePathChangedHandlerRecord
  { filePathChangedHandlerOwner :: String
  , filePathChangedHandlerFn :: FilePathChangedHandler
  }

data ModesChangedHandlerRecord = ModesChangedHandlerRecord
  { modesChangedHandlerOwner :: String
  , modesChangedHandlerFn :: ModesChangedHandler
  }

-- | A unique ID that identifies a 'UiCtrlImpl'.
newtype CtrlId = CtrlId Unique
               deriving (Eq, Ord)

-- | The standard instance of 'MonadUiGo', with no frills.
newtype UiGoM a = UiGoM (GoT (State UiGoState) a)
                deriving (Functor, Applicative, Monad, MonadGo, MonadState UiGoState)

instance MonadUiGo UiGoM where
  runUiGo cursor (UiGoM go) =
    let ((value, cursor'), state) = flip runState initialUiGoState $
                                    runGoT go cursor
    in (value, cursor', state)

  uiGoGetState = get
  uiGoPutState = put
  uiGoModifyState = modify

-- | The standard instance of 'UiCtrl'.  See 'StdUiCtrlImpl'.
data UiCtrlImpl go = UiCtrlImpl
  { uiCtrlId :: CtrlId
  , uiAppState :: AppState
  , uiDirty :: IORef Bool
  , uiFilePath :: IORef (Maybe FilePath)
  , uiTools :: IORef (Map ToolType (AnyTool go (UiCtrlImpl go)))
  , uiModes :: IORef UiModes
  , uiCursor :: MVar Cursor

  , uiMainWindow :: IORef (Maybe (MainWindow (UiCtrlImpl go)))
  , uiViews :: IORef (Map ViewId AnyView)

    -- Go monad action-related properties:
  , uiGoRegistrationsByView :: IORef (Map AnyView (Set (AnyEvent go)))
  , uiGoRegistrationsByEvent :: IORef (Map (AnyEvent go) (Set AnyView))
  , uiGoRegistrationsAction :: IORef (go ())

    -- Ui action-related properties:
  , uiDirtyChangedHandlers :: IORef (Map Registration DirtyChangedHandlerRecord)
  , uiFilePathChangedHandlers :: IORef (Map Registration FilePathChangedHandlerRecord)
  , uiModesChangedHandlers :: IORef (Map Registration ModesChangedHandlerRecord)
  }

-- | The standard concrete controller type.  Use this type with 'startBoard',
-- etc.
type StdUiCtrlImpl = UiCtrlImpl UiGoM

instance MonadUiGo go => UiCtrl go (UiCtrlImpl go) where
  readModes = readIORef . uiModes

  modifyModes ui f = do
    oldModes <- readModes ui
    newModes <- f oldModes
    when (newModes /= oldModes) $ do
      writeIORef (uiModes ui) newModes
      let oldToolType = uiToolType oldModes
          newToolType = uiToolType newModes
          toolChanged = newToolType /= oldToolType
      -- If the tool has changed, then follow the order:
      -- 1) Tell the new tool that it is about to become active, so that it can
      --    update the state of its widgets if necessary.
      -- 2) Fire modes changed handlers (causing the old tool's widget to hide
      --    and the new tool's widget, now up-to-date, to become visible).
      -- 3) Tell the old tool that is was deactivated.
      when toolChanged $ do
        AnyTool newTool <- findTool ui newToolType
        toolOnActivating newTool
      fireModesChangedHandlers ui oldModes newModes
      when toolChanged $ do
        AnyTool oldTool <- findTool ui oldToolType
        toolOnDeactivated oldTool

  findTool ui toolType =
    fromMaybe (error $ "UiCtrlImpl.findTool: Couldn't find " ++ show toolType ++ ".") .
    Map.lookup toolType <$>
    readIORef (uiTools ui)

  doUiGo ui go = do
    cursor <- takeMVar (uiCursor ui)
    doUiGo' ui go cursor

  readCursor = readMVar . uiCursor

  isValidMove ui coord = do
    cursor <- readMVar $ uiCursor ui
    return $ isCurrentValidMove (cursorBoard cursor) coord

  playAt ui move = do
    cursor <- takeMVar $ uiCursor ui
    let valid = case move of
          Nothing -> True
          Just coord -> isCurrentValidMove (cursorBoard cursor) coord
    if not valid
      then do
        putMVar (uiCursor ui) cursor
        mainWindow <- getMainWindow ui
        dialog <- messageDialogNew (Just mainWindow)
                                   [DialogModal, DialogDestroyWithParent]
                                   MessageError
                                   ButtonsOk
                                   "Illegal move."
        dialogRun dialog
        widgetDestroy dialog
      else case cursorChildPlayingAt move cursor of
        Just child -> doUiGo' ui (Monad.goDown $ cursorChildIndex child) cursor
        Nothing ->
          let board = cursorBoard cursor
              player = boardPlayerTurn board
              index = length $ cursorChildren cursor
              child = emptyNode { nodeProperties = [moveToProperty player move] }
          in doUiGo' ui (Monad.addChildAt index child >> Monad.goDown index) cursor

  goUp ui = doUiGo ui $ do
    cursor <- Monad.getCursor
    if isNothing $ cursorParent cursor
      then return False
      else Monad.goUp >> return True

  goDown ui index = doUiGo ui $ do
    cursor <- Monad.getCursor
    if null $ drop index $ cursorChildren cursor
      then return False
      else Monad.goDown index >> return True

  goLeft ui = doUiGo ui $ do
    cursor <- Monad.getCursor
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return False
      (Just _, 0) -> return False
      (Just _, n) -> do Monad.goUp
                        Monad.goDown $ n - 1
                        return True

  goRight ui = doUiGo ui $ do
    cursor <- Monad.getCursor
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return False
      (Just parent, n) | n == cursorChildCount parent - 1 -> return False
      (Just _, n) -> do Monad.goUp
                        Monad.goDown $ n + 1
                        return True

  register view events = do
    let ui = viewCtrl view
        view' = AnyView view

    -- Ensure that the view is in the controller's id -> view map.
    modifyIORef (uiViews ui) $ \views ->
      if Map.member (viewId view) views
      then views
      else Map.insert (viewId view) view' views

    -- Go ahead and connect the event to the view.
    byView <- readIORef $ uiGoRegistrationsByView ui
    byEvent <- readIORef $ uiGoRegistrationsByEvent ui
    let duplicates = Map.member view' byView
    when duplicates $
      uiLogWarning $ "UiCtrlImpl.register: A " ++ viewName view ++
      " view registered multiple times.  Overwriting previous registration(s)."
    writeIORef (uiGoRegistrationsByView ui) $
      Map.alter (Just .
                 (flip .) foldr Set.insert events .
                 fromMaybe Set.empty)
                view'
                byView
    writeIORef (uiGoRegistrationsByEvent ui) $
      foldr (Map.alter $ Just . maybe (Set.singleton view')
                                      (Set.insert view'))
            byEvent
            events
    -- TODO Don't need to fully rebuild the action.  We can append to it.
    rebuildGoRegistrationsAction ui

  unregister view event = do
    let ui = viewCtrl view
        view' = AnyView view
    byView <- readIORef $ uiGoRegistrationsByView ui
    byEvent <- readIORef $ uiGoRegistrationsByEvent ui
    let byView' = Map.update (\events ->
                               let events' = Set.delete event events
                               in if Set.null events' then Nothing else Just events')
                  view'
                  byView
        byEvent' = Map.update (\views ->
                                let views' = Set.delete view' views
                                in if Set.null views' then Nothing else Just views')
                   event
                   byEvent
    writeIORef (uiGoRegistrationsByView ui) byView'
    writeIORef (uiGoRegistrationsByEvent ui) byEvent'

    -- If there are no more events registered for the view, then remove it from
    -- the list of known views.
    when (isNothing $ Map.lookup view' byView') $
      modifyIORef (uiViews ui) $ Map.delete $ viewId view

    rebuildGoRegistrationsAction ui
    return $ maybe False (Set.member event) (Map.lookup view' byView) ||
             maybe False (Set.member view') (Map.lookup event byEvent)

  unregisterAll view =
    let ui = viewCtrl view
    in readIORef (uiGoRegistrationsByView ui) >>=
       Foldable.mapM_ (mapM_ (unregister view) . Set.elems) .
       Map.lookup (AnyView view)

  registeredHandlers =
    fmap (concatMap (\(view, events) ->
                      let viewStr = show view
                      in for (Set.elems events) $ \event -> (viewStr, show event)) .
          Map.assocs) .
    readIORef .
    uiGoRegistrationsByView

  registerModesChangedHandler ui owner handler = do
    unique <- newUnique
    modifyIORef (uiModesChangedHandlers ui) $ Map.insert unique
      ModesChangedHandlerRecord { modesChangedHandlerOwner = owner
                                , modesChangedHandlerFn = handler
                                }
    return unique

  unregisterModesChangedHandler ui unique = do
    handlers <- readIORef $ uiModesChangedHandlers ui
    let (handlers', found) = if Map.member unique handlers
                             then (Map.delete unique handlers, True)
                             else (handlers, False)
    when found $ writeIORef (uiModesChangedHandlers ui) handlers'
    return found

  registeredModesChangedHandlers =
    liftM (map modesChangedHandlerOwner . Map.elems) . readIORef . uiModesChangedHandlers

  getMainWindow = fmap MainWindow.myWindow . getMainWindow'

  openBoard maybeUi maybePath rootNode = do
    ctrlId <- fmap CtrlId newUnique
    appState <- maybe newAppState (return . uiAppState) maybeUi
    dirty <- newIORef False
    filePath <- newIORef maybePath
    toolsRef <- newIORef Map.empty
    modesVar <- newIORef defaultUiModes
    cursorVar <- newMVar $ rootCursor rootNode
    mainWindowRef <- newIORef Nothing
    views <- newIORef Map.empty
    goRegistrationsByView <- newIORef Map.empty
    goRegistrationsByEvent <- newIORef Map.empty
    goRegistrationsAction <- newIORef $ return ()
    dirtyChangedHandlers <- newIORef Map.empty
    filePathChangedHandlers <- newIORef Map.empty
    modesChangedHandlers <- newIORef Map.empty

    let ui = UiCtrlImpl { uiCtrlId = ctrlId
                        , uiAppState = appState
                        , uiDirty = dirty
                        , uiFilePath = filePath
                        , uiTools = toolsRef
                        , uiModes = modesVar
                        , uiCursor = cursorVar
                        , uiMainWindow = mainWindowRef
                        , uiViews = views
                        , uiGoRegistrationsByView = goRegistrationsByView
                        , uiGoRegistrationsByEvent = goRegistrationsByEvent
                        , uiGoRegistrationsAction = goRegistrationsAction
                        , uiDirtyChangedHandlers = dirtyChangedHandlers
                        , uiFilePathChangedHandlers = filePathChangedHandlers
                        , uiModesChangedHandlers = modesChangedHandlers
                        }

    appStateRegister appState ui
    rebuildGoRegistrationsAction ui

    createTools ui >>= writeIORef toolsRef
    readTool ui >>= \(AnyTool tool) -> toolOnActivating tool

    mainWindow <- MainWindow.create ui
    writeIORef mainWindowRef $ Just mainWindow
    MainWindow.display mainWindow
    return ui

  fileOpen ui = do
    dialog <- fileChooserDialogNew (Just "Open a file")
                                   Nothing
                                   FileChooserActionOpen
                                   [(stockCancel, ResponseCancel),
                                    (stockOpen, ResponseOk)]
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

  fileSave ui = do
    cursor <- readCursor ui
    maybePath <- getFilePath ui
    case maybePath of
      Nothing -> fileSaveAs ui
      Just path ->
        -- TODO Exception handling when the write fails.
        -- TODO Don't just write a single tree.
        -- TODO Only save when dirty?  (Be careful not to break Save As on a non-dirty game.)
        case runRender $
             renderCollection Collection { collectionTrees = [cursorNode $ cursorRoot cursor] } of
          Left message -> do
            dialog <- messageDialogNew Nothing
                      [DialogModal, DialogDestroyWithParent]
                      MessageError
                      ButtonsOk
                      ("Error serializing game tree:\n\n" ++ message)
            dialogRun dialog
            widgetDestroy dialog
            return False
          Right sgf -> do
            writeFile path sgf
            setDirty ui False
            return True

  fileSaveAs ui = do
    dialog <- fileChooserDialogNew (Just "Save file as")
                                   Nothing
                                   FileChooserActionSave
                                   [(stockCancel, ResponseCancel),
                                    (stockSave, ResponseOk)]
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

  fileClose ui = do
    close <- confirmFileClose ui
    when close $ fileCloseSilently ui
    return close

  fileCloseSilently ui = do
    MainWindow.destroy =<< getMainWindow' ui
    fmap Map.elems (readIORef $ uiTools ui) >>= mapM_ (\(AnyTool tool) -> toolDestroy tool)
    writeIORef (uiTools ui) Map.empty

    appStateUnregister (uiAppState ui) ui

  fileQuit ui = do
    ctrls <- fmap Map.elems $ readMVar $ appControllers $ uiAppState ui
    okayToClose <- andM $ for ctrls $ \(AnyUiCtrl ctrl) -> confirmFileClose ctrl
    when okayToClose $ forM_ ctrls $ \(AnyUiCtrl ctrl) -> fileCloseSilently ctrl
    return okayToClose

  editCutNode ui = do
    initialCursor <- readCursor ui
    case cursorParent initialCursor of
      Nothing -> uiLogWarning "UiCtrlImpl.editCutNode: Can't cut the root node."
      Just _ -> do
        success <- editCopyNode' ui
        when success $ doUiGo ui $ do
          cursor <- Monad.getCursor
          when (isJust $ cursorParent cursor) $ do
            let index = cursorChildIndex cursor
            Monad.goUp
            Monad.deleteChildAt index
            return ()

  editCopyNode = void . editCopyNode'

  editPasteNode ui = do
    clipboard <- getClipboard
    clipboardRequestText clipboard $ \maybeText -> case maybeText of
      Nothing -> return ()
      Just text -> unless (null text || all isSpace text) $ do
        rootInfo <- gameInfoRootInfo . boardGameInfo . cursorBoard <$> readCursor ui
        case parseSubtree rootInfo text of
          Left error -> do
            let (textBeginning, textRest) = splitAt 500 text
            mainWindow <- getMainWindow ui
            dialog <- messageDialogNew (Just mainWindow)
                      [DialogModal, DialogDestroyWithParent]
                      MessageError
                      ButtonsOk
                      ("Unable to parse the clipboard as an SGF game tree.\n\nError: " ++
                       error ++ "\n\nInput:\n" ++ textBeginning ++
                       if not $ null textRest then "\n(truncated)" else "")
            dialogRun dialog
            widgetDestroy dialog
          Right node -> doUiGo ui $ Monad.addChild node

  helpAbout _ = do
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

  getFilePath = readIORef . uiFilePath

  setFilePath ui path = do
    oldPath <- readIORef $ uiFilePath ui
    writeIORef (uiFilePath ui) path
    handlers <- readIORef $ uiFilePathChangedHandlers ui
    forM_ (Map.elems handlers) $ \record ->
      filePathChangedHandlerFn record oldPath path

  registerFilePathChangedHandler ui owner fireImmediately handler = do
    unique <- newUnique
    modifyIORef (uiFilePathChangedHandlers ui) $ Map.insert unique
      FilePathChangedHandlerRecord { filePathChangedHandlerOwner = owner
                                   , filePathChangedHandlerFn = handler
                                   }
    when fireImmediately $ do
      path <- getFilePath ui
      handler path path
    return unique

  unregisterFilePathChangedHandler ui unique = do
    handlers <- readIORef $ uiFilePathChangedHandlers ui
    let (handlers', found) = if Map.member unique handlers
                             then (Map.delete unique handlers, True)
                             else (handlers, False)
    when found $ writeIORef (uiFilePathChangedHandlers ui) handlers'
    return found

  registeredFilePathChangedHandlers =
    liftM (map filePathChangedHandlerOwner . Map.elems) . readIORef . uiFilePathChangedHandlers

  getDirty = readIORef . uiDirty

  setDirty ui newDirty = do
    oldDirty <- readIORef $ uiDirty ui
    when (newDirty /= oldDirty) $ do
      writeIORef (uiDirty ui) newDirty
      handlers <- readIORef $ uiDirtyChangedHandlers ui
      forM_ (map dirtyChangedHandlerFn $ Map.elems handlers) ($ newDirty)

  registerDirtyChangedHandler ui owner fireImmediately handler = do
    unique <- newUnique
    modifyIORef (uiDirtyChangedHandlers ui) $ Map.insert unique
      DirtyChangedHandlerRecord { dirtyChangedHandlerOwner = owner
                                , dirtyChangedHandlerFn = handler
                                }
    when fireImmediately $ do
      dirty <- readIORef $ uiDirty ui
      handler dirty
    return unique

  unregisterDirtyChangedHandler ui unique = do
    handlers <- readIORef $ uiDirtyChangedHandlers ui
    let (handlers', found) = if Map.member unique handlers
                             then (Map.delete unique handlers, True)
                             else (handlers, False)
    when found $ writeIORef (uiDirtyChangedHandlers ui) handlers'
    return found

  registeredDirtyChangedHandlers =
    liftM (map dirtyChangedHandlerOwner . Map.elems) . readIORef . uiDirtyChangedHandlers

-- | 'doUiGo' for 'UiCtrlImpl' is implemented by taking the cursor MVar, running
-- a Go action, putting the MVar, then running follow-up tasks.  Many types of
-- actions the UI wants to perform need to be able to take the cursor
-- themselves, do some logic, then pass it off to run a Go action, re-put, and
-- perform subsequent UI tasks.  This function is a helper for such UI code.
doUiGo' :: MonadUiGo go => UiCtrlImpl go -> go a -> Cursor -> IO a
doUiGo' ui go cursor = do
  goRegistrationsAction <- readIORef $ uiGoRegistrationsAction ui
  let (value, cursor', state) = runUiGo cursor (goRegistrationsAction >> go)
      staleViews = uiGoViewsToUpdate state
  putMVar (uiCursor ui) cursor'
  when (uiGoMakesDirty state) $ setDirty ui True
  unless (Set.null staleViews) $ do
    viewMap <- readIORef $ uiViews ui
    forM_ (Set.elems staleViews) $ \viewId -> case Map.lookup viewId viewMap of
      Just (AnyView view) -> viewUpdate view
      Nothing -> uiLogWarning "doUiGo': Asked to update an unknown view."
  return value

startBoard :: MonadUiGo go => Node -> IO (UiCtrlImpl go)
startBoard = openBoard Nothing Nothing

startNewBoard :: MonadUiGo go => Maybe (Int, Int) -> IO (UiCtrlImpl go)
startNewBoard = openNewBoard Nothing

startFile :: MonadUiGo go => FilePath -> IO (Either String (UiCtrlImpl go))
startFile = openFile Nothing

rebuildGoRegistrationsAction :: MonadUiGo go => UiCtrlImpl go -> IO ()
rebuildGoRegistrationsAction ui =
  readIORef (uiGoRegistrationsByEvent ui) >>=
  writeIORef (uiGoRegistrationsAction ui) . buildAction
  where buildAction =
          foldl' (\m (AnyEvent event, views) ->
                   m >> on0 event (forM_ (Set.elems views) $ \(AnyView view) ->
                                    uiGoUpdateView $ viewId view))
                 commonAction .
          Map.assocs
        commonAction = do
          -- TODO This really calls for some sort of event hierarchy, so
          -- that we can listen for all mutating events here, rather than
          -- making it easy to forget to add new events here.
          on0 childAddedEvent uiGoMakeDirty
          on0 childDeletedEvent uiGoMakeDirty
          on0 propertiesModifiedEvent uiGoMakeDirty

fireModesChangedHandlers :: UiCtrlImpl go -> UiModes -> UiModes -> IO ()
fireModesChangedHandlers ui oldModes newModes = do
  handlers <- readIORef $ uiModesChangedHandlers ui
  forM_ (Map.elems handlers) $ \handler ->
    modesChangedHandlerFn handler oldModes newModes

-- | Retrieves the 'MainWindow' owned by the controller.  It is an error to call
-- this before the main window has been set up.
getMainWindow' :: UiCtrlImpl go -> IO (MainWindow (UiCtrlImpl go))
getMainWindow' ui = join $
                    fmap (maybe (fail "getMainWindow: No window available.") return) $
                    readIORef $
                    uiMainWindow ui

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

-- | Should be called before destroying the main window.  Checks the dirty
-- state of UI; if dirty, then a dialog prompts the user whether the game
-- should be saved before closing, and giving the option to cancel closing.
-- Returns true if the window should be closed.
confirmFileClose :: UiCtrl go ui => ui -> IO Bool
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
            dialogAddButton dialog "Close without saving" ResponseNo
            dialogAddButton dialog stockCancel ResponseCancel
            dialogAddButton dialog (maybe stockSaveAs (const stockSave) filePath) ResponseYes
            result <- dialogRun dialog
            widgetDestroy dialog
            case result of
              ResponseYes -> fileSave ui
              ResponseNo -> return True
              _ -> return False
    else return True

-- | Attempts to copy the current node to the clipboard.  Returns true if
-- successful.  If not, this presents a model dialog describing the error, waits
-- for the user to click through, then returns false.
editCopyNode' :: MonadUiGo go => UiCtrlImpl go -> IO Bool
editCopyNode' ui = do
  clipboard <- getClipboard
  cursor <- readCursor ui
  case runRender $ renderGameTree $ cursorNode cursor of
    Right sgf -> do
      clipboardSetText clipboard sgf
      return True
    Left error -> do
      mainWindow <- getMainWindow ui
      dialog <- messageDialogNew (Just mainWindow)
                                 [DialogModal, DialogDestroyWithParent]
                                 MessageError
                                 ButtonsOk
                                 ("Error rendering node for copy:\n\n" ++ error)
      dialogRun dialog
      widgetDestroy dialog
      return False

-- | Returns the clipboard we'll use for explicit cut/copy/paste actions.
getClipboard :: IO Clipboard
getClipboard = clipboardGet selectionClipboard

-- | Attempts to read the project's license file.  If successful, the license is
-- returend, otherwise a fallback message is returned instead.
readLicense :: IO (Maybe String)
readLicense = do
  path <- Paths.getDataFileName "LICENSE"
  fmap Just (readFile path) `Control.Exception.catch` \(_ :: IOException) -> return Nothing

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

-- | Logs a warning to stderr.
uiLogWarning :: String -> IO ()
uiLogWarning msg = hPutStrLn stderr $ applicationName ++ " WARNING: " ++ msg
