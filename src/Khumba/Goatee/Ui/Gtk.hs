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
module Khumba.Goatee.Ui.Gtk (
  startBoard,
  startNewBoard,
  startFile,
  ) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, readMVar, putMVar, modifyMVar, modifyMVar_)
import Control.Exception (IOException, catch, finally)
import Control.Monad (forM_, join, liftM, unless, when)
import Data.Foldable (foldl')
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Unique (Unique, newUnique)
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  ButtonsType (ButtonsNone, ButtonsOk, ButtonsYesNo),
  DialogFlags (DialogDestroyWithParent, DialogModal),
  FileChooserAction (FileChooserActionOpen, FileChooserActionSave),
  MessageType (MessageError, MessageQuestion),
  ResponseId (ResponseCancel, ResponseNo, ResponseOk, ResponseYes),
  aboutDialogAuthors, aboutDialogCopyright, aboutDialogLicense, aboutDialogNew,
  aboutDialogProgramName, aboutDialogWebsite,
  dialogAddButton, dialogRun,
  fileChooserAddFilter, fileChooserDialogNew, fileChooserGetFilename,
  mainQuit,
  messageDialogNew,
  stockCancel, stockOpen, stockSave, stockSaveAs,
  widgetDestroy, widgetHide,
  set,
  )
import Khumba.Goatee.App
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Printer
import Khumba.Goatee.Sgf.Tree
import qualified Khumba.Goatee.Sgf.Monad as Monad
import Khumba.Goatee.Sgf.Monad (Event, on, childAddedEvent, propertiesChangedEvent)
import Khumba.Goatee.Ui.Gtk.Common
import qualified Khumba.Goatee.Ui.Gtk.MainWindow as MainWindow
import Khumba.Goatee.Ui.Gtk.MainWindow (MainWindow)
import qualified Paths_goatee as Paths
import System.Directory (doesFileExist)

-- | A structure for holding global application information about all open
-- boards.
data AppState = AppState { appControllers :: MVar (Map CtrlId UiCtrlImpl)
                           -- ^ Maps all of the open boards' controllers by
                           -- their IDs.
                         }

-- | Creates an 'AppState' that is holding no controllers.
newAppState :: IO AppState
newAppState = do
  controllers <- newMVar Map.empty
  return AppState { appControllers = controllers }

-- | Registers a 'UiCtrlImpl' with an 'AppState'.
appStateRegister :: AppState -> UiCtrlImpl -> IO ()
appStateRegister appState ui =
  modifyMVar_ (appControllers appState) $ return . Map.insert (uiCtrlId ui) ui

-- | Unregisters a 'UiCtrlImpl' from an 'AppState'.  If the 'AppState' is left
-- with no controllers, then the GTK+ main loop is shut down and the application
-- exits.
appStateUnregister :: AppState -> UiCtrlImpl -> IO ()
appStateUnregister appState ui = do
  ctrls' <- modifyMVar (appControllers appState) $ \ctrls ->
    let ctrls' = Map.delete (uiCtrlId ui) ctrls
    in return (ctrls', ctrls')
  when (Map.null ctrls') mainQuit

data UiHandler = forall handler. UiHandler String (Event UiGoM handler) handler

data DirtyChangedHandlerRecord =
  DirtyChangedHandlerRecord { dirtyChangedHandlerOwner :: String
                            , dirtyChangedHandlerFn :: DirtyChangedHandler
                            }

data FilePathChangedHandlerRecord =
  FilePathChangedHandlerRecord { filePathChangedHandlerOwner :: String
                               , filePathChangedHandlerFn :: FilePathChangedHandler
                               }

data ModesChangedHandlerRecord =
  ModesChangedHandlerRecord { modesChangedHandlerOwner :: String
                            , modesChangedHandlerFn :: ModesChangedHandler
                            }

-- | A unique ID that identifies a 'UiCtrlImpl'.
newtype CtrlId = CtrlId Unique
               deriving (Eq, Ord)

-- | Implementation of 'UiCtrl'.
data UiCtrlImpl = UiCtrlImpl { uiCtrlId :: CtrlId
                             , uiAppState :: AppState
                             , uiDirty :: IORef Bool
                             , uiFilePath :: IORef (Maybe FilePath)
                             , uiModes :: IORef UiModes
                             , uiCursor :: MVar Cursor

                             , uiMainWindow :: IORef (Maybe (MainWindow UiCtrlImpl))

                               -- Go monad action-related properties:
                             , uiHandlers :: IORef (Map Registration UiHandler)
                             , uiBaseAction :: IORef (UiGoM ())

                               -- Ui action-related properties:
                             , uiDirtyChangedHandlers ::
                               IORef (Map Registration DirtyChangedHandlerRecord)
                             , uiFilePathChangedHandlers ::
                               IORef (Map Registration FilePathChangedHandlerRecord)
                             , uiModesChangedHandlers ::
                               IORef (Map Registration ModesChangedHandlerRecord)
                             }

instance UiCtrl UiCtrlImpl where
  readModes = readIORef . uiModes

  modifyModes ui f = do
    oldModes <- readModes ui
    newModes <- f oldModes
    unless (newModes == oldModes) $ do
      writeIORef (uiModes ui) newModes
      fireModesChangedHandlers ui oldModes newModes

  readVariationMode ui = do
    cursor <- readCursor ui
    modes <- readModes ui
    return $
      fromMaybe (rootInfoVariationMode $ gameInfoRootInfo $
                 boardGameInfo $ cursorBoard cursor) $
      uiOverrideVariationMode modes

  runUiGo ui go = do
    cursor <- takeMVar (uiCursor ui)
    runUiGo' ui go cursor

  readCursor = readMVar . uiCursor

  isValidMove ui coord = do
    cursor <- readMVar $ uiCursor ui
    return $ isCurrentValidMove (cursorBoard cursor) coord

  playAt ui coord = do
    cursor <- takeMVar $ uiCursor ui
    if not $ isCurrentValidMove (cursorBoard cursor) coord
      then do
        dialog <- messageDialogNew Nothing
                                   [DialogModal, DialogDestroyWithParent]
                                   MessageError
                                   ButtonsOk
                                   "Illegal move."
        dialogRun dialog
        widgetDestroy dialog
        putMVar (uiCursor ui) cursor
      else case cursorChildPlayingAt coord cursor of
        Just child -> runUiGo' ui (Monad.goDown $ cursorChildIndex child) cursor
        Nothing ->
          let board = cursorBoard cursor
              player = boardPlayerTurn board
              index = length $ cursorChildren cursor
              child = emptyNode { nodeProperties = [colorToMove player coord] }
          in runUiGo' ui (Monad.addChild index child >> Monad.goDown index) cursor

  goUp ui = runUiGo ui $ do
    cursor <- Monad.getCursor
    if isNothing $ cursorParent cursor
      then return False
      else Monad.goUp >> return True

  goDown ui index = runUiGo ui $ do
    cursor <- Monad.getCursor
    if null $ drop index $ cursorChildren cursor
      then return False
      else Monad.goDown index >> return True

  goLeft ui = runUiGo ui $ do
    cursor <- Monad.getCursor
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return False
      (Just _, 0) -> return False
      (Just _, n) -> do Monad.goUp
                        Monad.goDown $ n - 1
                        return True

  goRight ui = runUiGo ui $ do
    cursor <- Monad.getCursor
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return False
      (Just parent, n) | n == cursorChildCount parent - 1 -> return False
      (Just _, n) -> do Monad.goUp
                        Monad.goDown $ n + 1
                        return True

  register ui caller event handler = do
    unique <- newUnique
    modifyIORef (uiHandlers ui) $ Map.insert unique $ UiHandler caller event handler
    modifyIORef (uiBaseAction ui) (>> on event handler)
    return unique

  unregister ui unique = do
    handlers <- readIORef $ uiHandlers ui
    let (handlers', found) = if Map.member unique handlers
                               then (Map.delete unique handlers, True)
                               else (handlers, False)
    when found $ do
      writeIORef (uiHandlers ui) handlers'
      rebuildBaseAction ui
    return found

  registeredHandlers =
    liftM (map (\(UiHandler owner event _) -> (owner, show event)) . Map.elems) .
    readIORef .
    uiHandlers

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
    modesVar <- newIORef defaultUiModes
    cursorVar <- newMVar $ rootCursor rootNode
    mainWindowRef <- newIORef Nothing
    uiHandlers <- newIORef Map.empty
    baseAction <- newIORef $ return ()
    dirtyChangedHandlers <- newIORef Map.empty
    filePathChangedHandlers <- newIORef Map.empty
    modesChangedHandlers <- newIORef Map.empty

    let ui = UiCtrlImpl { uiCtrlId = ctrlId
                        , uiAppState = appState
                        , uiDirty = dirty
                        , uiFilePath = filePath
                        , uiModes = modesVar
                        , uiCursor = cursorVar
                        , uiMainWindow = mainWindowRef
                        , uiHandlers = uiHandlers
                        , uiBaseAction = baseAction
                        , uiDirtyChangedHandlers = dirtyChangedHandlers
                        , uiFilePathChangedHandlers = filePathChangedHandlers
                        , uiModesChangedHandlers = modesChangedHandlers
                        }

    appStateRegister appState ui
    rebuildBaseAction ui

    mainWindow <- MainWindow.create ui
    writeIORef mainWindowRef $ Just mainWindow
    MainWindow.display mainWindow
    return ui

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

  fileClose ui = do
    close <- confirmFileClose ui
    when close $ closeBoard ui
    return close

  fileQuit ui = do
    ctrls <- fmap Map.elems $ readMVar $ appControllers $ uiAppState ui
    okayToClose <- andM $ map confirmFileClose ctrls
    when okayToClose $ mapM_ closeBoard ctrls
    return okayToClose

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
    unless (oldDirty == newDirty) $ do
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

-- | 'runUiGo' for 'UiCtrlImpl' is implemented by taking the cursor MVar,
-- running a Go action, putting the MVar, then running handlers.  Many types of
-- actions the UI wants to perform need to be able to take the cursor
-- themselves, do some logic, then pass it off to run a Go action, re-put, and
-- call handlers.  This function is a helper for such UI code.
runUiGo' :: UiCtrlImpl -> UiGoM a -> Cursor -> IO a
runUiGo' ui go cursor = do
  baseAction <- readIORef $ uiBaseAction ui
  let (value, cursor', handlers) = runUiGoPure (baseAction >> go) cursor
  putMVar (uiCursor ui) cursor'
  handlers
  return value

startBoard :: Node -> IO UiCtrlImpl
startBoard = openBoard Nothing Nothing

startNewBoard :: Maybe (Int, Int) -> IO UiCtrlImpl
startNewBoard = openNewBoard Nothing

startFile :: FilePath -> IO (Either String UiCtrlImpl)
startFile = openFile Nothing

rebuildBaseAction :: UiCtrlImpl -> IO ()
rebuildBaseAction ui =
  readIORef (uiHandlers ui) >>= writeIORef (uiBaseAction ui) . buildBaseAction
  where buildBaseAction =
          foldl' (\io (UiHandler _ event handler) -> io >> on event handler)
                 commonAction
        commonAction = do
          -- TODO This really calls for some sort of event hierarchy, so
          -- that we can listen for all mutating events here, rather than
          -- making it easy to forget to add new events here.
          on childAddedEvent $ \_ _ -> setDirtyTrue
          on propertiesChangedEvent $ \_ _ -> setDirtyTrue
        setDirtyTrue = afterGo $ setDirty ui True

fireModesChangedHandlers :: UiCtrlImpl -> UiModes -> UiModes -> IO ()
fireModesChangedHandlers ui oldModes newModes = do
  handlers <- readIORef $ uiModesChangedHandlers ui
  forM_ (Map.elems handlers) $ \handler ->
    modesChangedHandlerFn handler oldModes newModes

-- | Retrieves the 'MainWindow' owned by the controller.  It is an error to call
-- this before the main window has been set up.
getMainWindow' :: UiCtrlImpl -> IO (MainWindow UiCtrlImpl)
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

-- | Hides and releases the game's 'Khumba.Goatee.Ui.Gtk.MainWindow', and shuts
-- down the UI controller (in effect closing the game, with no prompting).  If
-- this is the last board open, then the application will exit.
closeBoard :: UiCtrlImpl -> IO ()
closeBoard ui = do
  MainWindow.destroy =<< getMainWindow' ui
  appStateUnregister (uiAppState ui) ui

-- | Attempts to read the project's license file.  If successful, the license is
-- returend, otherwise a fallback message is returned instead.
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
