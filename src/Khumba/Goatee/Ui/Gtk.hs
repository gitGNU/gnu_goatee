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
  startBoard
  , startNewBoard
  , startFile
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Foldable (foldl')
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Unique (newUnique)
import Graphics.UI.Gtk hiding (Cursor, on)
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Tree
import qualified Khumba.Goatee.Sgf.Monad as Monad
import Khumba.Goatee.Sgf.Monad (Event, on, childAddedEvent, propertiesChangedEvent)
import Khumba.Goatee.Ui.Gtk.Common
import qualified Khumba.Goatee.Ui.Gtk.MainWindow as MainWindow

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

data UiCtrlImpl = UiCtrlImpl { uiAppState :: AppState
                             , uiDirty :: IORef Bool
                             , uiFilePath :: IORef (Maybe FilePath)
                             , uiModes :: IORef UiModes
                             , uiCursor :: MVar Cursor

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

  windowCountInc ui =
    modifyMVar_ (appWindowCount $ uiAppState ui) (return . (+ 1))

  windowCountDec ui = do
    count <- modifyMVar (appWindowCount $ uiAppState ui) $
             \n -> let m = n - 1 in return (m, m)
    when (count == 0) mainQuit

  openBoard maybeUi maybePath rootNode = do
    uiRef' <- newIORef Nothing
    let uiRef = UiRef uiRef'
        cursor = rootCursor rootNode

    appState <- case maybeUi of
      Nothing -> newAppState
      Just ui -> return $ uiAppState ui
    modesVar <- newIORef defaultUiModes
    cursorVar <- newMVar cursor
    mainWindow <- MainWindow.create uiRef

    dirty <- newIORef False
    filePath <- newIORef maybePath
    uiHandlers <- newIORef Map.empty
    baseAction <- newIORef $ buildBaseAction (readUiRef uiRef) Map.empty
    dirtyChangedHandlers <- newIORef Map.empty
    filePathChangedHandlers <- newIORef Map.empty
    modesChangedHandlers <- newIORef Map.empty
    let ui = UiCtrlImpl { uiAppState = appState
                        , uiDirty = dirty
                        , uiFilePath = filePath
                        , uiModes = modesVar
                        , uiCursor = cursorVar
                        , uiHandlers = uiHandlers
                        , uiBaseAction = baseAction
                        , uiDirtyChangedHandlers = dirtyChangedHandlers
                        , uiFilePathChangedHandlers = filePathChangedHandlers
                        , uiModesChangedHandlers = modesChangedHandlers
                        }
    writeIORef uiRef' $ Just ui

    -- Do initialization that requires the 'UiCtrl' to be available.
    MainWindow.initialize mainWindow
    readMVar (appWindowCount appState) >>= \n -> unless (n > 0) $
      fail "UiCtrlImpl expected MainWindow to increment the open window count."

    MainWindow.display mainWindow
    return ui

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

buildBaseAction :: IO UiCtrlImpl -> Map Registration UiHandler -> UiGoM ()
buildBaseAction uiGetter handlers =
  let setDirtyTrue = afterGo $ do
        ui <- uiGetter
        setDirty ui True
  in foldl' (\io (UiHandler _ event handler) -> io >> on event handler)
            (do -- TODO This really calls for some sort of event hierarchy, so
                -- that we can listen for all mutating events here, rather than
                -- making it easy to forget to add new events here.
                on childAddedEvent $ \_ _ -> setDirtyTrue
                on propertiesChangedEvent $ \_ _ -> setDirtyTrue)
            handlers

rebuildBaseAction :: UiCtrlImpl -> IO ()
rebuildBaseAction ui =
  readIORef (uiHandlers ui) >>=
  writeIORef (uiBaseAction ui) . buildBaseAction (return ui)

fireModesChangedHandlers :: UiCtrlImpl -> UiModes -> UiModes -> IO ()
fireModesChangedHandlers ui oldModes newModes = do
  handlers <- readIORef $ uiModesChangedHandlers ui
  forM_ (Map.elems handlers) $ \handler ->
    modesChangedHandlerFn handler oldModes newModes
