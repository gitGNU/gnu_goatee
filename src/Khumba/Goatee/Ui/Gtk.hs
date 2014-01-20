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
import Data.Unique (newUnique)
import Graphics.UI.Gtk (ButtonsType(..), DialogFlags(..), MessageType(..), dialogRun, mainQuit, messageDialogNew, widgetDestroy)
import qualified Khumba.Goatee.Sgf as Sgf
import Khumba.Goatee.Sgf
import qualified Khumba.Goatee.Sgf.Monad as Monad
import Khumba.Goatee.Sgf.Monad (on, Event)
import Khumba.Goatee.Ui.Gtk.Common
import qualified Khumba.Goatee.Ui.Gtk.MainWindow as MainWindow

data UiHandler = forall handler. UiHandler (Event UiGoM handler) handler

data UiCtrlImpl = UiCtrlImpl { uiAppState :: AppState
                             , uiModes :: IORef UiModes
                             , uiCursor :: MVar Cursor

                               -- Go monad action-related properties:
                             , uiHandlers :: IORef (Map Registration UiHandler)
                             , uiBaseAction :: IORef (UiGoM ())

                               -- Ui action-related properties:
                             , uiModesChangedHandlers :: IORef (Map Registration ModesChangedHandler)
                             }

instance UiCtrl UiCtrlImpl where
  readModes = readIORef . uiModes

  modifyModes ui f = do
    oldModes <- readModes ui
    newModes <- f oldModes
    unless (newModes == oldModes) $ do
      writeIORef (uiModes ui) newModes
      fireModesChangedHandlers ui oldModes newModes

  readCursor = readMVar . uiCursor

  isValidMove ui coord = do
    cursor <- readMVar $ uiCursor ui
    return $ Sgf.isCurrentValidMove (cursorBoard cursor) coord

  playAt ui coord = modifyMVar_ (uiCursor ui) $ \cursor ->
    if not $ Sgf.isCurrentValidMove (cursorBoard cursor) coord
    then do
      dialog <- messageDialogNew Nothing
                                 [DialogModal, DialogDestroyWithParent]
                                 MessageError
                                 ButtonsOk
                                 "Illegal move."
      dialogRun dialog
      widgetDestroy dialog
      return cursor
    else case cursorChildPlayingAt coord cursor of
      Just child -> goDown ui (cursorChildIndex child) >> return child
      Nothing -> do
        let board = cursorBoard cursor
            player = boardPlayerTurn board
            child = emptyNode { nodeProperties = [colorToMove player coord] }
            cursor' = cursorModifyNode (addChild child) cursor  -- TODO WIP, should fire some event...
        goDown ui $ length (nodeChildren $ cursorNode cursor') - 1
        readCursor ui

  goUp ui = modifyMVar (uiCursor ui) $ \cursor ->
    case cursorParent cursor of
      Nothing -> return (cursor, False)
      Just _ -> execute ui cursor Monad.goUp

  goDown ui index = modifyMVar (uiCursor ui) $ \cursor ->
    if null $ drop index $ cursorChildren cursor
      then return (cursor, False)
      else execute ui cursor $ Monad.goDown index

  -- TODO Don't queue a second draw because of the intermediate parent state
  -- (maybe only one draw is actually queued?).
  goLeft ui = modifyMVar (uiCursor ui) $ \cursor ->
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return (cursor, False)
      (Just _, 0) -> return (cursor, False)
      (Just _, n) -> execute ui cursor $ Monad.goUp >> Monad.goDown (n - 1)

  -- TODO Don't queue a second draw because of the intermediate parent state
  -- (maybe only one draw is actually queued?).
  goRight ui = modifyMVar (uiCursor ui) $ \cursor ->
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return (cursor, False)
      (Just parent, n) | n == cursorChildCount parent - 1 -> return (cursor, False)
      (Just _, n) -> execute ui cursor $ Monad.goUp >> Monad.goDown (n + 1)

  register ui event handler = do
    unique <- newUnique
    modifyIORef (uiHandlers ui) $ Map.insert unique $ UiHandler event handler
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

  registeredHandlerCount = liftM Map.size . readIORef . uiHandlers

  registerModesChangedHandler ui handler = do
    unique <- newUnique
    modifyIORef (uiModesChangedHandlers ui) $ Map.insert unique handler
    return unique

  unregisterModesChangedHandler ui unique = do
    handlers <- readIORef $ uiModesChangedHandlers ui
    let (handlers', found) = if Map.member unique handlers
                               then (Map.delete unique handlers, True)
                               else (handlers, False)
    when found $ writeIORef (uiModesChangedHandlers ui) handlers'
    return found

  registeredModesChangedHandlerCount =
    liftM Map.size . readIORef . uiModesChangedHandlers

  windowCountInc ui =
    modifyMVar_ (appWindowCount $ uiAppState ui) (return . (+ 1))

  windowCountDec ui = do
    count <- modifyMVar (appWindowCount $ uiAppState ui) $
             \n -> let m = n - 1 in return (m, m)
    when (count == 0) mainQuit

  openBoard maybeUi rootNode = do
    uiRef' <- newIORef Nothing
    let uiRef = UiRef uiRef'
        cursor = rootCursor rootNode

    appState <- case maybeUi of
      Nothing -> newAppState
      Just ui -> return $ uiAppState ui
    modesVar <- newIORef defaultUiModes
    cursorVar <- newMVar cursor
    mainWindow <- MainWindow.create uiRef

    uiHandlers <- newIORef Map.empty
    baseAction <- newIORef $ return ()
    modesChangedHandlers <- newIORef Map.empty
    let ui = UiCtrlImpl { uiAppState = appState
                        , uiModes = modesVar
                        , uiCursor = cursorVar
                        , uiHandlers = uiHandlers
                        , uiBaseAction = baseAction
                        , uiModesChangedHandlers = modesChangedHandlers
                        }
    writeIORef uiRef' $ Just ui

    -- Do initialization that requires the 'UiCtrl' to be available.
    MainWindow.initialize mainWindow
    readMVar (appWindowCount appState) >>= \n -> unless (n > 0) $
      fail "UiCtrlImpl expected MainWindow to increment the open window count."

    --fireViewCursorChanged mainWindow cursor
    MainWindow.display mainWindow
    return ui

execute :: UiCtrlImpl -> Cursor -> UiGoM () -> IO (Cursor, Bool)
execute ui cursor action = do
  baseAction <- readIORef $ uiBaseAction ui
  let (_, cursor', handlers) = runUiGo (baseAction >> action) cursor
  handlers
  return (cursor', True)

startBoard :: Node -> IO UiCtrlImpl
startBoard = openBoard Nothing

startNewBoard :: Maybe (Int, Int) -> IO UiCtrlImpl
startNewBoard = openNewBoard Nothing

startFile :: String -> IO (Either String UiCtrlImpl)
startFile = openFile Nothing

rebuildBaseAction :: UiCtrlImpl -> IO ()
rebuildBaseAction ui = do
  handlers <- readIORef $ uiHandlers ui
  writeIORef (uiBaseAction ui) $
    foldl' (\io (UiHandler event handler) -> io >> on event handler)
           (return ())
           handlers

fireModesChangedHandlers :: UiCtrlImpl -> UiModes -> UiModes -> IO ()
fireModesChangedHandlers ui oldModes newModes =
  mapM_ (\f -> f oldModes newModes) . Map.elems =<< readIORef (uiModesChangedHandlers ui)
