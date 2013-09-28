module Khumba.GoHS.Ui.Gtk where

import Control.Concurrent.MVar.Strict
import Data.IORef
import Data.List (find)
import Data.Maybe
import Graphics.UI.Gtk (ButtonsType(..), DialogFlags(..), MessageType(..), Window, dialogRun, messageDialogNew, widgetDestroy, windowSetTitle)
import Khumba.GoHS.Sgf
import Khumba.GoHS.Sgf.Parser
import Khumba.GoHS.Ui.Gtk.Board
import Khumba.GoHS.Ui.Gtk.Common

data UiStateImpl = UiStateImpl { uiBoard :: GtkBoard UiStateImpl
                               , uiCursor :: MVar Cursor
                               }

isValidMove' :: Cursor -> Coord -> Bool
isValidMove' cursor coord =
  let board = cursorBoard cursor
      player = boardPlayerTurn board
      result = applyMove standardGoMoveParams player coord board
  in isJust $ getApplyMoveResult' result

instance UiState UiStateImpl where
  readCursor = readMVar . uiCursor

  isValidMove ui coord = do
    cursor <- readMVar $ uiCursor ui
    return $ isValidMove' cursor coord

  playAt ui coord = modifyMVar_ (uiCursor ui) $ \cursor ->
    if not $ isValidMove' cursor coord
    then do
      dialog <- messageDialogNew (Just $ gtkBoardWindow $ uiBoard ui)
                                 [DialogModal, DialogDestroyWithParent]
                                 MessageError
                                 ButtonsOk
                                 "Illegal move."
      dialogRun dialog
      widgetDestroy dialog
      return cursor
    else case cursorChildPlayingAt coord cursor of
      Just child -> goToChild ui child >> return child
      Nothing -> do
        let board = cursorBoard cursor
            player = boardPlayerTurn board
            child = emptyNode { nodeProperties = [colorToMove player coord] }
            cursor' = cursorModifyNode (addChild child) cursor
            childCursor = cursorChild cursor' $ length (nodeChildren $ cursorNode cursor') - 1
        goReplace ui cursor'  -- TODO Don't draw twice.
        goToChild ui childCursor
        return childCursor

  goUp ui = modifyMVar (uiCursor ui) $ \cursor ->
    case cursorParent cursor of
      Nothing -> return (cursor, False)
      Just parent -> goToParent ui parent >> return (parent, True)

  goDown ui = modifyMVar (uiCursor ui) $ \cursor ->
    case cursorChildren cursor of
      child:_ -> goToChild ui child >> return (child, True)
      _ -> return (cursor, False)

  goLeft ui = modifyMVar (uiCursor ui) $ \cursor ->
    case cursorParent cursor of
      Nothing -> return (cursor, False)
      Just parentCursor ->
        let index = cursorChildIndex cursor
        in if index == 0
           then return (cursor, False)
           else let left = cursorChild parentCursor $ index - 1
                in goToLeft ui left >> return (left, True)

  goRight ui = modifyMVar (uiCursor ui) $ \cursor ->
    case cursorParent cursor of
      Nothing -> return (cursor, False)
      Just parentCursor ->
        let index = cursorChildIndex cursor
        in if index == length (nodeChildren $ cursorNode parentCursor) - 1
           then return (cursor, False)
           else let right = cursorChild parentCursor $ index + 1
                in goToLeft ui right >> return (right, True)

isPropertyBOrW :: Property -> Bool
isPropertyBOrW prop = case prop of
  B _ -> True
  W _ -> True
  _ -> False

goToParent :: UiStateImpl -> Cursor -> IO ()
goToParent = updateUi

goToChild :: UiStateImpl -> Cursor -> IO ()
goToChild = updateUi

goToLeft :: UiStateImpl -> Cursor -> IO ()
goToLeft = updateUi

goToRight :: UiStateImpl -> Cursor -> IO ()
goToRight = updateUi

goReplace :: UiStateImpl -> Cursor -> IO ()
goReplace = updateUi

updateUi :: UiStateImpl -> Cursor -> IO ()
updateUi ui cursor = do
  goBoardWidgetUpdate cursor (uiBoard ui)
  return ()

openBoard :: Node -> IO UiStateImpl
openBoard rootNode = do
  let cursor = either (error . ("Error creating root cursor: " ++)) id $
               rootCursor rootNode
      board = cursorBoard cursor
      width = boardWidth board
      height = boardHeight board
  uiRef <- newIORef Nothing
  let uiRef' = UiRef uiRef

  uiBoard <- gtkBoardNew uiRef' width height

  cursorVar <- newMVar cursor
  let ui = UiStateImpl { uiBoard = uiBoard
                       , uiCursor = cursorVar
                       }
  writeIORef uiRef $ Just ui

  goBoardWidgetUpdate cursor uiBoard
  gtkBoardShow uiBoard
  return ui

openFile :: String -> IO (Either ParseError UiStateImpl)
-- TODO Don't only choose the first tree in the collection.
openFile file = do
  result <- parseFile file
  case result of
    Right trees -> fmap Right $ openBoard $ head trees
    Left err -> return $ Left err
