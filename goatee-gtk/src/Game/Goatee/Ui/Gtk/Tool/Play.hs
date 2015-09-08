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

module Game.Goatee.Ui.Gtk.Tool.Play (PlayTool, create) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust)
import Game.Goatee.Common
import qualified Game.Goatee.Lib.Board as Board
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Graphics.UI.Gtk (
  Button, buttonActivated, buttonNewWithLabel, on, toWidget,
  )

-- | A 'UiTool' for entering moves to record a game.  When this tool is active,
-- clicking (not dragging) on a board point will make a move at the point if the
-- move is valid.  A pass button is shown in the panel.
data PlayTool ui = PlayTool
  { myUi :: ui
  , myViewState :: ViewState
  , myToolState :: ToolState
  , myPassButton :: Button
  , myIsValidMoveCache :: IORef (Maybe (Coord, Bool))
  }

instance UiCtrl go ui => UiView go ui (PlayTool ui) where
  viewName = const "PlayTool"

  viewCtrl = myUi

  viewState = myViewState

  viewUpdate = const $ return ()

instance UiCtrl go ui => UiTool go ui (PlayTool ui) where
  toolState = myToolState

  toolPanelWidget = Just . toWidget . myPassButton

  toolGobanInvalidate = invalidateIsValidMoveCache

  toolGobanClickComplete me from to = do
    let ui = myUi me
    when (isJust from && from == to) $ do
      valid <- isValidMove ui $ fromJust from
      when valid $ playAt ui from

  toolGobanRenderModifyCoords me board coords = do
    state <- toolGetGobanState me
    let coordFromMouseState = case state of
          ToolGobanHovering maybeCoord -> maybeCoord
          ToolGobanDragging _ from current | from == current -> from
          _ -> Nothing
    coordIfValidMove <- case coordFromMouseState of
      Nothing -> return Nothing
      Just coord -> if' coordFromMouseState Nothing <$> getIsValidMove me coord
    return $ case coordIfValidMove of
      Just (x, y) ->
        listUpdate
        (flip listUpdate x $ \rendered ->
           let coord = renderedCoordState rendered
               coord' = coord { Board.coordStone = Just $ Board.boardPlayerTurn board }
           in rendered { renderedCoordState = coord' })
        y
        coords
      _ -> coords

create :: UiCtrl go ui => ui -> ToolState -> IO (PlayTool ui)
create ui toolState = do
  viewState <- viewStateNew
  passButton <- buttonNewWithLabel "Pass"
  isValidMoveCache <- newIORef Nothing

  let me = PlayTool
        { myUi = ui
        , myViewState = viewState
        , myToolState = toolState
        , myPassButton = passButton
        , myIsValidMoveCache = isValidMoveCache
        }

  -- TODO Reuse Actions.myGamePassAction?
  on passButton buttonActivated $ playAt ui Nothing

  return me

-- | Calculates whether it is valid for the current player to to play at a
-- 'Coord'.  Caches the result.
getIsValidMove :: UiCtrl go ui => PlayTool ui -> Coord -> IO Bool
getIsValidMove me coord = do
  let ui = myUi me
      cache = myIsValidMoveCache me
  cached <- readIORef cache
  case cached of
    Just (cachedCoord, cachedValue) | cachedCoord == coord -> return cachedValue
    _ -> do isValid <- isValidMove ui coord
            writeIORef cache $ Just (coord, isValid)
            return isValid

-- | Invalidates the cache used by 'getIsValidMove'.
invalidateIsValidMoveCache :: PlayTool ui -> IO ()
invalidateIsValidMoveCache me = writeIORef (myIsValidMoveCache me) Nothing
