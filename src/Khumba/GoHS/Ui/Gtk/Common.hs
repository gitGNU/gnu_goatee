module Khumba.GoHS.Ui.Gtk.Common where

import Control.Monad ((<=<))
import Data.IORef
import Data.Maybe
import Khumba.GoHS.Sgf

class UiState a where
  -- | Determines whether it is currently valid to play at the given point.
  isValidMove :: a -> Coord -> IO Bool

  -- | Makes the current player place a stone at the given point.
  playAt :: a -> Coord -> IO ()

  -- | If possible, takes a step up to the parent of the current node
  -- in the game tree.  Returns whether a move was made (i.e. whether
  -- the current node is not the root node).
  goUp :: a -> IO Bool

  -- | If possible, takes a step down from the current node to the
  -- last visited child, or the first if no children of this node have
  -- been visited.  Returns whether a move was made (i.e. whether
  -- there were any children to go to).
  goDown :: a -> IO Bool

  -- | If possible, move to the sibling node immediately to the left
  -- of the current one.  Returns whether a move was made
  -- (i.e. whether there was a left sibling).
  goLeft :: a -> IO Bool

  -- | If possible, move to the sibling node immediately to the right
  -- of the current one.  Returns whether a move was made
  -- (i.e. whether there was a right sibling).
  goRight :: a -> IO Bool

data UiRef ui = UiRef { getUiRef :: IORef (Maybe ui) }

readUiRef :: UiState ui => UiRef ui -> IO ui
readUiRef = maybe (fail "readUiRef failed.") return <=< readIORef . getUiRef
