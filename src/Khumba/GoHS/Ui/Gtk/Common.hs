module Khumba.GoHS.Ui.Gtk.Common where

import Control.Monad ((<=<), forM_)
import Data.IORef
import Data.Maybe
import Khumba.GoHS.Sgf

-- | A controller for the GTK+ UI.
class UiCtrl a where
  -- | Reads the current UI modes.
  readModes :: a -> IO UiModes

  -- | Modifies the current UI modes with an IO action.
  internalSetModes :: a -> UiModes -> IO ()

  -- | Returns the current cursor.
  readCursor :: a -> IO Cursor

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

modifyModes :: UiCtrl ui => ui -> (UiModes -> IO UiModes) -> IO ()
modifyModes ui f = readModes ui >>= f >>= internalSetModes ui

modifyModesPure :: UiCtrl ui => ui -> (UiModes -> UiModes) -> IO ()
modifyModesPure ui f = modifyModes ui (return . f)

-- | An IO variable that points to a 'UiCtrl'.
data UiRef ui = UiRef { getUiRef :: IORef (Maybe ui) }

readUiRef :: UiCtrl ui => UiRef ui -> IO ui
readUiRef = maybe (fail "readUiRef failed.") return <=< readIORef . getUiRef

data View = forall a. UiView a => View a

-- | A class for implementations of widgets that render boards.
class UiView a where
  viewModesChanged :: a -> UiModes -> IO ()
  viewModesChanged _ _ = return ()

  viewCursorChanged :: a -> Cursor -> IO ()
  viewCursorChanged _ _ = return ()

  viewChildren :: a -> [View]
  viewChildren _ = []

fireViewModesChanged :: UiView a => a -> UiModes -> IO ()
fireViewModesChanged view modes = do
  viewModesChanged view modes
  forM_ (viewChildren view) $ \(View child) ->
    fireViewModesChanged child modes

fireViewCursorChanged :: UiView a => a -> Cursor -> IO ()
fireViewCursorChanged view cursor = do
  viewCursorChanged view cursor
  forM_ (viewChildren view) $ \(View child) ->
    fireViewCursorChanged child cursor

data UiModes = UiModes { uiViewMode :: ViewMode
                       , uiViewOneColorModeColor :: Color
                       , uiViewBlindModesAnnouncePlayer :: Bool
                         -- ^ If true, announce the player whose turn it is with
                         -- blindfolds off.  If false, announce the player whose
                         -- turn it is with blindfolds on.
                       , uiOverrideVariationMode :: Maybe VariationMode
                       , uiShowMoveNumberMode :: Bool
                       , uiTool :: Tool
                       } deriving (Eq, Show)

data ViewMode = ViewRegularMode
              | ViewOneColorMode
              | ViewNothingMode
              deriving (Eq, Show)

defaultUiModes :: UiModes
defaultUiModes = UiModes { uiViewMode = ViewRegularMode
                         , uiViewOneColorModeColor = Black
                         , uiViewBlindModesAnnouncePlayer = True
                         , uiOverrideVariationMode = Nothing
                         , uiShowMoveNumberMode = False
                         , uiTool = ToolPlay
                         }

data Tool = -- * Game tools
            ToolPlay
          | ToolJump
          | ToolScore
            -- * Editing tools
          | ToolBlack
          | ToolWhite
          | ToolErase
            -- * Markup tools
          | ToolArrow
          | ToolMarkCircle
          | ToolLabel
          | ToolLine
          | ToolMarkX
          | ToolMarkSelected
          | ToolMarkSquare
          | ToolMarkTriangle
            -- * Visibility tools
          | ToolVisible
          | ToolDim
          | ToolInvisible
          deriving (Eq, Show)
