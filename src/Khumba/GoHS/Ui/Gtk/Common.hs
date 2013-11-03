module Khumba.GoHS.Ui.Gtk.Common where

import Control.Monad ((<=<), forM_)
import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk hiding (Color, Cursor)
import Khumba.GoHS.Sgf
import Khumba.GoHS.Sgf.Parser

-- | A controller for the GTK+ UI.
class UiCtrl a where
  -- | Reads the current UI modes.
  readModes :: a -> IO UiModes

  -- | Modifies the controller's modes according to the given action, then fires
  -- a mode change event via 'fireViewModesChanged'.
  modifyModes :: a -> (UiModes -> IO UiModes) -> IO ()

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

  openBoard :: a -> Node -> IO a

  openNewBoard :: a -> Int -> Int -> IO a
  openNewBoard ui width height = openBoard ui (rootNode width height)

  openFile :: a -> String -> IO (Either ParseError a)
  openFile ui file = do
    -- TODO Don't only choose the first tree in the collection.
    result <- parseFile file
    case result of
      Right trees -> fmap Right $ openBoard ui $ head trees
      Left err -> return $ Left err

modifyModesPure :: UiCtrl ui => ui -> (UiModes -> UiModes) -> IO ()
modifyModesPure ui f = modifyModes ui (return . f)

-- | Assigns to the current tool within the modes of 'ui' (firing any relevant
-- change handlers).
setTool :: UiCtrl ui => ui -> Tool -> IO ()
setTool ui tool = modifyModesPure ui $ \modes -> modes { uiTool = tool }

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

-- | Selectable tools for operating on the board.
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
          deriving (Bounded, Enum, Eq, Show)

-- | The tool that should be selected when a board first opens in the UI.
initialTool :: Tool
initialTool = ToolPlay

-- | The ordering and grouping of tools as they should appear in the UI.
toolOrdering :: [[Tool]]
toolOrdering =
  [[ToolPlay, ToolJump, ToolScore],
   [ToolBlack, ToolWhite, ToolErase],
   [ToolArrow, ToolMarkCircle, ToolLabel, ToolLine, ToolMarkX, ToolMarkSelected,
    ToolMarkSquare, ToolMarkTriangle],
   [ToolVisible, ToolDim, ToolInvisible]]

toolLabel :: Tool -> String
toolLabel tool = case tool of
  ToolPlay -> "Play"
  ToolJump -> "Jump to move"
  ToolScore -> "Score"
  ToolBlack -> "Paint black stones"
  ToolWhite -> "Paint white stones"
  ToolErase -> "Erase stones"
  ToolArrow -> "Draw arrows"
  ToolMarkCircle -> "Mark circles"
  ToolLabel -> "Label points"
  ToolLine -> "Draw lines"
  ToolMarkX -> "Mark Xs"
  ToolMarkSelected -> "Mark selected"
  ToolMarkSquare -> "Mark squares"
  ToolMarkTriangle -> "Mark triangles"
  ToolVisible -> "Set points visible"
  ToolDim -> "Set points dimmed"
  ToolInvisible -> "Set points invisible"

-- | Converts 'ToolBlack' and 'ToolWhite' into 'Color's.  Does not accept any
-- other tools.
toolToColor :: Tool -> Color
toolToColor ToolBlack = Black
toolToColor ToolWhite = White
toolToColor other = error $ "toolToColor is invalid for " ++ show other ++ "."

-- | Creates a list of 'FileFilter's that should be used in 'FileChooser's that
-- are working with SGF files.
fileFiltersForSgf :: IO [FileFilter]
fileFiltersForSgf = do
  sgf <- fileFilterNew
  fileFilterSetName sgf "SGF files (*.sgf)"
  fileFilterAddPattern sgf "*.sgf"
  all <- fileFilterNew
  fileFilterSetName all "All files (*)"
  fileFilterAddPattern all "*"
  return [sgf, all]
