-- | Common dependencies among all GTK+ UI code.  Contains class definitions and
-- some common data declarations.
module Khumba.GoHS.Ui.Gtk.Common where

import Control.Monad ((<=<))
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.IORef
import Data.Unique (Unique)
import Graphics.UI.Gtk hiding (Color, Cursor)
import Khumba.GoHS.Common (Seq(..))
import Khumba.GoHS.Sgf
import Khumba.GoHS.Sgf.Monad (GoT, runGoT, Event)
import Khumba.GoHS.Sgf.Parser

-- | A Go monad with handlers in the 'IO' monad.
type UiGoM = GoT (Writer (Seq IO))

-- | Schedules an IO action to run after the currently-executing Go monad
-- completes.  The IO action should not attempt to access the cursor, as it may
-- not be available; instead it should work within the Go monad for cursor
-- manipulation (e.g. 'Khumba.GoHS.Sgf.Monad.getCursor').
afterGo :: IO () -> UiGoM ()
afterGo = tell . Seq

runUiGo :: UiGoM a -> Cursor -> (a, Cursor, IO ())
runUiGo go cursor =
  let ((value, cursor'), Seq action) = runWriter $ runGoT go cursor
  in (value, cursor', action)

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

  -- | If possible, takes a step up to the parent of the current node in the
  -- game tree.  Returns whether a move was made (i.e. whether the current node
  -- is not the root node).
  goUp :: a -> IO Bool

  -- | If possible, takes a step down from the current node to its child at the
  -- given index.  Returns whether a move was made (i.e. whether the node had
  -- @n+1@ children).
  goDown :: a -> Int -> IO Bool

  -- | If possible, move to the sibling node immediately to the left of the
  -- current one.  Returns whether a move was made (i.e. whether there was a
  -- left sibling).
  goLeft :: a -> IO Bool

  -- | If possible, move to the sibling node immediately to the right of the
  -- current one.  Returns whether a move was made (i.e. whether there was a
  -- right sibling).
  goRight :: a -> IO Bool

  -- | Registers a handler for a given 'Event'.  Returns a 'Registration' that
  -- can be given to 'unregister' to prevent any further calls to the handler.
  register :: a -> Event UiGoM handler -> handler -> IO Registration

  -- | Unregisters the handler for a 'Registration' that was returned from
  -- 'register'.  Returns true if such a handler was found and removed.
  unregister :: a -> Registration -> IO Bool

  -- | Registers a handler that will execute when UI modes change.
  registerModesChangedHandler :: a -> ModesChangedHandler -> IO Registration

  -- | Unregisters the modes-changed handler for a 'Registration' that was
  -- returned from 'registerModesChangedHandler'.  Returns true if such a
  -- handler was found and removed.
  unregisterModesChangedHandler :: a -> Registration -> IO Bool

  openBoard :: a -> Node -> IO a

  openNewBoard :: a -> Maybe (Int, Int) -> IO a
  openNewBoard ui maybeSize =
    openBoard ui $ maybe emptyNode (uncurry rootNodeWithSize) maybeSize

  openFile :: a -> String -> IO (Either String a)
  openFile ui file = do
    result <- parseFile file
    case result of
      -- TODO Don't only choose the first tree in the collection.
      Right collection -> fmap Right $ openBoard ui $ head $ collectionTrees collection
      Left err -> return $ Left err

-- | A key that refers to registration of a handler with a UI controller.  Used
-- to unregister handlers.
type Registration = Unique

-- | A handler for taking action when UI modes change.  Passed the old modes and
-- the new modes, in that order.
type ModesChangedHandler = UiModes -> UiModes -> IO ()

modifyModesPure :: UiCtrl ui => ui -> (UiModes -> UiModes) -> IO ()
modifyModesPure ui f = modifyModes ui (return . f)

-- | Assigns to the current tool within the modes of 'ui' (firing any relevant
-- change handlers).
setTool :: UiCtrl ui => ui -> Tool -> IO ()
setTool ui tool = modifyModesPure ui $ \modes -> modes { uiTool = tool }

-- | An IO variable that points to a 'UiCtrl'.
newtype UiRef ui = UiRef { getUiRef :: IORef (Maybe ui) }

readUiRef :: UiCtrl ui => UiRef ui -> IO ui
readUiRef = maybe (fail message) return <=< readIORef . getUiRef
  where message = "readUiRef failed; can't call me during initial UI setup."

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
data Tool = ToolPlay  -- ^ Game tool.
          | ToolJump  -- ^ Game tool.
          | ToolScore  -- ^ Game tool.
          | ToolBlack  -- ^ Editing tool.
          | ToolWhite  -- ^ Editing tool.
          | ToolErase  -- ^ Editing tool.
          | ToolArrow  -- ^ Markup tool.
          | ToolMarkCircle  -- ^ Markup tool.
          | ToolLabel  -- ^ Markup tool.
          | ToolLine  -- ^ Markup tool.
          | ToolMarkX  -- ^ Markup tool.
          | ToolMarkSelected  -- ^ Markup tool.
          | ToolMarkSquare  -- ^ Markup tool.
          | ToolMarkTriangle  -- ^ Markup tool.
          | ToolVisible  -- ^ Visibility tool.
          | ToolDim  -- ^ Visibility tool.
          | ToolInvisible  -- ^ Visibility tool.
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
