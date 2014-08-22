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

-- | Common dependencies among all GTK+ UI code.  Contains class definitions and
-- some common data declarations.
module Game.Goatee.Ui.Gtk.Common (
  -- * UI Go monads
  MonadUiGo (..), uiGoUpdateView, uiGoMakeDirty,
  UiGoState (..), initialUiGoState,
  -- * UI controllers
  UiCtrl (..), AnyUiCtrl (..),
  Registration,
  DirtyChangedHandler,
  FilePathChangedHandler,
  ModesChangedHandler,
  modifyModesPure,
  setTool,
  -- * UI views
  UiView (..),
  AnyView (..),
  ViewId,
  ViewState,
  viewStateNew,
  viewDestroy,
  viewId,
  -- * UI modes
  UiModes (..),
  ViewMode (..),
  defaultUiModes,
  Tool (..),
  initialTool,
  toolOrdering,
  toolLabel,
  toolToColor,
  fileFiltersForSgf,
  ) where

import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Unique (Unique, newUnique)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad
import Game.Goatee.Lib.Parser
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import Graphics.UI.Gtk (FileFilter, Window, fileFilterAddPattern, fileFilterNew, fileFilterSetName)
import System.FilePath (takeFileName)

-- | A class for monads that provide the features required to be used with a
-- 'UiCtrl'.  The type must have a 'MonadGo' instance and also provide access to
-- some internal state, 'UiGoState'.
class MonadGo go => MonadUiGo go where
  -- | Evaluates the Go monad, returning the final value and cursor as well as
  -- the internal 'UiGoState'.
  runUiGo :: Cursor -> go a -> (a, Cursor, UiGoState)

  -- | Retrieves the current internal state.
  uiGoGetState :: go UiGoState

  -- | Assigns to the current internal state.
  uiGoPutState :: UiGoState -> go ()

  -- | Modifies the current internal state with the given function.
  uiGoModifyState :: (UiGoState -> UiGoState) -> go ()

-- | Forces the view with the given ID to update after the Go action completes.
uiGoUpdateView :: MonadUiGo go => ViewId -> go ()
uiGoUpdateView viewId = uiGoModifyState $ \state ->
  let views = uiGoViewsToUpdate state
  in if Set.member viewId views
     then state
     else state { uiGoViewsToUpdate = Set.insert viewId views }

-- | Forces the UI to become dirty after the Go action completes.  See
-- 'setDirty'.
uiGoMakeDirty :: MonadUiGo go => go ()
uiGoMakeDirty = uiGoModifyState $ \state -> state { uiGoMakesDirty = True }

-- | Internal state held by a type that implements 'MonadUiGo'.
data UiGoState = UiGoState
  { uiGoViewsToUpdate :: Set ViewId
  -- ^ Keeps track of views that need updating after the Go action completes.
  , uiGoMakesDirty :: Bool
    -- ^ Whether the UI should be marked dirty as a result of running the Go
    -- action.
  }

-- | The state with which a UI Go action should start executing.
initialUiGoState :: UiGoState
initialUiGoState = UiGoState
  { uiGoViewsToUpdate = Set.empty
  , uiGoMakesDirty = False
  }

-- | A controller for the GTK+ UI.
--
-- The controller is agnostic to the type of Go monad it is used with, as long
-- as it implements the functionality in 'MonadUiGo'.  The monad can have extra
-- functionality, e.g. for testing or debugging.
class MonadUiGo go => UiCtrl go ui | ui -> go where
  -- | Reads the current UI modes.
  readModes :: ui -> IO UiModes

  -- | Modifies the controller's modes according to the given action, then fires
  -- a mode change event via 'fireViewModesChanged'.
  modifyModes :: ui -> (UiModes -> IO UiModes) -> IO ()

  -- | Runs a Go monad on the current cursor, updating the cursor and firing
  -- handlers as necessary.
  doUiGo :: ui -> go a -> IO a

  -- | Returns the current cursor.
  readCursor :: ui -> IO Cursor

  -- | Determines whether it is currently valid to play at the given point.
  isValidMove :: ui -> Coord -> IO Bool

  -- | Makes the current player place a stone at the given point, or pass in the
  -- case of 'Nothing'.
  playAt :: ui -> Maybe Coord -> IO ()

  -- | If possible, takes a step up to the parent of the current node in the
  -- game tree.  Returns whether a move was made (i.e. whether the current node
  -- is not the root node).
  goUp :: ui -> IO Bool

  -- | If possible, takes a step down from the current node to its child at the
  -- given index.  Returns whether a move was made (i.e. whether the node had
  -- @n+1@ children).
  goDown :: ui -> Int -> IO Bool

  -- | If possible, move to the sibling node immediately to the left of the
  -- current one.  Returns whether a move was made (i.e. whether there was a
  -- left sibling).
  goLeft :: ui -> IO Bool

  -- | If possible, move to the sibling node immediately to the right of the
  -- current one.  Returns whether a move was made (i.e. whether there was a
  -- right sibling).
  goRight :: ui -> IO Bool

  -- | Registers a view to update when any of the given 'Event's fires.  The
  -- controller will call 'viewUpdate' after the Go action finishes running.
  --
  -- When the view is destroyed, it must call 'unregister' or 'unregisterAll' to
  -- free the handlers it has installed.  'viewDestroy' calls this, and all
  -- views should call 'viewDestroy', so there is no need to call this manually.
  register :: UiView go ui view => view -> [AnyEvent go] -> IO ()

  -- | Stops the controller from updating the view when the 'Event' fires.
  -- Returns true if there was a registration that was removed.
  unregister :: UiView go ui view => view -> AnyEvent go -> IO Bool

  -- | Stops the controller from updating the view entirely.
  unregisterAll :: UiView go ui view => view -> IO ()

  -- | Returns the currently registered handlers, as (owner, event name) pairs.
  registeredHandlers :: ui -> IO [(String, String)]

  -- | Registers a handler that will execute when UI modes change.  The string
  -- is the name of the caller, used to keep track of what components registered
  -- what handlers.
  registerModesChangedHandler :: ui -> String -> ModesChangedHandler -> IO Registration

  -- | Unregisters the modes-changed handler for a 'Registration' that was
  -- returned from 'registerModesChangedHandler'.  Returns true if such a
  -- handler was found and removed.
  unregisterModesChangedHandler :: ui -> Registration -> IO Bool

  -- | Returns the owners of the currently registered 'ModesChangedHandler's.
  registeredModesChangedHandlers :: ui -> IO [String]

  -- | Returns the 'Window' for the game's 'MainWindow'.
  getMainWindow :: ui -> IO Window

  openBoard :: Maybe ui -> Maybe FilePath -> Node -> IO ui

  openNewBoard :: Maybe ui -> Maybe (Int, Int) -> IO ui
  openNewBoard ui = openBoard ui Nothing . rootNode

  openFile :: Maybe ui -> FilePath -> IO (Either String ui)
  openFile ui file = do
    result <- parseFile file
    case result of
      -- TODO Don't only choose the first tree in the collection.
      Right collection ->
        fmap Right $ openBoard ui (Just file) $ head $ collectionTrees collection
      Left err -> return $ Left err

  -- | Prompts with an open file dialog for a game to open, then opens the
  -- selected game if the user picks one.
  fileOpen :: ui -> IO ()

  -- | Saves the current game to a file.  If the current game is not currently
  -- tied to a file, then this will act identically to 'fileSaveAs'.  Returns true
  -- iff the game was saved.
  fileSave :: ui -> IO Bool

  -- | Presents a file save dialog for the user to specify a file to write the
  -- current game to.  If the user provides a filename, then the game is
  -- written.  If the user names an existing file, then another dialog confirms
  -- overwriting the existing file.  Returns true iff the user accepted the
  -- dialog(s) and the game was saved.
  fileSaveAs :: ui -> IO Bool

  -- | Closes the game and all UI windows, etc. attached to the given controller.
  -- If the game is dirty, then a dialog first prompts the user whether to save,
  -- throw away changes, or abort the closing.
  fileClose :: ui -> IO Bool

  -- | Hides and releases the game's UI window, and shut downs the UI controller
  -- (in effect closing the game, with no prompting).  If this is the last board
  -- open, then the application will exit.
  fileCloseSilently :: ui -> IO ()

  -- | Closes all open games and exits the program.  If any games are dirty then
  -- we check if the user wants to save them.  If the user clicks cancel at any
  -- point then shutdown is cancelled and no games are closed.
  fileQuit :: ui -> IO Bool

  -- | Performs a copy a la 'editCopyNode'.  If this copy succeeds, then we
  -- navigate to the parent of the current node, and delete the node we were
  -- just on from the tree.
  editCutNode :: ui -> IO ()

  -- | Copies an SGF representation of the subtree rooted at the current node
  -- onto the system clipboard.  If the node fails to render, then an error
  -- dialog is displayed and the clipboard is left untouched.
  editCopyNode :: ui -> IO ()

  -- | Attempts to parse text on the system clipboard as an SGF subtree and
  -- insert the parsed node below the current node.
  editPasteNode :: ui -> IO ()

  -- | Presents the user with an about dialog that shows general information
  -- about the application to the user (authorship, license, etc.).
  helpAbout :: ui -> IO ()

  -- | Returns the path to the file that the UI is currently displaying, or
  -- nothing if the UI is displaying an unsaved game.
  getFilePath :: ui -> IO (Maybe FilePath)

  -- | Returns the filename (base name, with no directories before it) that is
  -- currently open in the UI, or if the UI is showing an unsaved game, then a
  -- fallback "untitled" string.  As this may not be a real filename, it should
  -- be used for display purposes only, and not for actually writing to.
  getFileName :: ui -> IO String
  getFileName ui = maybe untitledFileName takeFileName <$> getFilePath ui

  -- | Sets the path to the file that the UI is currently displaying.  This
  -- changes the value returned by 'getFilePath' but does not do any saving or
  -- loading.
  setFilePath :: ui -> Maybe FilePath -> IO ()

  -- | Registers a handler that will execute when the file path the UI is bound
  -- to changes.
  registerFilePathChangedHandler :: ui
                                 -> String
                                    -- ^ The name of the caller, used to track
                                    -- what components registered what handlers.
                                 -> Bool
                                    -- ^ If true, the handler will be called
                                    -- immediately after registration.
                                 -> FilePathChangedHandler
                                 -> IO Registration

  -- | Unregisters the handler for a 'Registration' that was returned from
  -- 'registerFilePathChangedHandler'.  Returns true if such a handler was found
  -- and removed.
  unregisterFilePathChangedHandler :: ui -> Registration -> IO Bool

  -- | Returns the owners of the currently registered 'FilePathChangedHandler's.
  registeredFilePathChangedHandlers :: ui -> IO [String]

  -- | Returns whether the UI is dirty, i.e. whether there are unsaved changes
  -- to the current game.
  getDirty :: ui -> IO Bool

  -- | Sets the dirty state of the UI.
  setDirty :: ui -> Bool -> IO ()

  -- | Registers a handler that will execute when the dirty state of the UI
  -- changes.
  registerDirtyChangedHandler :: ui
                              -> String
                                 -- ^ The name of the caller, used to track what
                                 -- components registered what handlers.
                              -> Bool
                                 -- ^ If true, the handler will be called
                                 -- immediately after registration.
                              -> DirtyChangedHandler
                              -> IO Registration

  -- | Unregisters the handler for a 'Registration' that was returned from
  -- 'registerDirtyChangedHandler'.  Returns true if such a handler was found
  -- and removed.
  unregisterDirtyChangedHandler :: ui -> Registration -> IO Bool

  -- | Returns the owners of the currently registered 'DirtyChangedHandler's.
  registeredDirtyChangedHandlers :: ui -> IO [String]

-- | An existential type for all UI controllers.
data AnyUiCtrl = forall go ui. UiCtrl go ui => AnyUiCtrl ui

-- | A key that refers to registration of a handler with a UI controller, for
-- non-Go-monad events.  Used to unregister handlers.
type Registration = Unique

-- | A handler for when the dirty state of the UI changes.  Passed the new dirty
-- state.  The old dirty state is the logical negation of the new state.
type DirtyChangedHandler = Bool -> IO ()

-- | A handler for taking action when the file path for the current game
-- changes.  Passed the old path and the new path, in that order.
type FilePathChangedHandler = Maybe FilePath -> Maybe FilePath -> IO ()

-- | A handler for taking action when UI modes change.  Passed the old modes and
-- the new modes, in that order.
type ModesChangedHandler = UiModes -> UiModes -> IO ()

modifyModesPure :: UiCtrl go ui => ui -> (UiModes -> UiModes) -> IO ()
modifyModesPure ui f = modifyModes ui (return . f)

-- | Assigns to the current tool within the modes of 'ui' (firing any relevant
-- change handlers).
setTool :: UiCtrl go ui => ui -> Tool -> IO ()
setTool ui tool = modifyModesPure ui $ \modes -> modes { uiTool = tool }

-- | A UI widget that listens to the state of a 'UiCtrl'.  This class makes it
-- easy to ask to be updated on relevant changes with 'register'.
class UiCtrl go ui => UiView go ui view | view -> ui where
  -- | A printable name of the view; usually just the data type name.
  viewName :: view -> String

  -- | A reference to the view's controller.
  viewCtrl :: view -> ui

  -- | Internal housekeeping data for the view.  Create with 'viewStateNew'.  A
  -- 'ViewState' may only be used with a single view.
  viewState :: view -> ViewState

  -- | Updates the UI state of the view based on the current state of data that
  -- backs it in the model.
  viewUpdate :: view -> IO ()

-- | An existential type over all views.  Provides 'Eq' and 'Ord' instances that
-- use each view's 'ViewId', and a 'Show' instance that returns a view's name.
data AnyView = forall go ui view. UiView go ui view => AnyView view

instance Eq AnyView where
  (AnyView v) == (AnyView v') = viewId v == viewId v'

instance Ord AnyView where
  compare (AnyView v) (AnyView v') = compare (viewId v) (viewId v')

instance Show AnyView where
  show (AnyView v) = viewName v

-- | A application-wide unique identifier for referring to an instance of a
-- view.
newtype ViewId = ViewId Unique deriving (Eq, Ord)

-- | Internal controller housekeeping data for a view.
data ViewState = ViewState
  { viewId' :: ViewId
  }

-- | Creates a new 'ViewState'.
viewStateNew :: IO ViewState
viewStateNew = ViewState <$> (ViewId <$> newUnique)

-- | Cleans up the internal state of a view, and releases registered Go event
-- handlers for the view.  Views should call this when destroying themselves;
-- this does __not force__ a view to destroy itself.
viewDestroy :: UiView go ui view => view -> IO ()
viewDestroy = unregisterAll

-- | Returns a view's unique ID.
viewId :: UiView go ui view => view -> ViewId
viewId = viewId' . viewState

data UiModes = UiModes
  { uiViewMode :: ViewMode
  , uiViewOneColorModeColor :: Color
  , uiViewBlindModesAnnouncePlayer :: Bool
    -- ^ If true, announce the player whose turn it is with
    -- blindfolds off.  If false, announce the player whose
    -- turn it is with blindfolds on.
  , uiHighlightCurrentMovesMode :: Bool
    -- ^ Whether to draw an indicator on the game board for
    -- moves on the current node.
  , uiTool :: Tool
  } deriving (Eq, Show)

data ViewMode = ViewRegularMode
              | ViewOneColorMode
              | ViewNothingMode
              deriving (Eq, Show)

defaultUiModes :: UiModes
defaultUiModes = UiModes
  { uiViewMode = ViewRegularMode
  , uiViewOneColorModeColor = Black
  , uiViewBlindModesAnnouncePlayer = True
  , uiHighlightCurrentMovesMode = True
  , uiTool = ToolPlay
  }

-- | Selectable tools for operating on the board.
data Tool =
  ToolPlay  -- ^ Game tool.
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
   [ToolVisible, ToolDim]]

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
  ToolVisible -> "Toggle points visible"
  ToolDim -> "Toggle points dimmed"

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

-- | The name to display in the UI for a game that has not yet been saved to
-- disk.
untitledFileName :: String
untitledFileName = "(Untitled)"
