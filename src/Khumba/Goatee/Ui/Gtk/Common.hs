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
module Khumba.Goatee.Ui.Gtk.Common (
  AppState(appWindowCount),
  newAppState,
  -- * UI controllers
  UiGoM,
  afterGo,
  runUiGoPure,
  UiCtrl(..),
  Registration,
  DirtyChangedHandler,
  FilePathChangedHandler,
  ModesChangedHandler,
  modifyModesPure,
  setTool,
  -- * UI views
  UiView(..),
  ViewRegistrations,
  viewNewRegistrations,
  viewRegister,
  viewUnregisterAll,
  -- * UI modes
  UiModes(..),
  ViewMode(..),
  defaultUiModes,
  Tool(..),
  initialTool,
  toolOrdering,
  toolLabel,
  toolToColor,
  fileFiltersForSgf,
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Unique (Unique)
import Graphics.UI.Gtk (FileFilter, fileFilterAddPattern, fileFilterNew, fileFilterSetName)
import Khumba.Goatee.Common (Seq(..))
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Monad
import Khumba.Goatee.Sgf.Parser
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types

-- | A structure for holding global UI information.
data AppState = AppState { appWindowCount :: MVar Int
                           -- ^ The number of open windows.  When this reaches
                           -- zero, the UI will exit.
                         }

newAppState :: IO AppState
newAppState = do
  windowCount <- newMVar 0
  return AppState { appWindowCount = windowCount }

-- | A Go monad with handlers in the 'IO' monad.
type UiGoM = GoT (Writer (Seq IO))

-- | Schedules an IO action to run after the currently-executing Go monad
-- completes.  The IO action should not attempt to access the cursor, as it may
-- not be available; instead it should work within the Go monad for cursor
-- manipulation (e.g. 'Khumba.Goatee.Sgf.Monad.getCursor').
afterGo :: IO () -> UiGoM ()
afterGo = tell . Seq

-- | Applies a 'UiGoM' to a 'Cursor' purely, as opposed to 'runUiGo' which works
-- with the UI controller's cursor.  Returns a tuple containing the Go action's
-- result, the final cursor, and the IO action resulting from all handlers being
-- run.
runUiGoPure :: UiGoM a -> Cursor -> (a, Cursor, IO ())
runUiGoPure go cursor =
  let ((value, cursor'), Seq action) = runWriter $ runGoT go cursor
  in (value, cursor', action)

-- | A controller for the GTK+ UI.
class UiCtrl a where
  -- | Reads the current UI modes.
  readModes :: a -> IO UiModes

  -- | Modifies the controller's modes according to the given action, then fires
  -- a mode change event via 'fireViewModesChanged'.
  modifyModes :: a -> (UiModes -> IO UiModes) -> IO ()

  -- | Runs a Go monad on the current cursor, updating the cursor and firing
  -- handlers as necessary.
  runUiGo :: a -> UiGoM b -> IO b

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

  -- | Registers a handler for a given 'Event'.  The string is the name of the
  -- caller, used to keep track of what components registered what handlers.
  -- Returns a 'Registration' that can be given to 'unregister' to prevent any
  -- further calls to the handler.
  register :: a -> String -> Event UiGoM handler -> handler -> IO Registration

  -- | Unregisters the handler for a 'Registration' that was returned from
  -- 'register'.  Returns true if such a handler was found and removed.
  unregister :: a -> Registration -> IO Bool

  -- | Returns the currently registered handlers, as (owner, event name) pairs.
  registeredHandlers :: a -> IO [(String, String)]

  -- | Registers a handler that will execute when UI modes change.  The string
  -- is the name of the caller, used to keep track of what components registered
  -- what handlers.
  registerModesChangedHandler :: a -> String -> ModesChangedHandler -> IO Registration

  -- | Unregisters the modes-changed handler for a 'Registration' that was
  -- returned from 'registerModesChangedHandler'.  Returns true if such a
  -- handler was found and removed.
  unregisterModesChangedHandler :: a -> Registration -> IO Bool

  -- | Returns the owners of the currently registered 'ModesChangedHandler's.
  registeredModesChangedHandlers :: a -> IO [String]

  -- | Increments a counter for the number of open windows.  When this reaches
  -- zero, the UI will exit.
  windowCountInc :: a -> IO ()

  -- | Decrements a counter for the number of open windows.  When this reaches
  -- zero, the UI will exit.
  windowCountDec :: a -> IO ()

  openBoard :: Maybe a -> Maybe FilePath -> Node -> IO a

  openNewBoard :: Maybe a -> Maybe (Int, Int) -> IO a
  openNewBoard ui = openBoard ui Nothing . rootNode

  openFile :: Maybe a -> FilePath -> IO (Either String a)
  openFile ui file = do
    result <- parseFile file
    case result of
      -- TODO Don't only choose the first tree in the collection.
      Right collection ->
        fmap Right $ openBoard ui (Just file) $ head $ collectionTrees collection
      Left err -> return $ Left err

  -- | Returns the path to the file that the UI is currently displaying, or
  -- nothing if the UI is displaying an unsaved game.
  getFilePath :: a -> IO (Maybe FilePath)

  -- | Sets the path to the file that the UI is currently displaying.  This
  -- changes the value returned by 'getFilePath' but does not do any saving or
  -- loading.
  setFilePath :: a -> Maybe FilePath -> IO ()

  -- | Registers a handler that will execute when the file path the UI is bound
  -- to changes.
  registerFilePathChangedHandler :: a
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
  unregisterFilePathChangedHandler :: a -> Registration -> IO Bool

  -- | Returns the owners of the currently registered 'FilePathChangedHandler's.
  registeredFilePathChangedHandlers :: a -> IO [String]

  -- | Returns whether the UI is dirty, i.e. whether there are unsaved changes
  -- to the current game.
  getDirty :: a -> IO Bool

  -- | Sets the dirty state of the UI.
  setDirty :: a -> Bool -> IO ()

  -- | Registers a handler that will execute when the dirty state of the UI
  -- changes.
  registerDirtyChangedHandler :: a
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
  unregisterDirtyChangedHandler :: a -> Registration -> IO Bool

  -- | Returns the owners of the currently registered 'DirtyChangedHandler's.
  registeredDirtyChangedHandlers :: a -> IO [String]

-- | A key that refers to registration of a handler with a UI controller.  Used
-- to unregister handlers.
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

modifyModesPure :: UiCtrl ui => ui -> (UiModes -> UiModes) -> IO ()
modifyModesPure ui f = modifyModes ui (return . f)

-- | Assigns to the current tool within the modes of 'ui' (firing any relevant
-- change handlers).
setTool :: UiCtrl ui => ui -> Tool -> IO ()
setTool ui tool = modifyModesPure ui $ \modes -> modes { uiTool = tool }

-- | A UI widget that listens to the state of a 'UiCtrl'.  This class makes it
-- easy to register and unregister event handlers with 'viewRegister' and
-- 'viewUnregisterAll'.
class UiCtrl ui => UiView view ui | view -> ui where
  -- | A printable name of the view; usually just the data type name.
  viewName :: view -> String

  -- | A reference to the view's controller.
  viewCtrl :: view -> ui

  -- | An updatable list of registrations for event handlers the view has
  -- registered.
  viewRegistrations :: view -> ViewRegistrations

type ViewRegistrations = IORef [Registration]

-- | Creates a new 'ViewRegistrations'.
viewNewRegistrations :: IO ViewRegistrations
viewNewRegistrations = newIORef []

-- | Registers a handler for an event on a view.
viewRegister :: UiView view ui => view -> Event UiGoM handler -> handler -> IO ()
viewRegister view event handler = do
  let ui = viewCtrl view
      name = viewName view
  registration <- register ui name event handler
  modifyIORef (viewRegistrations view) (registration:)

-- | Unregisters all event handlers that the view has registered in its
-- 'viewRegistrations'.
viewUnregisterAll :: UiView view ui => view -> IO ()
viewUnregisterAll view = do
  let ui = viewCtrl view
      registrations = viewRegistrations view
  readIORef registrations >>= mapM_ (unregister ui)
  writeIORef registrations []

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
