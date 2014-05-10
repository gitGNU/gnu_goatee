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

-- | A monad for working with game trees.
module Khumba.Goatee.Sgf.Monad (
  -- * The Go monad
  GoT, GoM,
  runGoT, runGo,
  evalGoT, evalGo,
  execGoT, execGo,
  getCursor, getCoordState,
  -- * Navigation
  Step(..), goUp, goDown, goToRoot, goToGameInfoNode,
  -- * Remembering positions
  pushPosition, popPosition, dropPosition,
  -- * Properties
  getProperties,
  modifyProperties,
  getProperty,
  getPropertyValue,
  putProperty,
  deleteProperty,
  modifyProperty,
  modifyPropertyValue,
  modifyPropertyString,
  modifyPropertyCoords,
  modifyGameInfo,
  modifyVariationMode,
  getMark,
  modifyMark,
  -- * Children
  addChild,
  -- * Event handling
  Event, on, fire,
  -- * Events
  childAddedEvent, ChildAddedHandler,
  gameInfoChangedEvent, GameInfoChangedHandler,
  navigationEvent, NavigationHandler,
  propertiesChangedEvent, PropertiesChangedHandler,
  variationModeChangedEvent, VariationModeChangedHandler,
  ) where

import Control.Applicative ((<$>), Applicative ((<*>), pure))
import Control.Monad (ap, liftM, when)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Writer.Class (MonadWriter, listen, pass, tell, writer)
import qualified Control.Monad.State as State
import Control.Monad.State (StateT)
import Data.List (delete, find, mapAccumL, nub)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Tree hiding (addChild)
import Khumba.Goatee.Sgf.Types

-- | The internal state of a Go monad transformer.  @go@ is the type of
-- Go monad or transformer (instance of 'GoMonad').
data GoState go = GoState { stateCursor :: Cursor
                            -- ^ The current position in the game tree.
                          , statePathStack :: PathStack
                            -- ^ The current path stack.

                          -- Event handlers.
                          , stateChildAddedHandlers :: [ChildAddedHandler go]
                            -- ^ Handlers for 'childAddedEvent'.
                          , stateGameInfoChangedHandlers :: [GameInfoChangedHandler go]
                            -- ^ Handlers for 'gameInfoChangedEvent'.
                          , stateNavigationHandlers :: [NavigationHandler go]
                            -- ^ Handlers for 'navigationEvent'.
                          , statePropertiesChangedHandlers :: [PropertiesChangedHandler go]
                            -- ^ Handlers for 'propertiesChangedEvent'.
                          , stateVariationModeChangedHandlers :: [VariationModeChangedHandler go]
                            -- ^ Handlers for 'variationModeChangedEvent'.
                          }

-- | A path stack is a record of previous places visited in a game tree.  It is
-- encoded a list of paths (steps) to each previous memorized position.
--
-- The positions saved in calls to 'pushPosition' correspond to entries in the
-- outer list here, with the first sublist representing the last call.  The
-- sublist contains the steps in order that will trace the path back to the
-- saved position.
type PathStack = [[Step]]

-- | A simplified constructor function for 'GoState'.
initialState :: Cursor -> GoState m
initialState cursor = GoState { stateCursor = cursor
                              , statePathStack = []
                              , stateChildAddedHandlers = []
                              , stateGameInfoChangedHandlers = []
                              , stateNavigationHandlers = []
                              , statePropertiesChangedHandlers = []
                              , stateVariationModeChangedHandlers = []
                              }

-- | A single step along a game tree.  Either up or down.
data Step = GoUp Int
            -- ^ Represents a step up from a child with the given index.
          | GoDown Int
            -- ^ Represents a step down to the child with the given index.
          deriving (Eq, Show)

-- | Reverses a step, such that taking a step then it's reverse will leave you
-- where you started.
reverseStep :: Step -> Step
reverseStep step = case step of
  GoUp index -> GoDown index
  GoDown index -> GoUp index

-- | Takes a 'Step' from a 'Cursor', returning a new 'Cursor'.
takeStep :: Step -> Cursor -> Cursor
takeStep (GoUp _) cursor = fromMaybe (error $ "takeStep: Can't go up from " ++ show cursor ++ ".") $
                           cursorParent cursor
takeStep (GoDown index) cursor = cursorChild cursor index

-- | Internal function.  Takes a 'Step' in the Go monad.  Updates the path stack
-- accordingly.
takeStepM :: Monad m => Step -> (PathStack -> PathStack) -> GoT m ()
takeStepM step = case step of
  GoUp _ -> goUp'
  GoDown index -> goDown' index

-- | A monad (transformer) for navigating and mutating 'Cursor's, and
-- remembering previous locations.  See 'GoT' and 'GoM'.
--
-- The monad supports handlers for events raised during actions it takes, such
-- as navigating through the tree and modifying nodes.
class Monad go => MonadGo go where
  -- | Returns the current cursor.
  getCursor :: go Cursor

  -- | Returns the 'CoordState' at the given point.
  getCoordState :: Coord -> go CoordState
  getCoordState coord = liftM (boardCoordState coord . cursorBoard) getCursor

  -- | Navigates up the tree.  It must be valid to do so, otherwise 'fail' is
  -- called.  Fires a 'navigationEvent' after moving.
  goUp :: go ()

  -- | Navigates down the tree to the child with the given index.  The child
  -- must exist.  Fires a 'navigationEvent' after moving.
  goDown :: Int -> go ()

  -- | Navigates up to the root of the tree.  Fires 'navigationEvent's for each
  -- step.
  goToRoot :: go ()

  -- | Navigates up the tree to the node containing game info properties, if
  -- any.  Returns true if a game info node was found.
  goToGameInfoNode :: Bool
                      -- ^ When no node with game info is found, then if false,
                      -- return to the original node, otherwise finish at the
                      -- root node.
                   -> go Bool

  -- | Pushes the current location in the game tree onto an internal position
  -- stack, such that 'popPosition' is capable of navigating back to the same
  -- position, even if the game tree has been modified (though the old position
  -- must still exist in the tree to return to it).
  pushPosition :: go ()

  -- | Returns to the last position pushed onto the internal position stack via
  -- 'pushPosition'.  This action must be balanced by a 'pushPosition'.
  popPosition :: go ()

  -- | Drops the last position pushed onto the internal stack by 'pushPosition'
  -- off of the stack.  This action must be balanced by a 'pushPosition'.
  dropPosition :: go ()

  -- | Returns the set of properties on the current node.
  getProperties :: go [Property]
  getProperties = liftM cursorProperties getCursor

  -- | Modifies the set of properties on the current node.
  --
  -- The given function must end on the same node on which it started.
  modifyProperties :: ([Property] -> go [Property]) -> go ()

  -- | Searches for a property on the current node, returning it if found.
  getProperty :: Descriptor d => d -> go (Maybe Property)

  -- | Searches for a valued property on the current node, returning its value
  -- if found.
  getPropertyValue :: ValuedDescriptor d v => d -> go (Maybe v)
  getPropertyValue descriptor = liftM (liftM $ propertyValue descriptor) $ getProperty descriptor

  -- | Sets a property on the current node, replacing an existing property with
  -- the same name, if one exists.
  putProperty :: Property -> go ()
  putProperty property = modifyProperty (propertyInfo property) $ const $ Just property

  -- | Deletes a property from the current node, if it's set.
  --
  -- Note that although a 'Property' is a 'Descriptor', giving a valued
  -- @Property@ here will not cause deletion to match on the value of the
  -- property.  That is, the following code will result in 'Nothing', because
  -- the deletion only cares about the name of the property.
  --
  -- > do putProperty $ PL Black
  -- >    deleteProperty $ PL White
  -- >    getPropertyValue propertyPL
  deleteProperty :: Descriptor d => d -> go ()
  deleteProperty descriptor = modifyProperty descriptor $ const Nothing

  -- | Calls the given function to modify the state of the given property
  -- (descriptor) on the current node.  'Nothing' represents the property not
  -- existing on the node, and a 'Just' marks the property's presence.  This
  -- function does not do any validation to check that the resulting tree state
  -- is valid.
  modifyProperty :: Descriptor d => d -> (Maybe Property -> Maybe Property) -> go ()

  -- | Calls the given function to modify the state of the given valued property
  -- (descriptor) on the current node.  'Nothing' represents the property not
  -- existing on the node, and a 'Just' with the property's value marks the
  -- property's presence.  This function does not do any validation to check
  -- that the resulting tree state is valid.
  modifyPropertyValue :: ValuedDescriptor d v => d -> (Maybe v -> Maybe v) -> go ()
  modifyPropertyValue descriptor fn = modifyProperty descriptor $ \old ->
    propertyBuilder descriptor <$> fn (propertyValue descriptor <$> old)

  -- | Mutates the string-valued property attached to the current node according
  -- to the given function.  The input string will be empty if the current node
  -- either has the property with an empty value, or doesn't have the property.
  -- Returning an empty string removes the property from the node, if it was
  -- set.
  modifyPropertyString :: (Stringlike s, ValuedDescriptor d s) => d -> (String -> String) -> go ()
  modifyPropertyString descriptor fn =
    modifyPropertyValue descriptor $ \value -> case fn (maybe "" sgfToString value) of
      "" -> Nothing
      str -> let sgf = stringToSgf str
                 -- Because stringToSgf might do processing, we have to check
                 -- the conversion back to a string for emptiness.
             in if null $ sgfToString sgf then Nothing else Just sgf

  -- | Mutates the 'CoordList'-valued property attached to the current node
  -- according to the given function.  Conversion between @CoordList@ and
  -- @[Coord]@ is performed automatically.  The input list will be empty if the
  -- current node either has the property with an empty value, or doesn't have
  -- the property.  Returning an empty list removes the property from the node,
  -- if it was set.
  --
  -- Importantly, this might not be specific enough for properties such as 'DD'
  -- and 'VW' where a present, empty list has different semantics from the
  -- property not being present.  In that case, 'modifyPropertyValue' is better.
  modifyPropertyCoords :: ValuedDescriptor d CoordList => d -> ([Coord] -> [Coord]) -> go ()
  modifyPropertyCoords descriptor fn =
    modifyPropertyValue descriptor $ \value -> case fn $ maybe [] expandCoordList value of
      [] -> Nothing
      coords -> Just $ buildCoordList coords

  -- | Mutates the game info for the current path, returning the new info.  If
  -- the current node or one of its ancestors has game info properties, then
  -- that node is modified.  Otherwise, properties are inserted on the root
  -- node.
  modifyGameInfo :: (GameInfo -> GameInfo) -> go GameInfo

  -- | Sets the game's 'VariationMode' via the 'ST' property on the root node,
  -- then fires a 'variationModeChangedEvent' if the variation mode has changed.
  modifyVariationMode :: (VariationMode -> VariationMode) -> go ()

  -- | Returns the 'Mark' at a point on the current node.
  getMark :: Coord -> go (Maybe Mark)
  getMark = liftM coordMark . getCoordState

  -- | Calls the given function to modify the presence of a 'Mark' on the
  -- current node.
  modifyMark :: (Maybe Mark -> Maybe Mark) -> Coord -> go ()
  modifyMark fn coord = do
    maybeOldMark <- getMark coord
    case (maybeOldMark, fn maybeOldMark) of
      (Just oldMark, Nothing) -> remove oldMark
      (Nothing, Just newMark) -> add newMark
      (Just oldMark, Just newMark) | oldMark /= newMark -> remove oldMark >> add newMark
      (Just _, Just _) -> return ()
      (Nothing, Nothing) -> return ()
    where remove mark = modifyPropertyCoords (markProperty mark) (delete coord)
          add mark = modifyPropertyCoords (markProperty mark) (coord:)

  -- | Adds a child node to the current node at the given index, shifting all
  -- existing children at and after the index to the right.  The index must in
  -- the range @[0, numberOfChildren]@.  Fires a 'childAddedEvent' after the
  -- child is added.
  addChild :: Int -> Node -> go ()

  -- | Registers a new event handler for a given event type.
  on :: Event go h -> h -> go ()

-- | The regular monad transformer for 'MonadGo'.
newtype GoT m a = GoT { goState :: StateT (GoState (GoT m)) m a }

-- | The regular monad for 'MonadGo'.
type GoM = GoT Identity

instance Monad m => Functor (GoT m) where
  fmap = liftM

instance Monad m => Applicative (GoT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (GoT m) where
  return x = GoT $ return x
  m >>= f = GoT $ goState . f =<< goState m
  fail = lift . fail

instance MonadTrans GoT where
  lift = GoT . lift

instance MonadIO m => MonadIO (GoT m) where
  liftIO = lift . liftIO

instance MonadWriter w m => MonadWriter w (GoT m) where
  writer = lift . writer
  tell = lift . tell
  listen = GoT . listen . goState
  pass = GoT . pass . goState

-- | Executes a Go monad transformer on a cursor, returning in the underlying
-- monad a tuple that contains the resulting value and the final cursor.
runGoT :: Monad m => GoT m a -> Cursor -> m (a, Cursor)
runGoT go cursor = do
  (value, state) <- State.runStateT (goState go) (initialState cursor)
  return (value, stateCursor state)

-- | Executes a Go monad transformer on a cursor, returning in the underlying
-- monad the value in the transformer.
evalGoT :: Monad m => GoT m a -> Cursor -> m a
evalGoT go cursor = liftM fst $ runGoT go cursor

-- | Executes a Go monad transformer on a cursor, returning in the underlying
-- monad the final cursor.
execGoT :: Monad m => GoT m a -> Cursor -> m Cursor
execGoT go cursor = liftM snd $ runGoT go cursor

-- | Runs a Go monad on a cursor.  See 'runGoT'.
runGo :: GoM a -> Cursor -> (a, Cursor)
runGo go = runIdentity . runGoT go

-- | Runs a Go monad on a cursor and returns the value in the monad.
evalGo :: GoM a -> Cursor -> a
evalGo m cursor = fst $ runGo m cursor

-- | Runs a Go monad on a cursor and returns the final cursor.
execGo :: GoM a -> Cursor -> Cursor
execGo m cursor = snd $ runGo m cursor

getState :: Monad m => GoT m (GoState (GoT m))
getState = GoT State.get

putState :: Monad m => GoState (GoT m) -> GoT m ()
putState = GoT . State.put

modifyState :: Monad m => (GoState (GoT m) -> GoState (GoT m)) -> GoT m ()
modifyState = GoT . State.modify

instance Monad m => MonadGo (GoT m) where
  getCursor = liftM stateCursor getState

  goUp = do
    index <- liftM cursorChildIndex getCursor
    goUp' $ \pathStack -> case pathStack of
      [] -> pathStack
      path:paths -> (GoDown index:path):paths

  goDown index = goDown' index $ \pathStack -> case pathStack of
    [] -> pathStack
    path:paths -> (GoUp index:path):paths

  goToRoot = whileM (isJust . cursorParent <$> getCursor) goUp

  goToGameInfoNode goToRootIfNotFound = pushPosition >> findGameInfoNode
    where findGameInfoNode = do
            cursor <- getCursor
            if hasGameInfo cursor
              then dropPosition >> return True
              else if isNothing $ cursorParent cursor
                   then do if goToRootIfNotFound then dropPosition else popPosition
                           return False
                   else goUp >> findGameInfoNode
          hasGameInfo cursor = internalIsGameInfoNode $ cursorNode cursor

  pushPosition = modifyState $ \state ->
    state { statePathStack = []:statePathStack state }

  popPosition = do
    getPathStack >>= \stack -> when (null stack) $
      fail "popPosition: No position to pop from the stack."

    -- Drop each step in the top list of the path stack one at a time, until the
    -- top list is empty.
    whileM' (do path:_ <- getPathStack
                return $ if null path then Nothing else Just $ head path) $
      flip takeStepM $ \((_:steps):paths) -> steps:paths

    -- Finally, drop the empty top of the path stack.
    modifyState $ \state -> case statePathStack state of
      []:rest -> state { statePathStack = rest }
      _ -> error "popPosition: Internal failure, top of path stack is not empty."

  dropPosition = do
    state <- getState
    -- If there are >=2 positions on the path stack, then we can't simply drop
    -- the moves that will return us to the top-of-stack position, because they
    -- may still be needed to return to the second-on-stack position by a
    -- following popPosition.
    case statePathStack state of
      x:y:xs -> putState $ state { statePathStack = (x ++ y):xs }
      _:[] -> putState $ state { statePathStack = [] }
      [] -> fail "dropPosition: No position to drop from the stack."

  modifyProperties fn = do
    oldCursor <- getCursor
    let oldProperties = cursorProperties oldCursor
    newProperties <- fn oldProperties
    modifyState $ \state ->
      state { stateCursor = cursorModifyNode
                            (\node -> node { nodeProperties = newProperties })
                            oldCursor
            }
    fire propertiesChangedEvent (\f -> f oldProperties newProperties)

    -- The current game info changes when modifying game info properties on the
    -- current node.  I think comparing game info properties should be faster
    -- than comparing 'GameInfo's.
    let filterToGameInfo = nub . filter ((GameInfoProperty ==) . propertyType)
        oldGameInfo = filterToGameInfo oldProperties
        newGameInfo = filterToGameInfo newProperties
    when (newGameInfo /= oldGameInfo) $ do
      newCursor <- getCursor
      fire gameInfoChangedEvent (\f -> f (boardGameInfo $ cursorBoard oldCursor)
                                         (boardGameInfo $ cursorBoard newCursor))

  getProperty descriptor = find (propertyPredicate descriptor) <$> getProperties

  modifyProperty descriptor fn = do
    cursor <- getCursor
    let node = cursorNode cursor
        old = findProperty descriptor node
        new = fn old
    when (maybe False (not . propertyPredicate descriptor) new) $
      fail $ "modifyProperty: May not change property type: " ++
      show old ++ " -> " ++ show new ++ "."
    case (old, new) of
      (Just _, Nothing) -> modifyProperties $ return . remove descriptor
      (Nothing, Just value') -> modifyProperties $ return . add value'
      (Just value, Just value') | value /= value' ->
        modifyProperties $ return . add value' . remove descriptor
      _ -> return ()
    where remove descriptor = filter (not . propertyPredicate descriptor)
          add value = (value:)

  modifyGameInfo fn = do
    cursor <- getCursor
    let info = boardGameInfo $ cursorBoard cursor
        info' = fn info
    when (gameInfoRootInfo info /= gameInfoRootInfo info') $
      fail "Illegal modification of root info in modifyGameInfo."
    pushPosition
    goToGameInfoNode True
    modifyProperties $ \props ->
      return $ gameInfoToProperties info' ++ filter ((GameInfoProperty /=) . propertyType) props
    popPosition
    return info'

  modifyVariationMode fn = do
    pushPosition
    goToRoot
    modifyPropertyValue propertyST $ \maybeOld ->
      -- If the new variation mode is equal to the old effective variation mode
      -- (effective applying the default if the property isn't present), then
      -- leave the property unchanged.  Otherwise, apply the new variation mode,
      -- deleting the property if the default variation mode is selected.  We
      -- don't delete the property if @maybeOld == Just new == Just
      -- defaultVariationMode@, because we don't want to trigger dirtyness
      -- unnecessarily.
      let old = fromMaybe defaultVariationMode maybeOld
          new = fn old
      in if new == old
         then maybeOld
         else if new == defaultVariationMode
              then Nothing
              else Just new
    popPosition

  addChild index node = do
    cursor <- getCursor
    let childCount = cursorChildCount cursor
    when (index < 0 || index > childCount) $ fail $
      "Monad.addChild: Index " ++ show index ++ " is not in [0, " ++ show childCount ++ "]."
    let cursor' = cursorModifyNode (addChildAt index node) cursor
    modifyState $ \state ->
      state { stateCursor = cursor'
            , statePathStack = updatePathStackCurrentNode
                               (\step -> case step of
                                   GoUp n -> GoUp $ if n < index then n else n + 1
                                   down@(GoDown _) -> down)
                               (\step -> case step of
                                   up@(GoUp _) -> up
                                   GoDown n -> GoDown $ if n < index then n else n + 1)
                               cursor'
                               (statePathStack state)
            }
    fire childAddedEvent (\f -> f index (cursorChild cursor' index))

  on event handler = modifyState $ addHandler event handler

-- | Takes a step up the game tree, updates the path stack according to the
-- given function, then fires navigation and game info changed events as
-- appropriate.
goUp' :: Monad m => (PathStack -> PathStack) -> GoT m ()
goUp' pathStackFn = do
  state@(GoState { stateCursor = cursor
                 , statePathStack = pathStack
                 }) <- getState
  case cursorParent cursor of
    Nothing -> fail $ "goUp': Can't go up from a root cursor: " ++ show cursor
    Just parent -> do
      let index = cursorChildIndex cursor
      putState state { stateCursor = parent
                     , statePathStack = pathStackFn pathStack
                     }
      fire navigationEvent ($ GoUp index)

      -- The current game info changes when navigating up from a node that has
      -- game info properties.
      when (any ((GameInfoProperty ==) . propertyType) $ cursorProperties cursor) $
        fire gameInfoChangedEvent (\f -> f (boardGameInfo $ cursorBoard cursor)
                                           (boardGameInfo $ cursorBoard parent))

-- | Takes a step down the game tree, updates the path stack according to the
-- given function, then fires navigation and game info changed events as
-- appropriate.
goDown' :: Monad m => Int -> (PathStack -> PathStack) -> GoT m ()
goDown' index pathStackFn = do
  state@(GoState { stateCursor = cursor
                 , statePathStack = pathStack
                 }) <- getState
  case drop index $ cursorChildren cursor of
    [] -> fail $ "goDown': Cursor does not have a child #" ++ show index ++ ": " ++ show cursor
    child:_ -> do
      putState state { stateCursor = child
                     , statePathStack = pathStackFn pathStack
                     }
      fire navigationEvent ($ GoDown index)

      -- The current game info changes when navigating down to a node that has
      -- game info properties.
      when (any ((GameInfoProperty ==) . propertyType) $ cursorProperties child) $
        fire gameInfoChangedEvent (\f -> f (boardGameInfo $ cursorBoard cursor)
                                           (boardGameInfo $ cursorBoard child))

-- | Returns the current path stack.
getPathStack :: Monad m => GoT m PathStack
getPathStack = liftM statePathStack getState

-- | Maps over a path stack, updating with the given functions all steps that
-- enter and leave the cursor's current node.
updatePathStackCurrentNode :: (Step -> Step)
                           -> (Step -> Step)
                           -> Cursor
                           -> PathStack
                           -> PathStack
updatePathStackCurrentNode _ _ _ [] = []
updatePathStackCurrentNode onEnter onExit cursor0 paths =
  snd $ mapAccumL updatePath (cursor0, []) paths
  where updatePath :: (Cursor, [Step]) -> [Step] -> ((Cursor, [Step]), [Step])
        updatePath = mapAccumL updateStep
        updateStep :: (Cursor, [Step]) -> Step -> ((Cursor, [Step]), Step)
        updateStep (cursor, []) step = ((takeStep step cursor, [reverseStep step]), onExit step)
        updateStep (cursor, pathToInitial@(stepToInitial:restToInitial)) step =
          let pathToInitial' = if stepToInitial == step
                               then restToInitial
                               else reverseStep step:pathToInitial
          in ((takeStep step cursor, pathToInitial'),
              if null pathToInitial' then onEnter step else step)

-- | Fires all of the handlers for the given event, using the given function to
-- create a Go action from each of the handlers (normally themselves functions
-- that create Go actions, if they're not just Go actions directly, depending on
-- the event).
fire :: Monad m => Event (GoT m) h -> (h -> GoT m ()) -> GoT m ()
fire event handlerGenerator = do
  state <- getState
  mapM_ handlerGenerator $ eventStateGetter event state

-- | A type of event in the Go monad transformer that can be handled by
-- executing an action.  @go@ is the type of the type of the Go
-- monad/transformer.  @h@ is the type of monad or monadic function which will
-- be used by Go actions that can trigger the event.  For example, a navigation
-- event is characterized by a 'Step' that cannot easily be recovered from the
-- regular monad state, and comparing before-and-after states would be a pain.
-- So @h@ for navigation events is @'Step' -> go ()@; a handler takes a 'Step'
-- and returns a Go action to run as a result.
data Event go h = Event { eventName :: String
                        , eventStateGetter :: GoState go -> [h]
                        , eventStateSetter :: [h] -> GoState go -> GoState go
                        }

instance Show (Event go h) where
  show = eventName

addHandler :: Event go h -> h -> GoState go -> GoState go
addHandler event handler state =
  eventStateSetter event (eventStateGetter event state ++ [handler]) state

-- | An event corresponding to a child node being added to the current node.
childAddedEvent :: Event go (ChildAddedHandler go)
childAddedEvent = Event {
  eventName = "childAddedEvent"
  , eventStateGetter = stateChildAddedHandlers
  , eventStateSetter = \handlers state -> state { stateChildAddedHandlers = handlers }
  }

-- | A handler for 'childAddedEvent's.
type ChildAddedHandler go = Int -> Cursor -> go ()

-- | An event that is fired when the current game info changes, either by
-- navigating past a node with game info properties, or by modifying the current
-- game info properties.
gameInfoChangedEvent :: Event go (GameInfoChangedHandler go)
gameInfoChangedEvent = Event {
  eventName = "gameInfoChangedEvent"
  , eventStateGetter = stateGameInfoChangedHandlers
  , eventStateSetter = \handlers state -> state { stateGameInfoChangedHandlers = handlers }
  }

-- | A handler for 'gameInfoChangedEvent's.  It is called with the old game info
-- then the new game info.
type GameInfoChangedHandler go = GameInfo -> GameInfo -> go ()

-- | An event that is fired when a single step up or down in a game tree is
-- made.
navigationEvent :: Event go (NavigationHandler go)
navigationEvent = Event {
  eventName = "navigationEvent"
  , eventStateGetter = stateNavigationHandlers
  , eventStateSetter = \handlers state -> state { stateNavigationHandlers = handlers }
  }

-- | A handler for 'navigationEvent's.
--
-- A navigation handler may navigate further, but beware infinite recursion.  A
-- navigation handler must end on the same node on which it started.
type NavigationHandler go = Step -> go ()

-- | An event corresponding to a change to the properties list of the current
-- node.
propertiesChangedEvent :: Event go (PropertiesChangedHandler go)
propertiesChangedEvent = Event {
  eventName = "propertiesChangedEvent"
  , eventStateGetter = statePropertiesChangedHandlers
  , eventStateSetter = \handlers state -> state { statePropertiesChangedHandlers = handlers }
  }

-- | A handler for 'propertiesChangedEvent's.  It is called with the old
-- property list then the new property list.
type PropertiesChangedHandler go = [Property] -> [Property] -> go ()

-- | An event corresponding to a change in the active 'VariationMode'.  This can
-- happen when modifying the 'ST' property, and also when navigating between
-- collections (as they have different root nodes).
variationModeChangedEvent :: Event go (VariationModeChangedHandler go)
variationModeChangedEvent = Event {
  eventName = "variationModeChangedEvent"
  , eventStateGetter = stateVariationModeChangedHandlers
  , eventStateSetter = \handlers state -> state { stateVariationModeChangedHandlers = handlers }
  }
-- TODO Test that this is fired when moving between root nodes in a collection.
-- For now, since we don't support multiple trees in a collection, we don't need
-- to worry about checking for active variation mode change on navigation.

-- | A handler for 'variationModeChangedEvent's.  It is called with the old
-- variation mode then the new variation mode.
type VariationModeChangedHandler go = VariationMode -> VariationMode -> go ()
