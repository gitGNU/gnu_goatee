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

-- | A monad for working with game trees.
module Game.Goatee.Lib.Monad (
  -- * The Go monad
  MonadGo (..),
  GoT, GoM,
  runGoT, runGo,
  evalGoT, evalGo,
  execGoT, execGo,
  Step (..),
  NodeDeleteResult (..),
  -- * Event handling
  Event, AnyEvent (..), eventName, fire, eventHandlerFromAction,
  -- * Events
  childAddedEvent, ChildAddedHandler,
  childDeletedEvent, ChildDeletedHandler,
  gameInfoChangedEvent, GameInfoChangedHandler,
  navigationEvent, NavigationHandler,
  propertiesModifiedEvent, PropertiesModifiedHandler,
  variationModeChangedEvent, VariationModeChangedHandler,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), Applicative ((<*>), pure))
#endif
#if !MIN_VERSION_containers(0,5,0)
import Control.Arrow (second)
#endif
import Control.Monad ((<=<), ap, forM, forM_, liftM, msum, unless, when)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Monad.State as State
import Control.Monad.State (MonadState, StateT, get, put)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Writer.Class (MonadWriter, listen, pass, tell, writer)
import qualified Data.Function as F
import Data.List (delete, find, mapAccumL, nub, sortBy)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Game.Goatee.Common
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Property
import qualified Game.Goatee.Lib.Tree as Tree
import Game.Goatee.Lib.Tree hiding (addChild, addChildAt, deleteChildAt)
import Game.Goatee.Lib.Types

-- | The internal state of a Go monad transformer.  @go@ is the type of
-- Go monad or transformer (instance of 'GoMonad').
data GoState go = GoState
  { stateCursor :: Cursor
    -- ^ The current position in the game tree.
  , statePathStack :: PathStack
    -- ^ The current path stack.

    -- Event handlers.
  , stateChildAddedHandlers :: [ChildAddedHandler go]
    -- ^ Handlers for 'childAddedEvent'.
  , stateChildDeletedHandlers :: [ChildDeletedHandler go]
    -- ^ Handlers for 'childDeletedEvent'.
  , stateGameInfoChangedHandlers :: [GameInfoChangedHandler go]
    -- ^ Handlers for 'gameInfoChangedEvent'.
  , stateNavigationHandlers :: [NavigationHandler go]
    -- ^ Handlers for 'navigationEvent'.
  , statePropertiesModifiedHandlers :: [PropertiesModifiedHandler go]
    -- ^ Handlers for 'propertiesModifiedEvent'.
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
                              , stateChildDeletedHandlers = []
                              , stateGameInfoChangedHandlers = []
                              , stateNavigationHandlers = []
                              , statePropertiesModifiedHandlers = []
                              , stateVariationModeChangedHandlers = []
                              }

-- | A single step along a game tree.  Either up or down.
data Step =
  GoUp Int
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

-- | Internal function.  Takes a 'Step' in the Go monad and returns whether that
-- step was successful (i.e. if there was a node to mode to).  Updates the path
-- stack accordingly.
takeStepM :: Monad m => Step -> (PathStack -> PathStack) -> GoT m Bool
takeStepM step = case step of
  GoUp _ -> goUp'
  GoDown index -> goDown' index

-- | A monad (transformer) for navigating and mutating 'Cursor's, and
-- remembering previous locations.  See 'GoT' and 'GoM'.
--
-- The monad supports handlers for events raised during actions it takes, such
-- as navigating through the tree and modifying nodes.
class (Functor go, Applicative go, Monad go) => MonadGo go where
  -- | Returns the current cursor.
  getCursor :: go Cursor

  -- | Returns the 'CoordState' at the given point.
  getCoordState :: Coord -> go CoordState
  getCoordState coord = liftM (boardCoordState coord . cursorBoard) getCursor

  -- | Navigates up to the parent node, fires a 'navigationEvent', then returns
  -- true.  If already at the root of the tree, then none of this happens and
  -- false is returned.
  goUp :: go Bool

  -- | Navigates down the tree to the child with the given index, fires a
  -- 'navigationEvent', then returns true.  If the requested child doesn't
  -- exist, then none of this happens and false is returned.
  goDown :: Int -> go Bool

  -- | If possible, moves to the sibling node immediately to the left of the
  -- current one.  Returns whether a move was made (i.e. whether there was a
  -- left sibling).  Fires 'navigationEvent's while moving.
  goLeft :: go Bool

  -- | If possible, moves to the sibling node immediately to the right of the
  -- current one.  Returns whether a move was made (i.e. whether there was a
  -- right sibling).  Fires 'navigationEvent's while moving.
  goRight :: go Bool

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

  -- | Modifies the set of properties on the current node.  Fires
  -- 'propertiesModifiedEvent' after modifying if the new property set is
  -- different from the old property set (order is irrelevant).
  modifyProperties :: ([Property] -> [Property]) -> go ()

  -- | Searches for a property on the current node, returning it if found.
  getProperty :: Descriptor d => d -> go (Maybe Property)

  -- | Searches for a valued property on the current node, returning its value
  -- if found.
  getPropertyValue :: ValuedDescriptor v d => d -> go (Maybe v)
  getPropertyValue descriptor = liftM (liftM $ propertyValue descriptor) $ getProperty descriptor

  -- | Sets a property on the current node, replacing an existing property with
  -- the same name, if one exists.  Fires 'propertiesModifiedEvent' if the
  -- property has changed.
  putProperty :: Property -> go ()
  putProperty property = modifyProperty property $ const $ Just property

  -- | Deletes a property from the current node, if it's set, and fires
  -- 'propertiesModifiedEvent'.
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
  -- existing on the node, and a 'Just' marks the property's presence.  Fires
  -- 'propertiesModifiedEvent' if the property changed.  This function does not
  -- do any validation to check that the resulting tree state is valid.
  modifyProperty :: Descriptor d => d -> (Maybe Property -> Maybe Property) -> go ()

  -- | Calls the given function to modify the state of the given valued property
  -- (descriptor) on the current node.  'Nothing' represents the property not
  -- existing on the node, and a 'Just' with the property's value marks the
  -- property's presence.  Fires 'propertiesModifiedEvent' if the property
  -- changed.  This function does not do any validation to check that the
  -- resulting tree state is valid.
  modifyPropertyValue :: ValuedDescriptor v d => d -> (Maybe v -> Maybe v) -> go ()
  modifyPropertyValue descriptor fn = modifyProperty descriptor $ \old ->
    propertyBuilder descriptor <$> fn (propertyValue descriptor <$> old)

  -- | Mutates the string-valued property attached to the current node according
  -- to the given function.  The input string will be empty if the current node
  -- either has the property with an empty value, or doesn't have the property.
  -- Returning an empty string removes the property from the node, if it was
  -- set.  Fires 'propertiesModifiedEvent' if the property changed.
  modifyPropertyString :: (Stringlike s, ValuedDescriptor s d) => d -> (String -> String) -> go ()
  modifyPropertyString descriptor fn =
    modifyPropertyValue descriptor $ \value -> case fn (maybe "" sgfToString value) of
      "" -> Nothing
      str -> let sgf = stringToSgf str
                 -- Because stringToSgf might do processing, we have to check
                 -- the conversion back to a string for emptiness.
             in if null $ sgfToString sgf then Nothing else Just sgf

  -- | Mutates the list-valued property attached to the current node
  -- according to the given function.  The input list will be empty if the
  -- current node either has the property with an empty value, or doesn't have
  -- the property.  Returning an empty list removes the property from the node,
  -- if it was set.
  --
  -- Fires 'propertiesModifiedEvent' if the property changed.
  --
  -- See also 'modifyPropertyCoords'.
  modifyPropertyList :: ValuedDescriptor [v] d => d -> ([v] -> [v]) -> go ()
  modifyPropertyList descriptor fn =
    modifyPropertyValue descriptor $ \value -> case fn $ fromMaybe [] value of
      [] -> Nothing
      value' -> Just value'

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
  --
  -- Fires 'propertiesModifiedEvent' if the property changed.
  modifyPropertyCoords :: ValuedDescriptor CoordList d => d -> ([Coord] -> [Coord]) -> go ()
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

  -- | Retrieves the stone assigned in the current node to a point by 'AB',
  -- 'AE', or 'AW'.  The possible results are:
  --
  -- * @Nothing@: No stone has been assigned to the point.  The point could
  -- still be in any state, e.g. from a play on the current node or some
  -- property in an ancestor node.
  --
  -- * @Just Nothing@: The point has been assigned to be empty.
  --
  -- * @Just (Just Color)@: The point has been assigned to have a stone of the
  -- given color.
  getAssignedStone :: Coord -> go (Maybe (Maybe Color))
  getAssignedStone coord =
    fmap msum $ forM stoneAssignmentProperties $ \descriptor ->
    ((\coords -> if coord `elem` coords
                 then Just $ stoneAssignmentPropertyToStone descriptor
                 else Nothing) <=<
     fmap expandCoordList) <$>
    getPropertyValue descriptor

  -- | Looks up all stones that are assigned by 'AB', 'AE', or 'AW' properties
  -- on the current node.  Returns a map from each point to the stone that is
  -- assigned to the point.
  getAllAssignedStones :: go (Map Coord (Maybe Color))
  getAllAssignedStones =
    fmap Map.unions $ forM stoneAssignmentProperties $ \descriptor ->
    let stone = stoneAssignmentPropertyToStone descriptor
    in Map.fromList . map (\coord -> (coord, stone)) . maybe [] expandCoordList <$>
       getPropertyValue descriptor

  -- | Modifies the state of currently assigned stones, keeping in mind that it
  -- is invalid to mix 'MoveProperty' and 'SetupProperty' properties in a single
  -- node.  This function has the behaviour of a user changing stone assignments
  -- in a UI.  How this function works is:
  --
  -- * Pick a node to work with.  If there is a move property on the current
  -- node and there is not already a setup property on the current node, then
  -- we'll create and modify a new child node.  Otherwise, either there are no
  -- move properties on the node (so we can add setup properties at will), or
  -- there are both move and setup properties on the node (the node is already
  -- invalid), so we'll just modify the current node.
  --
  -- * If we're modifying the current node, then apply the modification function
  -- to the state of stone assignment for each coordinate.  See
  -- 'getAssignedStone' for the meaning of @Maybe (Maybe Color)@.  Modify the
  -- properties in the node as necessary to apply the result
  -- ('propertiesModifiedEvent').  (__NOTE:__ Currently one event is fired for
  -- each property modified; this may change in the future.)
  --
  -- * If we need to create a child node, then apply the modification function
  -- to 'Nothing' to determine if we're actually adding assignments.  If the
  -- function returns a 'Just', then we create a child node with the necessary
  -- assignment properties, insert it ('childAddedEvent'), then navigate to it
  -- ('navigationEvent').  If the function returns 'Nothing', then
  -- 'modifyAssignedStones' does nothing.
  modifyAssignedStones :: [Coord] -> (Maybe (Maybe Color) -> Maybe (Maybe Color)) -> go ()
  modifyAssignedStones coords f = do
    needChild <- ((&&) <$> notElem SetupProperty <*> elem MoveProperty) .
                 map propertyType <$>
                 getProperties
    if needChild
      then case f Nothing of
        Nothing -> return ()
        Just assignedStone -> do
          addChild emptyNode { nodeProperties =
                               [propertyBuilder (stoneToStoneAssignmentProperty assignedStone) $
                                buildCoordList coords]
                             }
          ok <- goDown =<< subtract 1 . length . cursorChildren <$> getCursor
          unless ok $ fail "GoT.modifyAssignedStones: Failed to move to new child."
      else do
        -- Get a map from getAllAssignedStones: Map Coord (Maybe Color)
        allAssignedStones <- getAllAssignedStones
        let -- For each coord in coords, modify the map.
            allAssignedStones' = foldr (Map.alter f) allAssignedStones coords
            -- Invert both maps.
            byStone, byStone' :: Map (Maybe Color) [Coord]
            byStone = mapInvert allAssignedStones
            byStone' = mapInvert allAssignedStones'
            -- Compute a diff between the two maps.
            diff :: Map (Maybe Color) ([Coord], [Coord])
#if MIN_VERSION_containers(0,5,0)
            diff = Map.mergeWithKey
                   (\_ oldCoords newCoords -> if newCoords == oldCoords
                                              then Nothing
                                              else Just (oldCoords, newCoords))
                   (Map.map $ \oldCoords -> (oldCoords, []))
                   (Map.map $ \newCoords -> ([], newCoords))
                   byStone
                   byStone'
#else
            -- GHC 7.4.2 / containers <0.5.0 don't provide map merging.
            diff = (\partialDiff ->
                     foldr (\(stone, new) ->
                             Map.alter (Just . maybe ([], new) (second $ const new))
                                       stone)
                           partialDiff
                           (Map.assocs byStone')) $
                   Map.map (\old -> (old, [])) byStone
#endif
        -- Modify the AB,AE,AW properties for the stones that have changed lists.
        forM_ (Map.assocs diff) $ \(stone, (oldCoords, newCoords)) ->
          when (newCoords /= oldCoords) $
          modifyPropertyCoords (stoneToStoneAssignmentProperty stone) $ const newCoords

  -- | Returns the 'Mark' at a point on the current node.
  getMark :: Coord -> go (Maybe Mark)
  getMark = liftM coordMark . getCoordState

  -- | Calls the given function to modify the presence of a 'Mark' on the
  -- current node.
  modifyMark :: (Maybe Mark -> Maybe Mark) -> Coord -> go ()
  modifyMark f coord = do
    maybeOldMark <- getMark coord
    case (maybeOldMark, f maybeOldMark) of
      (Just oldMark, Nothing) -> remove oldMark
      (Nothing, Just newMark) -> add newMark
      (Just oldMark, Just newMark) | oldMark /= newMark -> remove oldMark >> add newMark
      (Just _, Just _) -> return ()
      (Nothing, Nothing) -> return ()
    where remove mark = modifyPropertyCoords (markProperty mark) (delete coord)
          add mark = modifyPropertyCoords (markProperty mark) (coord:)

  -- | Adds a child node to the current node at the end of the current node's
  -- child list.  Fires a 'childAddedEvent' after the child is added.
  addChild :: Node -> go ()
  addChild node = do
    childCount <- liftM (length . cursorChildren) getCursor
    addChildAt childCount node

  -- | Adds a child node to the current node at the given index, shifting all
  -- existing children at and after the index to the right.  The index must be
  -- in the range @[0, numberOfChildren]@.  Fires a 'childAddedEvent' after the
  -- child is added.
  addChildAt :: Int -> Node -> go ()

  -- | Tries to remove the child node at the given index below the current node.
  -- Returns a status code indicating whether the deletion succeeded, or why
  -- not.
  deleteChildAt :: Int -> go NodeDeleteResult

  -- | Registers a new event handler for a given event type.
  on :: Event go h -> h -> go ()

  -- | Registers a new event handler for a given event type.  Unlike 'on', whose
  -- handler may receive arguments, the handler given here doesn't receive any
  -- arguments.
  on0 :: Event go h -> go () -> go ()
  on0 event handler = on event $ eventHandlerFromAction event handler

-- | The result of deleting a node.
data NodeDeleteResult =
  NodeDeleteOk
  -- ^ The node was deleted successfully.
  | NodeDeleteBadIndex
    -- ^ The node couldn't be deleted, because an invalid index was given.
  | NodeDeleteOnPathStack
    -- ^ The node couldn't be deleted, because it is on the path stack.
  deriving (Bounded, Enum, Eq, Show)

-- | The standard monad transformer for 'MonadGo'.
newtype GoT m a = GoT { goState :: StateT (GoState (GoT m)) m a }

-- | The standard monad for 'MonadGo'.
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

instance MonadState s m => MonadState s (GoT m) where
  get = lift get
  put = lift . put

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

  -- TODO For goUp and goDown, optimize by seeing checking if (head $ head
  -- pathStack) is the step we're taking, and if so, dropping it from the list
  -- rather than pushing a fresh step.
  goUp = do
    index <- liftM cursorChildIndex getCursor
    goUp' $ \pathStack -> case pathStack of
      [] -> pathStack
      path:paths -> (GoDown index:path):paths

  goDown index = goDown' index $ \pathStack -> case pathStack of
    [] -> pathStack
    path:paths -> (GoUp index:path):paths

  goLeft = do
    cursor <- getCursor
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return False
      (Just _, 0) -> return False
      (Just _, n) -> do True <- goUp
                        True <- goDown $ n - 1
                        return True

  goRight = do
    cursor <- getCursor
    case (cursorParent cursor, cursorChildIndex cursor) of
      (Nothing, _) -> return False
      (Just parent, n) | n == cursorChildCount parent - 1 -> return False
      (Just _, n) -> do True <- goUp
                        True <- goDown $ n + 1
                        return True

  goToRoot = whileM goUp $ return ()

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
                return $ if null path then Nothing else Just $ head path) $ \step -> do
      ok <- takeStepM step $ \((_:steps):paths) -> steps:paths
      unless ok $ fail "popPosition: Failed to retrace steps."

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
      [_] -> putState $ state { statePathStack = [] }
      [] -> fail "dropPosition: No position to drop from the stack."

  modifyProperties fn = do
    oldCursor <- getCursor
    let oldProperties = cursorProperties oldCursor
        newProperties = fn oldProperties
    modifyState $ \state ->
      state { stateCursor = cursorModifyNode
                            (\node -> node { nodeProperties = newProperties })
                            oldCursor
            }
    when (sortBy (comparing propertyName) newProperties /=
          sortBy (comparing propertyName) oldProperties) $
      fire propertiesModifiedEvent (\f -> f oldProperties newProperties)

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
      (Just _, Nothing) -> modifyProperties $ remove descriptor
      (Nothing, Just value') -> modifyProperties $ add value'
      (Just value, Just value') | value /= value' ->
        modifyProperties $ add value' . remove descriptor
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
    _ <- goToGameInfoNode True
    modifyProperties $ \props ->
      gameInfoToProperties info' ++ filter ((GameInfoProperty /=) . propertyType) props
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

  addChildAt index node = do
    cursor <- getCursor
    let childCount = cursorChildCount cursor
    when (index < 0 || index > childCount) $ fail $
      "Monad.addChildAt: Index " ++ show index ++ " is not in [0, " ++ show childCount ++ "]."
    let cursor' = cursorModifyNode (Tree.addChildAt index node) cursor
    modifyState $ \state ->
      state { stateCursor = cursor'
            , statePathStack = foldPathStack
                               (\step -> case step of
                                   GoUp n -> GoUp $ if n < index then n else n + 1
                                   down@(GoDown _) -> down)
                               (\step -> case step of
                                   up@(GoUp _) -> up
                                   GoDown n -> GoDown $ if n < index then n else n + 1)
                               id
                               cursor'
                               (statePathStack state)
            }
    fire childAddedEvent ($ index)

  deleteChildAt index = do
    childCount <- cursorChildCount <$> getCursor
    if index < 0 || index >= childCount
      then return NodeDeleteBadIndex
      else do goDown index >>=
                \ok -> unless ok $ fail "GoT.deleteChildAt: Internal error, index isn't valid."
              childCursor <- getCursor
              deletingNodeOnPath <- doesPathStackEnterCurrentNode <$>
                                    pure childCursor <*> getPathStack
              goUp >>= \ok -> unless ok $ fail "GoT.deleteChildAt: Internal error, can't go up."
              if deletingNodeOnPath
                then return NodeDeleteOnPathStack
                else do cursor <- getCursor
                        let cursor' = cursorModifyNode (Tree.deleteChildAt index) cursor
                        modifyState $ \state ->
                          state { stateCursor = cursor'
                                , statePathStack =
                                  foldPathStack
                                  (\step -> case step of
                                      GoUp n -> GoUp $ if n < index then n else n - 1
                                      down@(GoDown _) -> down)
                                  (\step -> case step of
                                      up@(GoUp _) -> up
                                      GoDown n -> GoDown $ if n < index then n else n - 1)
                                  id
                                  cursor'
                                  (statePathStack state)
                                }
                        fire childDeletedEvent ($ childCursor)
                        return NodeDeleteOk

  on event handler = modifyState $ addHandler event handler

-- | Takes a step up the game tree, updates the path stack according to the
-- given function, then fires navigation and game info changed events as
-- appropriate, finally returning true.  When at the root of the tree, none of
-- this happens and false is returned.
goUp' :: Monad m => (PathStack -> PathStack) -> GoT m Bool
goUp' pathStackFn = do
  state@(GoState { stateCursor = cursor
                 , statePathStack = pathStack
                 }) <- getState
  case cursorParent cursor of
    Nothing -> return False
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
      return True

-- | Takes a step down the game tree, updates the path stack according to the
-- given function, then fires navigation and game info changed events as
-- appropriate, finally returning true.  When the child index is invalid, none
-- of this happens and false is returned.
goDown' :: Monad m => Int -> (PathStack -> PathStack) -> GoT m Bool
goDown' index pathStackFn = do
  state@(GoState { stateCursor = cursor
                 , statePathStack = pathStack
                 }) <- getState
  case drop index $ cursorChildren cursor of
    [] -> return False
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
      return True

-- | Returns the current path stack.
getPathStack :: Monad m => GoT m PathStack
getPathStack = liftM statePathStack getState

doesPathStackEnterCurrentNode :: Cursor -> PathStack -> Bool
doesPathStackEnterCurrentNode cursor pathStack =
  or $ or <$> foldPathStack (const True) (const False) (const False) cursor pathStack

-- | Maps over a path stack, updating with the given functions all steps that
-- enter and leave the cursor's current node.
foldPathStack :: (Step -> a)
              -> (Step -> a)
              -> (Step -> a)
              -> Cursor
              -> PathStack
              -> [[a]]
foldPathStack _ _ _ _ [] = []
foldPathStack onEnter onExit onOther cursor0 paths =
  snd $ mapAccumL updatePath (cursor0, []) paths
  where -- updatePath :: (Cursor, [Step]) -> [Step] -> ((Cursor, [Step]), [a])
        updatePath = mapAccumL updateStep
        -- updateStep :: (Cursor, [Step]) -> Step -> ((Cursor, [Step]), a)
        updateStep (cursor, []) step = ((takeStep step cursor, [reverseStep step]), onExit step)
        updateStep (cursor, pathToInitial@(stepToInitial:restToInitial)) step =
          let pathToInitial' = if stepToInitial == step
                               then restToInitial
                               else reverseStep step:pathToInitial
          in ((takeStep step cursor, pathToInitial'),
              if null pathToInitial' then onEnter step else onOther step)

-- | Fires all of the handlers for the given event, using the given function to
-- create a Go action from each of the handlers (normally themselves functions
-- that create Go actions, if they're not just Go actions directly, depending on
-- the event).
fire :: Monad m => Event (GoT m) h -> (h -> GoT m ()) -> GoT m ()
fire event handlerGenerator = do
  state <- getState
  mapM_ handlerGenerator $ eventStateGetter event state

-- | A type of event in a Go monad that can be handled by executing an action.
-- @go@ is the type of the Go monad.  @h@ is the handler type, a function that
-- takes some arguments relating to the event and returns an action in the Go
-- monad.  The arguments to the handler are usually things that would be
-- difficult to recover from the state of the monad alone, for example the
-- 'Step' associated with a 'navigationEvent'.
--
-- The 'Eq', 'Ord', and 'Show' instances use events' names, via 'eventName'.
data Event go h = Event
  { eventName :: String
  , eventStateGetter :: GoState go -> [h]
  , eventStateSetter :: [h] -> GoState go -> GoState go
  , eventHandlerFromAction :: go () -> h
  }

instance Eq (Event go h) where
  (==) = (==) `F.on` eventName

instance Ord (Event go h) where
  compare = comparing eventName

instance Show (Event go h) where
  show = eventName

-- | An existential type for any event in a particular Go monad.  Like 'Event',
-- the 'Eq', 'Ord', and 'Show' instances use events' names, via 'eventName'.
data AnyEvent go = forall h. AnyEvent (Event go h)

instance Eq (AnyEvent go) where
  (AnyEvent e) == (AnyEvent e') = eventName e == eventName e'

instance Ord (AnyEvent go) where
  compare (AnyEvent e) (AnyEvent e') = compare (eventName e) (eventName e')

instance Show (AnyEvent go) where
  show (AnyEvent e) = eventName e

addHandler :: Event go h -> h -> GoState go -> GoState go
addHandler event handler state =
  eventStateSetter event (eventStateGetter event state ++ [handler]) state

-- | An event corresponding to a child node being added to the current node.
childAddedEvent :: Event go (ChildAddedHandler go)
childAddedEvent = Event
  { eventName = "childAddedEvent"
  , eventStateGetter = stateChildAddedHandlers
  , eventStateSetter = \handlers state -> state { stateChildAddedHandlers = handlers }
  , eventHandlerFromAction = const
  }

-- | A handler for 'childAddedEvent's.  Called with the index of the child added
-- to the current node.
type ChildAddedHandler go = Int -> go ()

-- | An event corresponding to the deletion of one of the current node's
-- children.
childDeletedEvent :: Event go (ChildDeletedHandler go)
childDeletedEvent = Event
  { eventName = "childDeletedEvent"
  , eventStateGetter = stateChildDeletedHandlers
  , eventStateSetter = \handlers state -> state { stateChildDeletedHandlers = handlers }
  , eventHandlerFromAction = const
  }

-- | A handler for 'childDeletedEvent's.  It is called with a cursor at the
-- child that was deleted (this cursor is now out of date).
type ChildDeletedHandler go = Cursor -> go ()

-- | An event that is fired when the current game info changes, either by
-- navigating past a node with game info properties, or by modifying the current
-- game info properties.
gameInfoChangedEvent :: Event go (GameInfoChangedHandler go)
gameInfoChangedEvent = Event
  { eventName = "gameInfoChangedEvent"
  , eventStateGetter = stateGameInfoChangedHandlers
  , eventStateSetter = \handlers state -> state { stateGameInfoChangedHandlers = handlers }
  , eventHandlerFromAction = const . const
  }

-- | A handler for 'gameInfoChangedEvent's.  It is called with the old game info
-- then the new game info.
type GameInfoChangedHandler go = GameInfo -> GameInfo -> go ()

-- | An event that is fired when a single step up or down in a game tree is
-- made.
navigationEvent :: Event go (NavigationHandler go)
navigationEvent = Event
  { eventName = "navigationEvent"
  , eventStateGetter = stateNavigationHandlers
  , eventStateSetter = \handlers state -> state { stateNavigationHandlers = handlers }
  , eventHandlerFromAction = const
  }

-- | A handler for 'navigationEvent's.
--
-- A navigation handler may navigate further, but beware infinite recursion.  A
-- navigation handler must end on the same node on which it started.
type NavigationHandler go = Step -> go ()

-- | An event corresponding to a modification to the properties list of the
-- current node.
propertiesModifiedEvent :: Event go (PropertiesModifiedHandler go)
propertiesModifiedEvent = Event
  { eventName = "propertiesModifiedEvent"
  , eventStateGetter = statePropertiesModifiedHandlers
  , eventStateSetter = \handlers state -> state { statePropertiesModifiedHandlers = handlers }
  , eventHandlerFromAction = const . const
  }

-- | A handler for 'propertiesModifiedEvent's.  It is called with the old
-- property list then the new property list.
type PropertiesModifiedHandler go = [Property] -> [Property] -> go ()

-- | An event corresponding to a change in the active 'VariationMode'.  This can
-- happen when modifying the 'ST' property, and also when navigating between
-- collections (as they have different root nodes).
variationModeChangedEvent :: Event go (VariationModeChangedHandler go)
variationModeChangedEvent = Event
  { eventName = "variationModeChangedEvent"
  , eventStateGetter = stateVariationModeChangedHandlers
  , eventStateSetter = \handlers state -> state { stateVariationModeChangedHandlers = handlers }
  , eventHandlerFromAction = const . const
  }
-- TODO Test that this is fired when moving between root nodes in a collection.
-- For now, since we don't support multiple trees in a collection, we don't need
-- to worry about checking for active variation mode change on navigation.

-- | A handler for 'variationModeChangedEvent's.  It is called with the old
-- variation mode then the new variation mode.
type VariationModeChangedHandler go = VariationMode -> VariationMode -> go ()
