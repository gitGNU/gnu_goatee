-- | A monad for working with a game tree.
module Khumba.Goatee.Sgf.Monad (
  -- * The Go monad
  GoT
  , GoM
  , runGoT
  , runGo
  , evalGoT
  , evalGo
  , execGoT
  , execGo
  , getCursor
    -- * Navigation
  , Step(..)
  , goUp
  , goDown
  , goToRoot
  , goToGameInfoNode
    -- * Remembering positions
  , pushPosition
  , popPosition
  , dropPosition
    -- * Mutation
  , modifyGameInfo
  , addChild
    -- * Event handling
  , Event
  , on
  , fire
  , NavigationHandler
  , navigationEvent
  , PropertiesChangedHandler
  , propertiesChangedEvent
  , ChildAddedHandler
  , childAddedEvent
  ) where

import Control.Monad
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Control.Applicative
import qualified Control.Monad.State as State
import Control.Monad.State (StateT)
import Data.List (mapAccumL)
import Data.Maybe
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf hiding (addChild)

-- | The internal state of a Go monad transformer.  @go@ is the type of
-- Go monad or transformer (instance of 'GoMonad').
data GoState go = GoState { stateCursor :: Cursor
                            -- ^ The current position in the game tree.
                          , statePathStack :: [[Step]]
                            -- ^ The positions saved in calls to 'pushPosition' correspond to
                            -- entries in the outer list here, with the first sublist representing
                            -- the last call.  The sublist contains the steps in order that will
                            -- trace the path back to the saved position.

                          -- Event handlers.
                          , stateNavigationHandlers :: [NavigationHandler go]
                            -- ^ Handlers for 'navigationEvent'.
                          , statePropertiesChangedHandlers :: [PropertiesChangedHandler go]
                            -- ^ Handlers for 'propertiesChangedEvent'.
                          , stateChildAddedHandlers :: [ChildAddedHandler go]
                            -- ^ Handlers for 'childAddedEvent'.
                          }

-- | A simplified constructor function for 'GoState'.
initialState :: Cursor -> GoState m
initialState cursor = GoState { stateCursor = cursor
                              , statePathStack = []
                              , stateNavigationHandlers = []
                              , statePropertiesChangedHandlers = []
                              , stateChildAddedHandlers = []
                              }

-- | A single step along a game tree.  Either up or down.
data Step = GoUp
          | GoDown Int
          deriving (Eq, Show)

-- | Takes a 'Step' from a 'Cursor', returning a new 'Cursor'.
takeStep :: Step -> Cursor -> Cursor
takeStep GoUp cursor = fromMaybe (error $ "takeStep: Can't go up from " ++ show cursor ++ ".") $
                       cursorParent cursor
takeStep (GoDown index) cursor = cursorChild cursor index

-- | A monad (transformer) for navigating and mutating 'Cursor's, and
-- remembering previous locations.  See 'GoT' and 'GoM'.
--
-- The monad supports handlers for events raised during actions it takes, such
-- as navigating through the tree and modifying nodes.
class Monad go => MonadGo go where
  -- | Returns the current cursor.
  getCursor :: go Cursor

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

  -- | Modifies the set of properties on the current node.
  modifyProperties :: ([Property] -> [Property]) -> go ()

  -- | Deletes properties from the current node for which the predicate returns
  -- true.
  deleteProperties :: (Property -> Bool) -> go ()

  -- TODO putProperties
  putProperties :: [Property] -> go ()

  -- | Mutates the game info for the current path, returning the new info.  If
  -- the current node or one of its ancestors has game info properties, then
  -- that node is modified.  Otherwise, properties are inserted on the root
  -- node.
  modifyGameInfo :: (GameInfo -> GameInfo) -> go GameInfo

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
    state@(GoState { stateCursor = cursor
                   , statePathStack = pathStack
                   }) <- getState
    case cursorParent cursor of
      Nothing -> fail $ "Can't go up from a root cursor: " ++ show cursor
      Just parent -> do putState state { stateCursor = parent
                                       , statePathStack = case pathStack of
                                         [] -> []
                                         path:paths -> (GoDown (cursorChildIndex cursor):path):paths
                                       }
                        fire navigationEvent ($ GoUp)

  goDown childIndex = do
    state@(GoState { stateCursor = cursor
                   , statePathStack = pathStack
                   }) <- getState
    case drop childIndex $ cursorChildren cursor of
      [] -> fail $ "Cursor does not have a child #" ++ show childIndex ++ ": " ++ show cursor
      child:_ -> do putState state { stateCursor = child
                                   , statePathStack = case pathStack of
                                     [] -> []
                                     path:paths -> (GoUp:path):paths
                                   }
                    fire navigationEvent ($ GoDown childIndex)

  goToRoot = whileM (liftM (isJust . cursorParent) getCursor) goUp

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
    state@(GoState { stateCursor = cursor
                   , statePathStack = pathStack
                   }) <- getState
    when (null pathStack) $ fail "popPosition: No position to pop from the stack."
    putState state { stateCursor =
                       -- TODO Hmm, should this be firing navigation handlers?
                       foldl (\cursor step -> case step of
                               GoUp -> fromMaybe (error "popPosition: Reverse path is invalid, can't go up.")
                                                 (cursorParent cursor)
                               GoDown childIndex -> cursorChild cursor childIndex)
                             cursor
                             (head pathStack)
                   , statePathStack = tail pathStack
                   }

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

  modifyProperties fn = modifyState $ \state ->
    state { stateCursor = cursorModifyNode (\node -> node { nodeProperties = fn $ nodeProperties node })
                                           (stateCursor state)
          }

  deleteProperties pred = modifyProperties $ filter $ not . pred

  -- TODO Implement putProperties.  But how to dedup?
  putProperties _ = fail "putProperties not implemented."

  modifyGameInfo fn = do
    cursor <- getCursor
    let info = boardGameInfo $ cursorBoard cursor
        info' = fn info
    when (gameInfoRootInfo info /= gameInfoRootInfo info') $
      fail "Illegal modification of root info in modifyGameInfo."
    pushPosition
    goToGameInfoNode True
    deleteProperties ((GameInfoProperty ==) . propertyType)
    putProperties $ gameInfoToProperties info'
    popPosition
    return info'

  addChild index node = do
    cursor <- getCursor
    let childCount = cursorChildCount cursor
    when (index < 0 || index > childCount) $ fail $
      "Monad.addChild: Index " ++ show index ++ " is not in [0, " ++ show childCount ++ "]."
    let cursor' = cursorModifyNode (addChildAt index node) cursor
    modifyState $ \state -> state { stateCursor = cursor'
                                  , statePathStack = updatePathStackCurrentNode
                                                     (\step -> case step of
                                                         GoUp -> GoUp
                                                         GoDown n -> GoDown $ if n < index then n else n + 1)
                                                     cursor'
                                                     (statePathStack state)
                                  }
    fire childAddedEvent (\f -> f index (cursorChild cursor' index))

  on event handler = modifyState $ addHandler event handler

-- | Maps over a path stack, updating with the given function all steps that
-- leave the cursor's node.
updatePathStackCurrentNode :: (Step -> Step) -> Cursor -> [[Step]] -> [[Step]]
updatePathStackCurrentNode _ _ [] = []
updatePathStackCurrentNode fn cursor0 paths = snd $ mapAccumL updatePath (cursor0, []) paths
  where updatePath = mapAccumL updateStep
        updateStep (cursor, []) step = ((takeStep step cursor, [reverseStep cursor step]), fn step)
        updateStep (cursor, pathToInitial@(stepToInitial:restToInitial)) step =
          let pathToInitial' = if stepToInitial == step
                               then restToInitial
                               else reverseStep cursor step:pathToInitial
          in ((takeStep step cursor, pathToInitial'), step)
        reverseStep cursor step = case step of
          GoUp -> GoDown $ cursorChildIndex cursor
          GoDown _ -> GoUp

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
-- be used by Go actions that can trigger the event.
data Event go h = Event { eventName :: String
                        , eventStateGetter :: GoState go -> [h]
                        , eventStateSetter :: [h] -> GoState go -> GoState go
                        }

instance Show (Event go h) where
  show = eventName

addHandler :: Event go h -> h -> GoState go -> GoState go
addHandler event handler state =
  eventStateSetter event (eventStateGetter event state ++ [handler]) state

type NavigationHandler go = Step -> go ()

-- | An event that is fired when a single step up or down in a game tree is
-- made.
navigationEvent :: Event go (NavigationHandler go)
navigationEvent = Event { eventName = "navigationEvent"
                        , eventStateGetter = stateNavigationHandlers
                        , eventStateSetter = \handlers state -> state { stateNavigationHandlers = handlers }
                        }

-- TODO Include old properties?  Don't include new properties, since those are
-- accessible from the monad?
type PropertiesChangedHandler go = [Property] -> go ()

-- | An event corresponding to a change to the properties list of the current
-- node.
propertiesChangedEvent :: Event go (PropertiesChangedHandler go)
propertiesChangedEvent = Event { eventName = "propertiesChangedEvent"
                               , eventStateGetter = statePropertiesChangedHandlers
                               , eventStateSetter = \handlers state -> state { statePropertiesChangedHandlers = handlers }
                               }

type ChildAddedHandler go = Int -> Cursor -> go ()

-- | An event corresponding to a child node being added to the current node.
childAddedEvent :: Event go (ChildAddedHandler go)
childAddedEvent = Event { eventName = "childAddedEvent"
                        , eventStateGetter = stateChildAddedHandlers
                        , eventStateSetter = \handlers state -> state { stateChildAddedHandlers = handlers }
                        }
