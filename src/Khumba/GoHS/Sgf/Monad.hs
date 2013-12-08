-- | A monad for working with a game tree.
module Khumba.GoHS.Sgf.Monad ( -- * The Go monad
                               GoT
                             , BasicGoT
                             , GoM
                             , BasicGoM
                             , runGoT
                             , runBasicGoT
                             , runGo
                             , runBasicGo
                             , evalGo
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
                               -- * Event handling
                             , Event
                             , on
                             , NavigationHandler
                             , navigationEvent
                             , PropertiesChangedHandler
                             , propertiesChangedEvent
                             ) where

import Control.Monad
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Control.Applicative
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import Data.Maybe
import Data.Monoid (Monoid)
import Khumba.GoHS.Common
import Khumba.GoHS.Sgf

-- | The internal state of a Go monad.
data GoState h = GoState { stateCursor :: Cursor
                           -- ^ The current position in the game tree.
                         , statePathStack :: [[Step]]
                           -- ^ The positions saved in calls to 'pushPosition' correspond to
                           -- entries in the outer list here, with the first sublist representing
                           -- the last call.  The sublist contains the steps in order that will
                           -- trace the path back to the saved position.
                         , stateHandlerActions :: [h Bool]
                           -- ^ The list of actions to take as a result of event handlers being
                           -- triggered.  This list is stored in reverse order -- the last action
                           -- should be executed first.

                         -- Event handlers.  Handlers are stored in reverse order to that in which
                         -- they are added, so these lists should be reversed before being
                         -- invoked.
                         , stateNavigationHandlers :: [NavigationHandler h]
                           -- ^ Handlers for 'navigationEvent'.
                         , statePropertiesChangedHandlers :: [PropertiesChangedHandler h]
                           -- ^ Handlers for 'propertiesChangedEvent'.
                         }

-- | A simplified constructor function for 'GoState'.
initialState :: Cursor -> GoState h
initialState cursor = GoState { stateCursor = cursor
                              , statePathStack = []
                              , stateHandlerActions = []
                              , stateNavigationHandlers = []
                              , statePropertiesChangedHandlers = []
                              }

-- | A single step along a game tree.  Either up or down.
data Step = GoUp
          | GoDown Int
          deriving (Show)

-- | A monad (transformer) for navigating and mutating 'Cursor's, and
-- remembering previous locations.  See 'GoT' and 'GoM'.
--
-- The monad supports handlers for actions it takes, such as navigating through
-- the tree and modifying nodes.  @h@ is the monad in which these handlers run,
-- for example 'IO'.
class (Monad h, Monad m) => MonadGo h m | m -> h where
  -- | Returns the current cursor.
  getCursor :: m Cursor

  -- | Navigates up the tree.  It must be valid to do so, otherwise 'fail' is
  -- called.
  goUp :: m ()

  -- | Navigates down the tree to the child with the given index.  The child
  -- must exist.
  goDown :: Int -> m ()

  -- | Navigates up to the root of the tree.
  goToRoot :: m ()

  -- | Navigates up the tree to the node containing game info properties, if
  -- any.  Returns true if a game info node was found.
  goToGameInfoNode :: Bool
                      -- ^ When no node with game info is found, then if false,
                      -- return to the original node, otherwise finish at the
                      -- root node.
                   -> m Bool

  -- | Pushes the current location in the game tree onto an internal position
  -- stack, such that 'popPosition' is capable of navigating back to the same
  -- position, even if the game tree has been modified (though the old position
  -- must still exist in the tree to return to it).
  pushPosition :: m ()

  -- | Returns to the last position pushed onto the internal position stack via
  -- 'pushPosition'.  This action must be balanced by a 'pushPosition'.
  popPosition :: m ()

  -- | Drops the last position pushed onto the internal stack by 'pushPosition'
  -- off of the stack.  This action must be balanced by a 'pushPosition'.
  dropPosition :: m ()

  -- | Modifies the set of properties on the current node.
  modifyProperties :: ([Property] -> [Property]) -> m ()

  -- | Deletes properties from the current node for which the predicate returns
  -- true.
  deleteProperties :: (Property -> Bool) -> m ()

  -- TODO putProperties
  putProperties :: [Property] -> m ()

  -- | Mutates the game info for the current path, returning the new info.  If
  -- the current node or one of its ancestors has game info properties, then
  -- that node is modified.  Otherwise, properties are inserted on the root
  -- node.
  modifyGameInfo :: (GameInfo -> GameInfo) -> m GameInfo

  -- | Registers a new event handler for a given event type.
  on :: Event h handler -> handler -> m ()

-- | The regular monad transformer for 'MonadGo'.
data GoT h m a = GoT { goState :: State.StateT (GoState h) m a }

-- | A type synonym for a 'GoT' with no event handler type.
type BasicGoT m a = GoT Identity m a

-- | The regular monad for 'MonadGo'.
type GoM h a = GoT h Identity a

-- | A type synonym for 'GoM' with no event handler type.
type BasicGoM a = GoT Identity Identity a

instance (Monad h, Monad m) => Functor (GoT h m) where
  fmap = liftM

instance (Monad h, Monad m) => Applicative (GoT h m) where
  pure = return
  (<*>) = ap

instance (Monad h, Monad m) => Monad (GoT h m) where
  return x = GoT $ return x
  m >>= f = GoT $ goState . f =<< goState m
  fail = lift . fail

instance Monad h => MonadTrans (GoT h) where
  lift = GoT . lift

instance (Monad h, MonadWriter w m) => MonadWriter w (GoT h m) where
  writer = lift . writer
  tell = lift . tell
  listen = GoT . listen . goState
  pass = GoT . pass . goState

-- | Executes a Go monad transformer on a cursor, returning in the underlying
-- monad a tuple that contains the resulting value; the final cursor; and the
-- event handler action that is the result of manipulations in the Go monad.
runGoT :: (Monad h, Monad m) => GoT h m a -> Cursor -> m (a, Cursor, h ())
runGoT go cursor = do
  (value, state) <- State.runStateT (goState go) (initialState cursor)
  return (value,
          stateCursor state,
          do orM $ reverse $ stateHandlerActions state
             return ())

-- | An version of 'runGoT' that only works for Go monads that don't use event
-- handlers, and potentially saves a type declaration for the unused @h@
-- parameter.
runBasicGoT :: Monad m => BasicGoT m a -> Cursor -> m (a, Cursor)
runBasicGoT go cursor = do
  (value, cursor', _) <- runGoT go cursor
  return (value, cursor')

-- | Runs a Go monad on a cursor.  See 'runGoT'.
runGo :: Monad h => GoM h a -> Cursor -> (a, Cursor, h ())
runGo go = runIdentity . runGoT go

-- | Runs a Go monad without event handlers on a cursor.  See 'runBasicGoT'.
runBasicGo :: BasicGoM a -> Cursor -> (a, Cursor)
runBasicGo go cursor =
  let (value, cursor', _) = runGo go cursor
  in (value, cursor')

-- | Runs a Go monad on a cursor and returns the value in the monad.
evalGo :: Monad h => GoM h a -> Cursor -> a
evalGo m cursor = let (value, _, _) = runGo m cursor
                  in value

-- | Runs a Go monad on a cursor and returns the final cursor.
execGo :: Monad h => GoM h a -> Cursor -> Cursor
execGo m cursor = let (_, cursor', _) = runGo m cursor
                  in cursor'

getState :: Monad m => GoT h m (GoState h)
getState = GoT State.get

putState :: Monad m => GoState h -> GoT h m ()
putState = GoT . State.put

modifyState :: Monad m => (GoState h -> GoState h) -> GoT h m ()
modifyState = GoT . State.modify

instance (Monad h, Monad m) => MonadGo h (GoT h m) where
  getCursor = liftM stateCursor getState

  goUp = do
    state@(GoState { stateCursor = cursor
                   , statePathStack = pathStack
                   }) <- getState
    case cursorParent cursor of
      Nothing -> fail $ "Can't go up from a root cursor: " ++ show cursor
      Just parent -> putState state { stateCursor = parent
                                    , statePathStack = case pathStack of
                                      [] -> []
                                      path:paths -> (GoDown (cursorChildIndex cursor):path):paths
                                    , stateHandlerActions =
                                      map ($ GoUp) (stateNavigationHandlers state) ++
                                      stateHandlerActions state
                                    }

  goDown childIndex = modifyState $
    \state@(GoState { stateCursor = cursor
                    , statePathStack = pathStack
                    }) ->
    state { stateCursor = cursorChild cursor childIndex
          , statePathStack = case pathStack of
            [] -> []
            path:paths -> (GoUp:path):paths
          , stateHandlerActions =
            map ($ GoDown childIndex) (stateNavigationHandlers state) ++
            stateHandlerActions state
          }

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
      x:[] -> putState $ state { statePathStack = [] }
      [] -> fail "dropPosition: No position to drop from the stack."

  modifyProperties fn = modifyState $ \state ->
    state { stateCursor = cursorModifyNode (\node -> node { nodeProperties = fn $ nodeProperties node })
                                           (stateCursor state)
          }

  deleteProperties pred = modifyProperties $ filter $ not . pred

  -- TODO Implement putProperties.  But how to dedup?
  putProperties props = fail "putProperties not implemented."

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

  on event handler = modifyState $ addHandler event handler

-- | A type of event in the Go monad that can be handled by executing an action
-- in some other monad @h@.  @handler@ is the type of monad or monadic function
-- which will be used by Go actions that can trigger the event.
data Event h handler = Event { eventStateGetter :: GoState h -> [handler]
                             , eventStateSetter :: [handler] -> GoState h -> GoState h
                             }

addHandler :: Event h handler -> handler -> GoState h -> GoState h
addHandler event handler state =
  eventStateSetter event (handler:eventStateGetter event state) state

type NavigationHandler h = Step -> h Bool

-- | An event that is fired when a single step up or down in a game tree is
-- made.
navigationEvent :: Event h (NavigationHandler h)
navigationEvent = Event { eventStateGetter = stateNavigationHandlers
                        , eventStateSetter = \handlers state -> state { stateNavigationHandlers = handlers }
                        }

type PropertiesChangedHandler h = [Property] -> h Bool

-- | An event corresponding to a change to the properties list of the current
-- node.
propertiesChangedEvent :: Event h (PropertiesChangedHandler h)
propertiesChangedEvent = Event { eventStateGetter = statePropertiesChangedHandlers
                               , eventStateSetter = \handlers state -> state { statePropertiesChangedHandlers = handlers }
                               }
