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

-- | Data structures that wrap and provide a higher-level interface to the SGF
-- game tree, including a zipper that navigates the tree and provides the
-- current board state.
module Khumba.Goatee.Sgf.Board (
  defaultSize,
  RootInfo(..), GameInfo(..), emptyGameInfo, internalIsGameInfoNode,
  gameInfoToProperties,
  BoardState(..), boardWidth, boardHeight,
  CoordState(..), rootBoardState, mapBoardCoords,
  isValidMove, isCurrentValidMove,
  Cursor(..), rootCursor, cursorRoot, cursorChild, cursorChildren,
  cursorChildCount, cursorChildPlayingAt, cursorProperties,
  cursorModifyNode,
  colorToMove
  ) where

import Control.Monad (unless, when)
import Control.Monad.Writer (execWriter, tell)
import Data.List (find, intercalate, nub)
import Data.Maybe
import qualified Data.Set as Set
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types

-- TODO Stop using errors everywhere, they're not testable.

-- | The default size of the board.  The FF[4] SGF spec says that the default Go
-- board is 19x19 square.
defaultSize :: Int
defaultSize = 19

-- | Properties that are specified in the root nodes of game trees.
data RootInfo = RootInfo { rootInfoWidth :: Int
                         , rootInfoHeight :: Int
                         , rootInfoVariationMode :: VariationMode
                         } deriving (Eq, Show)

-- | Properties that are specified in game info nodes.
data GameInfo = GameInfo { gameInfoRootInfo :: RootInfo

                         , gameInfoBlackName :: Maybe String
                         , gameInfoBlackTeamName :: Maybe String
                         , gameInfoBlackRank :: Maybe String

                         , gameInfoWhiteName :: Maybe String
                         , gameInfoWhiteTeamName :: Maybe String
                         , gameInfoWhiteRank :: Maybe String

                         , gameInfoRuleset :: Maybe Ruleset
                         , gameInfoBasicTimeSeconds :: Maybe Rational
                         , gameInfoOvertime :: Maybe String
                         , gameInfoResult :: Maybe GameResult

                         , gameInfoGameName :: Maybe String
                         , gameInfoGameComment :: Maybe String
                         , gameInfoOpeningComment :: Maybe String

                         , gameInfoEvent :: Maybe String
                         , gameInfoRound :: Maybe String
                         , gameInfoPlace :: Maybe String
                         , gameInfoDatesPlayed :: Maybe String
                         , gameInfoSource :: Maybe String
                         , gameInfoCopyright :: Maybe String

                         , gameInfoAnnotatorName :: Maybe String
                         , gameInfoEntererName :: Maybe String
                         } deriving (Show)

-- | Builds a 'GameInfo' with the given 'RootInfo' and no extra data.
emptyGameInfo :: RootInfo -> GameInfo
emptyGameInfo rootInfo =
  GameInfo { gameInfoRootInfo = rootInfo

           , gameInfoBlackName = Nothing
           , gameInfoBlackTeamName = Nothing
           , gameInfoBlackRank = Nothing

           , gameInfoWhiteName = Nothing
           , gameInfoWhiteTeamName = Nothing
           , gameInfoWhiteRank = Nothing

           , gameInfoRuleset = Nothing
           , gameInfoBasicTimeSeconds = Nothing
           , gameInfoOvertime = Nothing
           , gameInfoResult = Nothing

           , gameInfoGameName = Nothing
           , gameInfoGameComment = Nothing
           , gameInfoOpeningComment = Nothing

           , gameInfoEvent = Nothing
           , gameInfoRound = Nothing
           , gameInfoPlace = Nothing
           , gameInfoDatesPlayed = Nothing
           , gameInfoSource = Nothing
           , gameInfoCopyright = Nothing

           , gameInfoAnnotatorName = Nothing
           , gameInfoEntererName = Nothing
           }

-- | Returns whether a node contains any game info properties.
internalIsGameInfoNode :: Node -> Bool
internalIsGameInfoNode = any ((GameInfoProperty ==) . propertyType) . nodeProperties

-- | Converts a 'GameInfo' into a list of 'Property's that can be used to
-- reconstruct the 'GameInfo'.
gameInfoToProperties :: GameInfo -> [Property]
gameInfoToProperties info = execWriter $ do
  copy (PB . toSimpleText) gameInfoBlackName
  copy (BT . toSimpleText) gameInfoBlackTeamName
  copy (BR . toSimpleText) gameInfoBlackRank

  copy (PW . toSimpleText) gameInfoWhiteName
  copy (WT . toSimpleText) gameInfoWhiteTeamName
  copy (WR . toSimpleText) gameInfoWhiteRank

  copy RU gameInfoRuleset
  copy TM gameInfoBasicTimeSeconds
  copy (OT . toSimpleText) gameInfoOvertime
  copy RE gameInfoResult

  copy (GN . toSimpleText) gameInfoGameName
  copy (GC . toSimpleText) gameInfoGameComment
  copy (ON . toSimpleText) gameInfoOpeningComment

  copy (EV . toSimpleText) gameInfoEvent
  copy (RO . toSimpleText) gameInfoRound
  copy (PC . toSimpleText) gameInfoPlace
  copy (DT . toSimpleText) gameInfoDatesPlayed
  copy (SO . toSimpleText) gameInfoSource
  copy (CP . toSimpleText) gameInfoCopyright

  copy (AN . toSimpleText) gameInfoAnnotatorName
  copy (US . toSimpleText) gameInfoEntererName
  where copy ctor accessor = whenMaybe (accessor info) $ \x -> tell [ctor x]

-- | An object that corresponds to a node in some game tree, and represents the
-- state of the game at that node, including board position, player turn and
-- captures, and also board annotations.
data BoardState = BoardState { boardCoordStates :: [[CoordState]]
                               -- ^ The state of individual points on the board.
                               -- Stored in row-major order.  Point @(x, y)@ can
                               -- be accessed via @!! y !! x@.
                             , boardArrows :: ArrowList
                             , boardLines :: LineList
                             , boardLabels :: LabelList
                             , boardMoveNumber :: Integer
                             , boardPlayerTurn :: Color
                             , boardBlackCaptures :: Int
                             , boardWhiteCaptures :: Int
                             , boardGameInfo :: GameInfo
                             }

instance Show BoardState where
  show board = concat $ execWriter $ do
    tell ["Board: (Move ", show (boardMoveNumber board),
          ", ", show (boardPlayerTurn board), "'s turn, B:",
          show (boardBlackCaptures board), ", W:",
          show (boardWhiteCaptures board), ")\n"]
    tell [intercalate "\n" $ flip map (boardCoordStates board) $
          \row -> unwords $ map show row]

    let arrows = boardArrows board
    let lines = boardLines board
    let labels = boardLabels board
    unless (null arrows) $ tell ["\nArrows: ", show arrows]
    unless (null lines) $ tell ["\nLines: ", show lines]
    unless (null labels) $ tell ["\nLabels: ", show labels]

-- | Returns the width of the board, in stones.
boardWidth :: BoardState -> Int
boardWidth = rootInfoWidth . gameInfoRootInfo . boardGameInfo

-- | Returns the height of the board, in stones.
boardHeight :: BoardState -> Int
boardHeight = rootInfoHeight . gameInfoRootInfo . boardGameInfo

-- | Used by 'BoardState' to represent the state of a single point on the board.
-- Records whether a stone is present, as well as annotations and visibility
-- properties.
data CoordState = CoordState { coordStar :: Bool
                               -- ^ Whether this point is a star point.
                             , coordStone :: Maybe Color
                             , coordMark :: Maybe Mark
                             , coordVisibility :: CoordVisibility
                             }

instance Show CoordState where
  show c = case coordVisibility c of
             CoordInvisible -> "--"
             _ -> let stoneChar = case coordStone c of
                                    Nothing -> if coordStar c then '*' else '\''
                                    Just Black -> 'X'
                                    Just White -> 'O'
                      markChar = case coordMark c of
                                   Nothing -> ' '
                                   Just MarkCircle -> 'o'
                                   Just MarkSquare -> 's'
                                   Just MarkTriangle -> 'v'
                                   Just MarkX -> 'x'
                                   Just MarkSelected -> '!'
                  in [stoneChar, markChar]

-- | Creates a 'BoardState' for an empty board of the given width and height.
emptyBoardState :: Int -> Int -> BoardState
emptyBoardState width height =
  BoardState { boardCoordStates = coords
             , boardArrows = []
             , boardLines = []
             , boardLabels = []
             , boardMoveNumber = 0
             , boardPlayerTurn = Black
             , boardBlackCaptures = 0
             , boardWhiteCaptures = 0
             , boardGameInfo = emptyGameInfo rootInfo
             }
  where rootInfo = RootInfo { rootInfoWidth = width
                            , rootInfoHeight = height
                            , rootInfoVariationMode = defaultVariationMode
                            }
        emptyCoord = CoordState { coordStar = False
                                , coordStone = Nothing
                                , coordMark = Nothing
                                , coordVisibility = CoordVisible
                                }
        starCoord = emptyCoord { coordStar = True }
        isStarPoint' = isStarPoint width height
        coords = map (\y -> map (\x -> if isStarPoint' x y then starCoord else emptyCoord)
                                [0..width-1])
                     [0..height-1]

rootBoardState :: Node -> BoardState
rootBoardState rootNode =
  foldr applyProperty
        (emptyBoardState width height)
        (nodeProperties rootNode)
  where SZ width height = fromMaybe (SZ defaultSize defaultSize) $
                          findProperty propertySZ rootNode

mapBoardCoords :: (Int -> Int -> CoordState -> a) -> BoardState -> [a]
mapBoardCoords fn board =
  concatMap applyRow $ zip [0..] $ boardCoordStates board
  where applyRow (y, row) = map (applyCell y) $ zip [0..] row
        applyCell y (x, cell) = fn x y cell

-- | Applies a function to the 'GameInfo' of a 'BoardState'.
updateBoardInfo :: (GameInfo -> GameInfo) -> BoardState -> BoardState
updateBoardInfo fn board = board { boardGameInfo = fn $ boardGameInfo board }

-- | Performs necessary updates to a 'BoardState' between nodes in the tree.
-- Clears marks.
boardChild :: BoardState -> BoardState
boardChild board =
  board { boardCoordStates = map (map clearMark) $ boardCoordStates board
        , boardArrows = []
        , boardLines = []
        , boardLabels = []
        }
  where clearMark coord = case coordMark coord of
          Nothing -> coord
          Just _ -> coord { coordMark = Nothing }

-- |> isStarPoint width height x y
--
-- Returns whether @(x, y)@ is a known star point on a board of the given width
-- and height.
isStarPoint :: Int -> Int -> Int -> Int -> Bool
isStarPoint width height
  | width == 9 && height == 9 = isStarPoint9
  | width == 13 && height == 13 = isStarPoint13
  | width == 19 && height == 19 = isStarPoint19
  | otherwise = const $ const False

isStarPoint' :: [Int] -> Int -> Int -> Bool
isStarPoint' ixs x y = x `elem` ixs && y `elem` ixs

isStarPoint9 = isStarPoint' [2, 4, 6]
isStarPoint13 = isStarPoint' [3, 6, 9]
isStarPoint19 = isStarPoint' [3, 9, 15]

-- | Applies a property to a 'BoardState'.  This function covers all properties
-- that modify 'BoardState's, including making moves, adding markup, and so on.
applyProperty :: Property -> BoardState -> BoardState

applyProperty (B maybeXy) board = updateBoardForMove Black $ case maybeXy of
  Nothing -> board  -- Pass.
  Just xy -> getApplyMoveResult board $
             applyMove playTheDarnMoveGoParams Black xy board
applyProperty KO board = board
applyProperty (MN moveNum) board = board { boardMoveNumber = moveNum }
applyProperty (W maybeXy) board = updateBoardForMove White $ case maybeXy of
  Nothing -> board  -- Pass.
  Just xy -> getApplyMoveResult board $
             applyMove playTheDarnMoveGoParams White xy board

applyProperty (AB coords) board =
  updateCoordStates' (\state -> state { coordStone = Just Black }) coords board
applyProperty (AW coords) board =
  updateCoordStates' (\state -> state { coordStone = Just White }) coords board
applyProperty (AE coords) board =
  updateCoordStates' (\state -> state { coordStone = Nothing }) coords board
applyProperty (PL color) board = board { boardPlayerTurn = color }

applyProperty (C {}) board = board
applyProperty (DM {}) board = board
applyProperty (GB {}) board = board
applyProperty (GW {}) board = board
applyProperty (HO {}) board = board
applyProperty (N {}) board = board
applyProperty (UC {}) board = board
applyProperty (V {}) board = board

applyProperty (BM {}) board = board
applyProperty (DO {}) board = board
applyProperty (IT {}) board = board
applyProperty (TE {}) board = board

applyProperty (AR arrows) board = board { boardArrows = arrows ++ boardArrows board }
applyProperty (CR coords) board =
  updateCoordStates' (\state -> state { coordMark = Just MarkCircle }) coords board
applyProperty (DD coords) board =
  updateCoordStates' (\state -> state { coordVisibility = CoordDimmed }) coords board
applyProperty (LB labels) board = board { boardLabels = labels ++ boardLabels board }
applyProperty (LN lines) board = board { boardLines = lines ++ boardLines board }
applyProperty (MA coords) board =
  updateCoordStates' (\state -> state { coordMark = Just MarkX }) coords board
applyProperty (SL coords) board =
  updateCoordStates' (\state -> state { coordMark = Just MarkSelected }) coords board
applyProperty (SQ coords) board =
  updateCoordStates' (\state -> state { coordMark = Just MarkSquare }) coords board
applyProperty (TR coords) board =
  updateCoordStates' (\state -> state { coordMark = Just MarkTriangle }) coords board

applyProperty (AP {}) board = board
applyProperty (CA {}) board = board
applyProperty (FF {}) board = board
applyProperty (GM {}) board = board
applyProperty (ST {}) board = board
applyProperty (SZ {}) board = board

applyProperty (AN str) board =
  updateBoardInfo (\info -> info { gameInfoAnnotatorName = Just $ fromSimpleText str })
                  board
applyProperty (BR str) board =
  updateBoardInfo (\info -> info { gameInfoBlackRank = Just $ fromSimpleText str })
                  board
applyProperty (BT str) board =
  updateBoardInfo (\info -> info { gameInfoBlackTeamName = Just $ fromSimpleText str })
                  board
applyProperty (CP str) board =
  updateBoardInfo (\info -> info { gameInfoCopyright = Just $ fromSimpleText str })
                  board
applyProperty (DT str) board =
  updateBoardInfo (\info -> info { gameInfoDatesPlayed = Just $ fromSimpleText str })
                  board
applyProperty (EV str) board =
  updateBoardInfo (\info -> info { gameInfoEvent = Just $ fromSimpleText str })
                  board
applyProperty (GC str) board =
  updateBoardInfo (\info -> info { gameInfoGameComment = Just $ fromSimpleText str })
                  board
applyProperty (GN str) board =
  updateBoardInfo (\info -> info { gameInfoGameName = Just $ fromSimpleText str })
                  board
applyProperty (ON str) board =
  updateBoardInfo (\info -> info { gameInfoOpeningComment = Just $ fromSimpleText str })
                  board
applyProperty (OT str) board =
  updateBoardInfo (\info -> info { gameInfoOvertime = Just $ fromSimpleText str })
                  board
applyProperty (PB str) board =
  updateBoardInfo (\info -> info { gameInfoBlackName = Just $ fromSimpleText str })
                  board
applyProperty (PC str) board =
  updateBoardInfo (\info -> info { gameInfoPlace = Just $ fromSimpleText str })
                  board
applyProperty (PW str) board =
  updateBoardInfo (\info -> info { gameInfoWhiteName = Just $ fromSimpleText str })
                  board
applyProperty (RE result) board =
  updateBoardInfo (\info -> info { gameInfoResult = Just result })
                  board
applyProperty (RO str) board =
  updateBoardInfo (\info -> info { gameInfoRound = Just $ fromSimpleText str })
                  board
applyProperty (RU ruleset) board =
  updateBoardInfo (\info -> info { gameInfoRuleset = Just ruleset })
                  board
applyProperty (SO str) board =
  updateBoardInfo (\info -> info { gameInfoSource = Just $ fromSimpleText str })
                  board
applyProperty (TM seconds) board =
  updateBoardInfo (\info -> info { gameInfoBasicTimeSeconds = Just seconds })
                  board
applyProperty (US str) board =
  updateBoardInfo (\info -> info { gameInfoEntererName = Just $ fromSimpleText str })
                  board
applyProperty (WR str) board =
  updateBoardInfo (\info -> info { gameInfoWhiteRank = Just $ fromSimpleText str })
                  board
applyProperty (WT str) board =
  updateBoardInfo (\info -> info { gameInfoWhiteTeamName = Just $ fromSimpleText str })
                  board

applyProperty (VW {}) board = board

applyProperty (UnknownProperty {}) board = board

applyProperties :: Node -> BoardState -> BoardState
applyProperties node board = foldr applyProperty board (nodeProperties node)

-- | Applies the transformation function to all of a board's coordinates
-- referred to by the 'CoordList'.
updateCoordStates :: (CoordState -> CoordState) -> [Coord] -> BoardState -> BoardState
updateCoordStates fn coords board =
  board { boardCoordStates = foldr applyFn (boardCoordStates board) coords }
  where applyFn (x, y) = listUpdate (updateRow x) y
        updateRow = listUpdate fn

updateCoordStates' :: (CoordState -> CoordState) -> CoordList -> BoardState -> BoardState
updateCoordStates' fn coords = updateCoordStates fn (expandCoordList coords)

-- | Extracts the 'CoordState' for a coordinate on a board.
getCoordState :: Coord -> BoardState -> CoordState
getCoordState (x, y) board = boardCoordStates board !! y !! x

-- | Updates properties of a 'BoardState' given that the player of the given
-- color has just made a move.  Increments the move number and updates the
-- player turn.
updateBoardForMove :: Color -> BoardState -> BoardState
updateBoardForMove movedPlayer board =
  board { boardMoveNumber = boardMoveNumber board + 1
        , boardPlayerTurn = cnot movedPlayer
        }

-- | A structure that configures how 'applyMove' should handle moves that are
-- normally illegal in Go.
data ApplyMoveParams = ApplyMoveParams { allowSuicide :: Bool
                                         -- ^ If false, suicide will cause 'applyMove' to return
                                         -- 'ApplyMoveSuicideError'.  If true, suicide will kill the
                                         -- friendly group and give points to the opponent.
                                       , allowOverwrite :: Bool
                                         -- ^ If false, playing on an occupied point will cause
                                         -- 'applyMove' to return 'ApplyMoveOverwriteError' with the
                                         -- color of the stone occupying the point.  If true,
                                         -- playing on an occupied point will overwrite the point
                                         -- (the previous stone vanishes), then capture rules are
                                         -- applied as normal.
                                       } deriving (Show)

-- | As an argument to 'applyMove', causes illegal moves to be treated as
-- errors.
standardGoMoveParams :: ApplyMoveParams
standardGoMoveParams = ApplyMoveParams { allowSuicide = False
                                       , allowOverwrite = False
                                       }

-- | As an argument to 'applyMove', causes illegal moves to be played
-- unconditionally.
playTheDarnMoveGoParams :: ApplyMoveParams
playTheDarnMoveGoParams = ApplyMoveParams { allowSuicide = True
                                          , allowOverwrite = True
                                          }

-- | The possible results from 'applyMove'.
data ApplyMoveResult = ApplyMoveOk BoardState
                       -- ^ The move was accepted; playing it resulted in the
                       -- given board without capture.
                     | ApplyMoveCapture BoardState Color Int
                       -- ^ The move was accepted; playing it resulted in the
                       -- given board with a capture.  The specified side gained
                       -- the number of points given.
                     | ApplyMoveSuicideError
                       -- ^ Playing the move would result in suicide, which is
                       -- forbidden.
                     | ApplyMoveOverwriteError Color
                       -- ^ There is already a stone of the specified color on
                       -- the target point, and overwriting is forbidden.

-- | If the 'ApplyMoveResult' represents a successful move, then the resulting
-- 'BoardState' is returned, otherwise, the default 'BoardState' given is
-- returned.
getApplyMoveResult :: BoardState -> ApplyMoveResult -> BoardState
getApplyMoveResult defaultBoard result = fromMaybe defaultBoard $ getApplyMoveResult' result

getApplyMoveResult' :: ApplyMoveResult -> Maybe BoardState
getApplyMoveResult' result = case result of
  ApplyMoveOk board -> Just board
  ApplyMoveCapture board color points -> Just $ case color of
    Black -> board { boardBlackCaptures = boardBlackCaptures board + points }
    White -> board { boardWhiteCaptures = boardWhiteCaptures board + points }
  ApplyMoveSuicideError -> Nothing
  ApplyMoveOverwriteError _ -> Nothing

-- | Internal data structure, only for move application code.  Represents a
-- group of stones.
data ApplyMoveGroup = ApplyMoveGroup { applyMoveGroupOrigin :: Coord
                                     , applyMoveGroupCoords :: [Coord]
                                     , applyMoveGroupLiberties :: Int
                                     } deriving (Show)

-- | Places a stone of a color at a point on a board, and runs move validation
-- and capturing logic according to the given parameters.  Returns whether the
-- move was successful, and the result if so.
applyMove :: ApplyMoveParams -> Color -> Coord -> BoardState -> ApplyMoveResult
applyMove params color xy board =
  let currentStone = coordStone $ getCoordState xy board
  in case currentStone of
    Just color -> if allowOverwrite params
                  then moveResult
                  else ApplyMoveOverwriteError color
    Nothing -> moveResult
  where boardWithMove = updateCoordStates (\state -> state { coordStone = Just color })
                                          [xy]
                                          board
        (boardWithCaptures, points) = foldr (maybeCapture $ cnot color)
                                            (boardWithMove, 0)
                                            (adjacentPoints boardWithMove xy)
        playedGroup = computeGroup boardWithCaptures xy
        moveResult
          | applyMoveGroupLiberties playedGroup == 0 =
            if points /= 0
            then error "Cannot commit suicide and capture at the same time."
            else if allowSuicide params
                 then let (boardWithSuicide, suicidePoints) =
                            applyMoveCapture (boardWithCaptures, 0) playedGroup
                      in ApplyMoveCapture boardWithSuicide (cnot color) suicidePoints
                 else ApplyMoveSuicideError
          | points /= 0 = ApplyMoveCapture boardWithCaptures color points
          | otherwise = ApplyMoveOk boardWithCaptures

-- | Capture if there is a liberty-less group of a color at a point on
-- a board.  Removes captures stones from the board and accumulates
-- points for captured stones.
maybeCapture :: Color -> Coord -> (BoardState, Int) -> (BoardState, Int)
maybeCapture color xy result@(board, _) =
  if coordStone (getCoordState xy board) /= Just color
  then result
  else let group = computeGroup board xy
       in if applyMoveGroupLiberties group /= 0
          then result
          else applyMoveCapture result group

computeGroup :: BoardState -> Coord -> ApplyMoveGroup
computeGroup board xy =
  if isNothing (coordStone $ getCoordState xy board)
  then error "computeGroup called on an empty point."
  else let groupCoords = bucketFill board xy
       in ApplyMoveGroup { applyMoveGroupOrigin = xy
                         , applyMoveGroupCoords = groupCoords
                         , applyMoveGroupLiberties = getLibertiesOfGroup board groupCoords
                         }

applyMoveCapture :: (BoardState, Int) -> ApplyMoveGroup -> (BoardState, Int)
applyMoveCapture (board, points) group =
  (updateCoordStates (\state -> state { coordStone = Nothing })
                     (applyMoveGroupCoords group)
                     board,
   points + length (applyMoveGroupCoords group))

-- | Returns a list of the four coordinates that are adjacent to the
-- given coordinate on the board, excluding coordinates that are out
-- of bounds.
adjacentPoints :: BoardState -> Coord -> [Coord]
adjacentPoints board (x, y) = execWriter $ do
  when (x > 0) $ tell [(x - 1, y)]
  when (y > 0) $ tell [(x, y - 1)]
  when (x < boardWidth board - 1) $ tell [(x + 1, y)]
  when (y < boardHeight board - 1) $ tell [(x, y + 1)]

-- | Takes a list of coordinates that comprise a group (e.g. a list
-- returned from 'bucketFill') and returns the number of liberties the
-- group has.  Does no error checking to ensure that the list refers
-- to a single or maximal group.
getLibertiesOfGroup :: BoardState -> [Coord] -> Int
getLibertiesOfGroup board groupCoords =
  length $ nub $ concatMap findLiberties groupCoords
  where findLiberties xy = filter (\xy' -> isNothing $ coordStone $ getCoordState xy' board)
                                  (adjacentPoints board xy)

-- | Expands a single coordinate on a board into a list of all the
-- coordinates connected to it by some continuous path of stones of
-- the same color (or empty spaces).
bucketFill :: BoardState -> Coord -> [Coord]
bucketFill board xy0 = bucketFill' Set.empty [xy0]
  where bucketFill' known [] = Set.toList known
        bucketFill' known (xy:xys) =
          if Set.member xy known
          then bucketFill' known xys
          else let new = filter ((stone0 ==) . coordStone . flip getCoordState board)
                                (adjacentPoints board xy)
               in bucketFill' (Set.insert xy known) (new ++ xys)
        stone0 = coordStone $ getCoordState xy0 board

-- | Returns whether it is legal to place a stone of the given color at a point
-- on a board.  Accepts out-of-bound coordinates and returns false.
isValidMove :: BoardState -> Color -> Coord -> Bool
-- TODO Should out-of-bound coordinates be accepted?
isValidMove board color coord@(x, y) =
  let w = boardWidth board
      h = boardHeight board
  in x >= 0 && y >= 0 && x < w && y < h &&
     isJust (getApplyMoveResult' $ applyMove standardGoMoveParams color coord board)

-- | Returns whether it is legal for the current player to place a stone at a
-- point on a board.  Accepts out-of-bound coordinates and returns false.
isCurrentValidMove :: BoardState -> Coord -> Bool
isCurrentValidMove board = isValidMove board (boardPlayerTurn board)

-- | A pointer to a node in a game tree that also holds information
-- about the current state of the game at that node.
data Cursor = Cursor { cursorParent :: Maybe Cursor
                       -- ^ The cursor for the node above this cursor's node in
                       -- the game tree.  The node of the parent cursor is the
                       -- parent of the cursor's node.
                       --
                       -- This is @Nothing@ iff the cursor's node has no parent.
                     , cursorChildIndex :: Int
                       -- ^ The index of this cursor's node in its parent's
                       -- child list.  When the cursor's node has no parent,
                       -- the value in this field is not specified.
                     , cursorNode :: Node
                       -- ^ The game tree node about which the cursor stores
                       -- information.
                     , cursorBoard :: BoardState
                       -- ^ The complete board state for the current node.
                     } deriving (Show) -- TODO Better Show Cursor instance.

-- | Returns a cursor for a root node.
rootCursor :: Node -> Cursor
rootCursor node =
  Cursor { cursorParent = Nothing
         , cursorChildIndex = -1
         , cursorNode = node
         , cursorBoard = rootBoardState node
         }

cursorRoot :: Cursor -> Cursor
cursorRoot cursor = case cursorParent cursor of
  Nothing -> cursor
  Just parent -> cursorRoot parent

cursorChild :: Cursor -> Int -> Cursor
cursorChild cursor index =
  Cursor { cursorParent = Just cursor
         , cursorChildIndex = index
         , cursorNode = child
         , cursorBoard = applyProperties child $ boardChild $ cursorBoard cursor
         }
  -- TODO Better handling or messaging for out-of-bounds:
  where child = (!! index) $ nodeChildren $ cursorNode cursor

cursorChildren :: Cursor -> [Cursor]
cursorChildren cursor =
  let board = boardChild $ cursorBoard cursor
  in map (\(index, child) -> Cursor { cursorParent = Just cursor
                                    , cursorChildIndex = index
                                    , cursorNode = child
                                    , cursorBoard = applyProperties child board
                                    })
     $ zip [0..]
     $ nodeChildren
     $ cursorNode cursor

cursorChildCount :: Cursor -> Int
cursorChildCount = length . nodeChildren . cursorNode

cursorChildPlayingAt :: Coord -> Cursor -> Maybe Cursor
cursorChildPlayingAt coord cursor =
  let children = cursorChildren cursor
      color = boardPlayerTurn $ cursorBoard cursor
      hasMove = elem $ colorToMove color coord
  in find (hasMove . nodeProperties . cursorNode) children

-- | This is simply @'nodeProperties' . 'cursorNode'@.
cursorProperties :: Cursor -> [Property]
cursorProperties = nodeProperties . cursorNode

cursorModifyNode :: (Node -> Node) -> Cursor -> Cursor
cursorModifyNode fn cursor =
  let node' = fn $ cursorNode cursor
  in case cursorParent cursor of
    Nothing -> rootCursor node'
    Just parentCursor ->
      let index = cursorChildIndex cursor
          parentCursor' = cursorModifyNode
                          (\parentNode ->
                            parentNode { nodeChildren = listUpdate (const node')
                                                        index
                                                        (nodeChildren parentNode)
                                       })
                          parentCursor
      in cursorChild parentCursor' index

colorToMove :: Color -> Coord -> Property
colorToMove color coord =
  case color of
    Black -> B $ Just coord
    White -> W $ Just coord
