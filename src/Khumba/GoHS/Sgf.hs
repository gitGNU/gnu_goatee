module Khumba.GoHS.Sgf where

import qualified Control.Monad.State as State
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.DeepSeq
import Control.Monad (forM_, liftM, sequence_, unless, when)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, sort, sortBy)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid
import Khumba.GoHS.Common

-- | A coordinate on a Go board.  @(0, 0)@ refers to the upper-left corner of
-- the board.  The first component is the horizontal position; the second
-- component is the vertical position.
type Coord = (Int, Int)

-- | Allows for compact representation of a list of coordinates.  Convert to a
-- list with 'expandCoordList'.
data CoordList = CoordList { coordListSingles :: [Coord]
                           , coordListRects :: [(Coord, Coord)]
                           } deriving (Show)

instance Eq CoordList where
  (==) = (==) `on` sort . expandCoordList

instance Monoid CoordList where
  mempty = CoordList { coordListSingles = []
                     , coordListRects = []
                     }

  mappend x y = CoordList { coordListSingles = coordListSingles x ++ coordListSingles y
                          , coordListRects = coordListRects x ++ coordListRects y
                          }

instance NFData CoordList where
  rnf coords = rnf (coordListSingles coords) `seq`
               rnf (coordListRects coords)

-- | Converts a compact 'CoordList' to a list of coordinates.
expandCoordList :: CoordList -> [Coord]
expandCoordList cl = coordListSingles cl ++
                     foldr (\r@((x0, y0), (x1, y1)) rest ->
                             if x0 > x1 || y0 > y1
                             then error ("Invalid coord. rectangle: " ++ show r)
                             else [(x, y) | x <- [x0..x1], y <- [y0..y1]] ++ rest)
                           []
                           (coordListRects cl)

-- | An SGF collection of game trees.
data Collection = Collection { collectionTrees :: [Node]
                             } deriving (Show)

-- | An SGF game tree node.  Unlike in the SGF spec, we represent a game tree
-- with nodes uniformly, rather than having the separation between sequences and
-- nodes.
data Node = Node { nodeProperties :: [Property]
                 , nodeChildren :: [Node]
                 } deriving (Show)

instance NFData Node where
  rnf node = rnf (nodeProperties node) `seq`
             rnf (nodeChildren node)

-- | A node with no properties and no children.
emptyNode :: Node
emptyNode = Node { nodeProperties = [], nodeChildren = [] }

rootNode :: Int -- ^ Board width
         -> Int -- ^ Board height
         -> Node
rootNode width height =
  Node { nodeProperties = [SZ width height]
       , nodeChildren = []
       }

findProperty :: Node -> (Property -> Bool) -> Maybe Property
findProperty node pred = find pred $ nodeProperties node

-- | Appends a property to a node's property list.
addProperty :: Property -> Node -> Node
addProperty prop node = node { nodeProperties = nodeProperties node ++ [prop] }

-- | Appends a child node to a node's child list.
addChild :: Node -> Node -> Node
addChild child node = node { nodeChildren = nodeChildren node ++ [child] }

-- | Returns a list of validation errors for the current node, an
-- empty list if no errors are detected.
validateNode :: Bool -> Bool -> Node -> [String]
validateNode isRoot seenGameNode node = execWriter $ do
  let props = nodeProperties node
  let propTypes = nub $ map propertyType $ nodeProperties node

  -- Check for move and setup properties in a single node.
  when (MoveProperty `elem` propTypes && SetupProperty `elem` propTypes) $
    tell ["Node contains move and setup properties."]

  -- Check for root properties in non-root nodes.
  let rootProps = filter ((RootProperty ==) . propertyType) props
  when (not isRoot && not (null rootProps)) $
    tell $ map (\p -> "Root property found on non-root node: " ++
                      show p ++ ".")
           rootProps

  -- TODO Check for game-info properties.

  -- Check for coordinates marked multiple times.
  validateNodeDuplicates props getMarkedCoords $ \group ->
    tell ["Coordinate " ++ show (fst $ head group) ++
          " is specified multiple times in properties " ++
          intercalate ", " (map snd group) ++ "."]

  -- TODO Validate recursively.

  where getMarkedCoords (CR cs) = tagMarkedCoords cs "CR"
        getMarkedCoords (MA cs) = tagMarkedCoords cs "MA"
        getMarkedCoords (SL cs) = tagMarkedCoords cs "SL"
        getMarkedCoords (SQ cs) = tagMarkedCoords cs "SQ"
        getMarkedCoords (TR cs) = tagMarkedCoords cs "TR"
        getMarkedCoords _ = []

        tagMarkedCoords cs tag = map (\x -> (x, tag)) $ expandCoordList cs

validateNodeDuplicates :: (Eq v, Ord v)
                          => [Property]
                          -> (Property -> [(v, t)])
                          -> ([(v, t)] -> Writer [String] ())
                          -> Writer [String] ()
validateNodeDuplicates props getTaggedElts errAction =
  let groups = groupBy ((==) `on` fst) $
               sortBy (compare `on` fst) $
               concatMap getTaggedElts props
  in forM_ groups $ \group ->
       unless (null $ tail group) $
         errAction group

-- | An SGF property that gives a node meaning.
data Property =
  -- Move properties.
    B (Maybe Coord)      -- ^ Black move (nothing iff pass).
  | KO                   -- ^ Execute move unconditionally (even if illegal).
  | MN Integer           -- ^ Assign move number.
  | W (Maybe Coord)      -- ^ White move (nothing iff pass).

  -- Setup properties.
  | AB CoordList         -- ^ Assign black stones.
  | AE CoordList         -- ^ Assign empty stones.
  | AW CoordList         -- ^ Assign white stones.
  | PL Color             -- ^ Player to play.

  -- Node annotation properties.
  | C String             -- ^ Comment.
  | DM DoubleValue       -- ^ Even position.
  | GB DoubleValue       -- ^ Good for black.
  | GW DoubleValue       -- ^ Good for white.
  | HO DoubleValue       -- ^ Hotspot.
  | N SimpleText         -- ^ Node name.
  | UC DoubleValue       -- ^ Unclear position.
  | V RealValue          -- ^ Node value.

  -- Move annotation properties.
  | BM DoubleValue       -- ^ Bad move.
  | DO                   -- ^ Doubtful move.
  | IT                   -- ^ Interesting move.
  | TE DoubleValue       -- ^ Tesuji.

  -- Markup properties.
  | AR ArrowList         -- ^ Arrows.
  | CR CoordList         -- ^ Mark points with circles.
  | DD CoordList         -- ^ Dim points.
  | LB LabelList         -- ^ Label points with text.
  | LN LineList          -- ^ Lines.
  | MA CoordList         -- ^ Mark points with 'X's.
  | SL CoordList         -- ^ Mark points as selected.
  | SQ CoordList         -- ^ Mark points with squares.
  | TR CoordList         -- ^ Mark points with trianges.

  -- Root properties.
  | AP SimpleText SimpleText -- ^ Application info.
  | CA SimpleText        -- ^ Copyright info.
  | FF Int               -- ^ File format version.
  | GM Int               -- ^ Game (must be 1 = Go).
  | ST Int               -- ^ Variation display format.
  | SZ Int Int           -- ^ Board size.

  -- Game info properties.
  | AN SimpleText        -- ^ Name of annotator.
  | BR SimpleText        -- ^ Rank of black player.
  | BT SimpleText        -- ^ Name of black team.
  | CP SimpleText        -- ^ Copyright info.
  | DT SimpleText        -- ^ Dates played.
  | EV SimpleText        -- ^ Event name.
  | GC SimpleText        -- ^ Game comment/background/summary.
  | GN SimpleText        -- ^ Game name.
  | ON SimpleText        -- ^ Information about the opening.
  | OT SimpleText        -- ^ The method used for overtime.
  | PB SimpleText        -- ^ Name of black player.
  | PC SimpleText        -- ^ Where the game was played.
  | PW SimpleText        -- ^ Name of white player.
  | RE GameResult        -- ^ Result of the game.
  | RO SimpleText        -- ^ Round info.
  | RU Ruleset           -- ^ Ruleset used.
  | SO SimpleText        -- ^ Source of the game.
  | TM RealValue         -- ^ Time limit, in seconds.
  | US SimpleText        -- ^ Name of user or program who entered the game.
  | WR SimpleText        -- ^ Rank of white player.
  | WT SimpleText        -- ^ Name of white team.

  | UnknownProperty String String

  -- TODO Game info, timing, and miscellaneous properties.
  -- Also in functions below.
  deriving (Eq, Show)

instance NFData Property where
  rnf (B x) = rnf x
  rnf KO = ()
  rnf (MN x) = rnf x
  rnf (W x) = rnf x

  rnf (AB x) = rnf x
  rnf (AE x) = rnf x
  rnf (AW x) = rnf x
  rnf (PL x) = rnf x

  rnf (C x) = rnf x
  rnf (DM x) = rnf x
  rnf (GB x) = rnf x
  rnf (GW x) = rnf x
  rnf (HO x) = rnf x
  rnf (N x) = rnf x
  rnf (UC x) = rnf x
  rnf (V x) = rnf x

  rnf (BM x) = rnf x
  rnf DO = ()
  rnf IT = ()
  rnf (TE x) = rnf x

  rnf (AR x) = rnf x
  rnf (CR x) = rnf x
  rnf (DD x) = rnf x
  rnf (LB x) = rnf x
  rnf (LN x) = rnf x
  rnf (MA x) = rnf x
  rnf (SL x) = rnf x
  rnf (SQ x) = rnf x
  rnf (TR x) = rnf x

  rnf (AP x y) = rnf x `seq` rnf y
  rnf (CA x) = rnf x
  rnf (FF x) = rnf x
  rnf (GM x) = rnf x
  rnf (ST x) = rnf x
  rnf (SZ x y) = rnf x `seq` rnf y

  rnf (AN x) = rnf x
  rnf (BR x) = rnf x
  rnf (BT x) = rnf x
  rnf (CP x) = rnf x
  rnf (DT x) = rnf x
  rnf (EV x) = rnf x
  rnf (GC x) = rnf x
  rnf (GN x) = rnf x
  rnf (ON x) = rnf x
  rnf (OT x) = rnf x
  rnf (PB x) = rnf x
  rnf (PC x) = rnf x
  rnf (PW x) = rnf x
  rnf (RE x) = rnf x
  rnf (RO x) = rnf x
  rnf (RU x) = rnf x
  rnf (SO x) = rnf x
  rnf (TM x) = rnf x
  rnf (US x) = rnf x
  rnf (WR x) = rnf x
  rnf (WT x) = rnf x

  rnf (UnknownProperty x y) = rnf x `seq` rnf y

-- | An SGF real value.
type RealValue = Rational

-- | An SGF simple text value.
data SimpleText = SimpleText String
                deriving (Eq, Show)

instance NFData SimpleText where
  rnf (SimpleText str) = rnf str

sanitizeSimpleText :: String -> String
sanitizeSimpleText = listReplace '\n' ' '

toSimpleText :: String -> SimpleText
toSimpleText = SimpleText . sanitizeSimpleText

fromSimpleText :: SimpleText -> String
fromSimpleText (SimpleText str) = sanitizeSimpleText str

-- | An SGF double value: either 1 or 2, nothing else.
data DoubleValue = Double1
                 | Double2
                 deriving (Eq, Show)

instance NFData DoubleValue

-- | Stone color: black or white.
data Color = Black
           | White
           deriving (Eq, Show)

instance NFData Color

-- | Returns the logical negation of a stone color, yang for yin and
-- yin for yang.
cnot :: Color -> Color
cnot Black = White
cnot White = Black

colorToMove :: Color -> Coord -> Property
colorToMove color coord =
  case color of
    Black -> B $ Just coord
    White -> W $ Just coord

-- | A list of arrows, each specified as @(startCoord, endCoord)@.
type ArrowList = [(Coord, Coord)]

-- | A list of lines, each specified as @(startCoord, endCoord)@.
type LineList = [(Coord, Coord)]

-- | A list of labels, each specified with a string and a coordinate about which
-- to center the string.
type LabelList = [(Coord, String)]

-- | The markings that SGF supports annotating coordinates with.
data Mark = MarkCircle | MarkSquare | MarkTriangle | MarkX | MarkSelected

instance NFData Mark

-- | The visibility states that SGF allows a coordinate to be in.
data CoordVisibility = CoordVisible | CoordDimmed | CoordInvisible

instance NFData CoordVisibility

data GameResult = GameResultWin Color WinReason
                | GameResultDraw
                | GameResultVoid
                | GameResultUnknown
                | GameResultOther String
                deriving (Eq, Show)

instance NFData GameResult where
  rnf (GameResultWin color reason) = rnf color `seq` rnf reason
  rnf (GameResultOther str) = rnf str
  rnf _ = ()

data WinReason = WinByScore Rational
               | WinByResignation
               | WinByTime
               | WinByForfeit
               deriving (Eq, Show)

instance NFData WinReason where
  rnf (WinByScore points) = rnf points
  rnf _ = ()

data Ruleset = RulesetAga
             | RulesetIng
             | RulesetJapanese
             | RulesetNewZealand
             | RulesetOther String
             deriving (Eq, Show)

instance NFData Ruleset where
  rnf (RulesetOther str) = rnf str
  rnf _ = ()

-- | The property types that SGF uses to group properties.
data PropertyType = MoveProperty     -- ^ Cannot mix with setup nodes.
                  | SetupProperty    -- ^ Cannot mix with move nodes.
                  | RootProperty     -- ^ May only appear in root nodes.
                  | GameInfoProperty -- ^ At most one on any path.
                  | GeneralProperty  -- ^ May appear anywhere in the game tree.
                  deriving (Eq, Show)

-- | Returns a property's type, as specified by the SGF spec.
propertyType :: Property -> PropertyType

propertyType (B _) = MoveProperty
propertyType KO = MoveProperty
propertyType (MN _) = MoveProperty
propertyType (W _) = MoveProperty

propertyType (AB _) = SetupProperty
propertyType (AE _) = SetupProperty
propertyType (AW _) = SetupProperty
propertyType (PL _) = SetupProperty

propertyType (C _) = GeneralProperty
propertyType (DM _) = GeneralProperty
propertyType (GB _) = GeneralProperty
propertyType (GW _) = GeneralProperty
propertyType (HO _) = GeneralProperty
propertyType (N _) = GeneralProperty
propertyType (UC _) = GeneralProperty
propertyType (V _) = GeneralProperty

propertyType (BM _) = MoveProperty
propertyType DO = MoveProperty
propertyType IT = MoveProperty
propertyType (TE _) = MoveProperty

propertyType (AR _) = GeneralProperty
propertyType (CR _) = GeneralProperty
propertyType (DD _) = GeneralProperty
propertyType (LB _) = GeneralProperty
propertyType (LN _) = GeneralProperty
propertyType (MA _) = GeneralProperty
propertyType (SL _) = GeneralProperty
propertyType (SQ _) = GeneralProperty
propertyType (TR _) = GeneralProperty

propertyType (AP _ _) = RootProperty
propertyType (CA _) = RootProperty
propertyType (FF _) = RootProperty
propertyType (GM _) = RootProperty
propertyType (ST _) = RootProperty
propertyType (SZ _ _) = RootProperty

propertyType (AN _) = GameInfoProperty
propertyType (BR _) = GameInfoProperty
propertyType (BT _) = GameInfoProperty
propertyType (CP _) = GameInfoProperty
propertyType (DT _) = GameInfoProperty
propertyType (EV _) = GameInfoProperty
propertyType (GC _) = GameInfoProperty
propertyType (GN _) = GameInfoProperty
propertyType (ON _) = GameInfoProperty
propertyType (OT _) = GameInfoProperty
propertyType (PB _) = GameInfoProperty
propertyType (PC _) = GameInfoProperty
propertyType (PW _) = GameInfoProperty
propertyType (RE _) = GameInfoProperty
propertyType (RO _) = GameInfoProperty
propertyType (RU _) = GameInfoProperty
propertyType (SO _) = GameInfoProperty
propertyType (TM _) = GameInfoProperty
propertyType (US _) = GameInfoProperty
propertyType (WR _) = GameInfoProperty
propertyType (WT _) = GameInfoProperty

-- TODO Is this correct?
propertyType (UnknownProperty _ _) = GeneralProperty

-- | Returns whether the value of the given property is inherited from the
-- lowest ancestor specifying the property, when the property is not set on a
-- node itself.
propertyInherited :: Property -> Bool
propertyInherited (DD _) = True
propertyInherited _ = False

data GameInfo = GameInfo { gameInfoBlackName :: Maybe String
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

instance NFData GameInfo where
  rnf info = rnf (gameInfoBlackName info) `seq`
             rnf (gameInfoBlackTeamName info) `seq`
             rnf (gameInfoBlackRank info) `seq`

             rnf (gameInfoWhiteName info) `seq`
             rnf (gameInfoWhiteTeamName info) `seq`
             rnf (gameInfoWhiteRank info) `seq`

             rnf (gameInfoRuleset info) `seq`
             rnf (gameInfoBasicTimeSeconds info) `seq`
             rnf (gameInfoOvertime info) `seq`
             rnf (gameInfoResult info) `seq`

             rnf (gameInfoGameName info) `seq`
             rnf (gameInfoGameComment info) `seq`
             rnf (gameInfoOpeningComment info) `seq`

             rnf (gameInfoEvent info) `seq`
             rnf (gameInfoRound info) `seq`
             rnf (gameInfoPlace info) `seq`
             rnf (gameInfoDatesPlayed info) `seq`
             rnf (gameInfoSource info) `seq`
             rnf (gameInfoCopyright info) `seq`

             rnf (gameInfoAnnotatorName info) `seq`
             rnf (gameInfoEntererName info)

emptyGameInfo :: GameInfo
emptyGameInfo = GameInfo { gameInfoBlackName = Nothing
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
                             , boardWidth :: Int
                             , boardHeight :: Int
                             , boardGameInfo :: GameInfo
                             }

instance NFData BoardState where
  rnf board = rnf (boardCoordStates board) `seq`
              rnf (boardArrows board) `seq`
              rnf (boardLines board) `seq`
              rnf (boardLabels board) `seq`
              rnf (boardMoveNumber board) `seq`
              rnf (boardPlayerTurn board) `seq`
              rnf (boardBlackCaptures board) `seq`
              rnf (boardWhiteCaptures board) `seq`
              rnf (boardWidth board) `seq`
              rnf (boardHeight board) `seq`
              rnf (boardGameInfo board)

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

-- | Used by 'BoardState' to represent the state of a single point on the board.
-- Records whether a stone is present, as well as annotations and visibility
-- properties.
data CoordState = CoordState { coordStar :: Bool
                               -- ^ Whether this point is a star point.
                             , coordStone :: Maybe Color
                             , coordMark :: Maybe Mark
                             , coordVisibility :: CoordVisibility
                             }

instance NFData CoordState where
  rnf c = rnf (coordStar c) `seq`
          rnf (coordStone c) `seq`
          rnf (coordMark c) `seq`
          rnf (coordVisibility c)

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
             , boardMoveNumber = 1
             , boardPlayerTurn = Black
             , boardBlackCaptures = 0
             , boardWhiteCaptures = 0
             , boardWidth = width
             , boardHeight = height
             , boardGameInfo = emptyGameInfo
             }
  where emptyCoord = CoordState { coordStar = False
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
  where SZ width height = fromMaybe (error $ "rootBoardState given a non-root node: " ++ show rootNode) $
                          findProperty rootNode $
                          \prop -> case prop of
                            (SZ _ _) -> True
                            _ -> False

mapBoardCoords :: (Int -> Int -> CoordState -> a) -> BoardState -> [a]
mapBoardCoords fn board =
  concatMap applyRow $ zip [0..] $ boardCoordStates board
  where applyRow (y, row) = map (applyCell y) $ zip [0..] row
        applyCell y (x, cell) = fn x y cell

-- | Applies a function to the 'GameInfo' of a 'BoardState'.
updateBoardInfo :: (GameInfo -> GameInfo) -> BoardState -> BoardState
updateBoardInfo fn board = board { boardGameInfo = fn $ boardGameInfo board }

advanceMove :: BoardState -> BoardState
advanceMove board = board { boardMoveNumber = boardMoveNumber board + 1
                          , boardPlayerTurn = cnot $ boardPlayerTurn board
                          }

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

applyProperty (B maybeXy) board = case maybeXy of
  Nothing -> board  -- Pass.
  Just xy -> getApplyMoveResult board $
             applyMove playTheDarnMoveGoParams Black xy board
applyProperty KO board = board
applyProperty (MN moveNum) board = board { boardMoveNumber = moveNum }
applyProperty (W maybeXy) board = case maybeXy of
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

applyProperty (C _) board = board
applyProperty (DM _) board = board
applyProperty (GB _) board = board
applyProperty (GW _) board = board
applyProperty (HO _) board = board
applyProperty (N _) board = board
applyProperty (UC _) board = board
applyProperty (V _) board = board

applyProperty (BM _) board = board
applyProperty DO board = board
applyProperty IT board = board
applyProperty (TE _) board = board

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

applyProperty (AP _ _) board = board
applyProperty (CA _) board = board
applyProperty (FF _) board = board
applyProperty (GM _) board = board
applyProperty (ST _) board = board
applyProperty (SZ _ _) board = board

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

applyProperty (UnknownProperty _ _) board = board

applyProperties :: Node -> BoardState -> BoardState
applyProperties node board = foldr applyProperty board (nodeProperties node)

-- | Applies the transformation function to all of a board's coordinates
-- referred to by the 'CoordList'.
updateCoordStates :: (CoordState -> CoordState) -> [Coord] -> BoardState -> BoardState
updateCoordStates fn coords board = board { boardCoordStates = foldr applyFn (boardCoordStates board) coords }
  where applyFn (x, y) = listUpdate (updateRow x) y
        updateRow = listUpdate fn

updateCoordStates' :: (CoordState -> CoordState) -> CoordList -> BoardState -> BoardState
updateCoordStates' fn coords = updateCoordStates fn (expandCoordList coords)

-- | Extracts the 'CoordState' for a coordinate on a board.
getCoordState :: Coord -> BoardState -> CoordState
getCoordState (x, y) board = boardCoordStates board !! y !! x

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
maybeCapture color xy result@(board, points) =
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
        bucketFill' known (xy:xys) = if Set.member xy known
                                     then bucketFill' known xys
                                     else let new = filter ((stone0 ==) . coordStone . flip getCoordState board)
                                                           (adjacentPoints board xy)
                                          in bucketFill' (Set.insert xy known) (new ++ xys)
        stone0 = coordStone $ getCoordState xy0 board

-- | Returns whether it is legal to place a stone of the given color at a point
-- on a board.
isValidMove :: BoardState -> Color -> Coord -> Bool
isValidMove board color coord@(x, y) =
  let w = boardWidth board
      h = boardHeight board
  in x >= 0 && y >= 0 && x < w && y < h &&
     case applyMove standardGoMoveParams color coord board of
       ApplyMoveOk {} -> True
       ApplyMoveCapture {} -> True
       ApplyMoveSuicideError {} -> False
       ApplyMoveOverwriteError {} -> False

-- | Plays a stone on a board at a point, applying capture rules.
play :: Color -> Coord -> BoardState -> BoardState
play color xy = applyProperty prop
  where prop = (if color == Black then B else W) (Just xy)

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

instance NFData Cursor where
  rnf cursor = rnf (cursorParent cursor) `seq`
               rnf (cursorChildIndex cursor) `seq`
               rnf (cursorNode cursor) `seq`
               rnf (cursorBoard cursor)

-- | Returns either a cursor for a root node, or an error message if the node
-- lacks the information needed to create a cursor (e.g. if the node is not a
-- root node).
rootCursor :: Node -> Either String Cursor
rootCursor node = do
  SZ width height <- maybe (Left "No board size property in root node.") Right $
                     findProperty node $
                     \prop -> case prop of
                       (SZ _ _) -> True
                       _ -> False
  return Cursor { cursorParent = Nothing
                , cursorChildIndex = -1
                , cursorNode = node
                , cursorBoard = applyProperties node $
                                emptyBoardState width height
                }

cursorChild :: Cursor -> Int -> Cursor
cursorChild cursor index =
  Cursor { cursorParent = Just cursor
         , cursorChildIndex = index
         , cursorNode = child
         , cursorBoard = applyProperties child $
                         advanceMove $
                         cursorBoard cursor
         }
  where child = (!! index) $ nodeChildren $ cursorNode cursor

cursorChildren :: Cursor -> [Cursor]
cursorChildren cursor =
  let board = advanceMove $ cursorBoard cursor
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

cursorModifyNode :: (Node -> Node) -> Cursor -> Cursor
cursorModifyNode fn cursor =
  let node' = fn $ cursorNode cursor
  in case cursorParent cursor of
    Nothing -> either (\msg -> error $ "Failed to reconstruct root node: " ++ msg)
                      id
                      (rootCursor node')
    Just parentCursor ->
      let index = cursorChildIndex cursor
          parentCursor' = cursorModifyNode (\parentNode ->
                                             parentNode { nodeChildren = listUpdate (const node')
                                                                                    index
                                                                                    (nodeChildren parentNode)
                                                        })
                                           parentCursor
      in cursorChild parentCursor' index

-- | A state that is transformed inside of a Go monad, 'GoM'.
data GoState h = GoState { stateCursor :: Cursor
                         , stateHandlers :: [ChangeHandler h]
                         , stateHandlerAction :: h ()
                         }

--zz: Old
--emptyGoState :: Monad h => GoState h
--emptyGoState = GoState { stateCursor = rootCursor emptyNode
--                       , stateHandlers = []
--                       , stateHandlerAction = return ()
--                       }

-- | A monad for executing Go actions.
--
-- @h@ must be a monad in which event handlers will execute.
data GoM h a = GoM { goState :: State.State (GoState h) a }

instance Monad h => Monad (GoM h) where
  return x = GoM { goState = return x }

  -- m :: GoM a
  -- f :: a -> GoM b
  --m >>= f = GoM $ let s = runGo m    -- s :: State.State GoState a
  --                in s >>= (\x -> runGo (f x))
  m >>= f = GoM $ goState . f =<< goState m
  --m >>= f = let s1 = goState m
  --              h1 = goHandlers m
  --              --go2 = s1 >>= f
  --              --s2 = goState . f =<< s1
  --              s2 = goState go2
  --              h2 = goHandlers go2
  --          in GoM { goState = s2, goHandlers = h1 ++ h2 }

-- h must be a monad.
type ChangeHandler h = ChangeEvent -> Maybe (h ())

data ChangeEvent = MoveEvent Cursor Cursor
                 | NodeAddedEvent Cursor

updateState :: (GoState h -> GoState h) -> GoM h ()
updateState f = GoM $ State.put . f =<< State.get

getCursor :: GoM h Cursor
getCursor = GoM { goState = liftM stateCursor State.get }

putCursor :: Monad h => Cursor -> GoM h ()
--putCursor cursor = GoM { goState = State.put $ GoState cursor }
putCursor cursor = do
  oldCursor <- getCursor
  updateState (\goState -> goState { stateCursor = cursor })
  fireEvent (MoveEvent oldCursor cursor)

-- Weird that "Monad h =>" is needed here?
getNode :: Monad h => GoM h Node
getNode = liftM cursorNode getCursor

putRoot :: Monad h => Node -> GoM h (Maybe String)
putRoot node =
  case rootCursor node of
    Left errorMessage -> return $ Just errorMessage
    Right cursor -> putCursor cursor >> return Nothing

getHandlers :: GoM h [ChangeHandler h]
getHandlers = GoM $ liftM stateHandlers State.get

addHandler :: ChangeHandler h -> GoM h ()
addHandler h = updateState
  (\goState -> goState { stateHandlers = stateHandlers goState ++ [h] })

getHandlerAction :: Monad h => GoM h (h ())
getHandlerAction = GoM $ liftM stateHandlerAction State.get

fireEvent :: Monad h => ChangeEvent -> GoM h ()
fireEvent e = do
  handlers <- getHandlers
  action <- getHandlerAction
  let newActions = mapMaybe ($ e) handlers
  let action' = action >> sequence_ newActions
  updateState (\goState -> goState { stateHandlerAction = action' })

---- Fails if already at the root.
--goUp :: Monad h => GoM h ()
--goUp = do
--  cursor <- getCursor
--  case cursorParent cursor of
--    Just parent -> putCursor parent
--    Nothing -> fail ("Could not go up from cursor: " ++ show cursor)

--zz: Disabled, emptyGoState is broken.
---- Executes the actions in the Go monad, returning the handler action
---- as well as a Go monad with the current state and no handler action.
--runGo :: Monad h => GoM h a -> (GoM h (), h ())
--runGo go =
--  let (_, state) = State.runState (goState go) emptyGoState
--      action = stateHandlerAction state
--  in (go >> updateState (\goState -> goState { stateHandlerAction = return () }),
--      action)

--zz:
foo :: GoM IO ()
foo = do
  addHandler $ const Nothing
  addHandler (\e -> case e of
               MoveEvent from to ->
                 Just $ putStrLn $ "Moved from " ++ show from ++ " to " ++ show to ++ "."
               _ -> Nothing)
  let Right cursor = rootCursor emptyNode
  putCursor cursor

--do
--  putRoot root
--  putCursor . head =<< getChildren

-- [(a, 0)]
-- [(a, 1)]
-- [(b, 0), (a, 2)]
-- [(c, 0), (a, 3)]
-- [(c, 1), (a, 3)]
