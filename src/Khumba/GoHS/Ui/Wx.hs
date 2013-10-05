module Khumba.GoHS.Ui.Wx where

import qualified Control.Concurrent.MVar as MV
import qualified Control.Concurrent.MVar.Strict as MVS
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import Khumba.GoHS.Sgf
import Khumba.GoHS.Common
import Graphics.UI.WX hiding (Color, when)
import qualified Graphics.UI.WX as WX

--data UiState = UiState { uiCursor :: MVar Cursor
--                       }

-- | The minimum board width and height in pixels.
minBoardDiameter :: Int
minBoardDiameter = 200

boardBgColor :: WX.Color
boardBgColor = rgb 229 178 58

boardPadding :: Int
boardPadding = 8

blackStoneStyle = [penColor := white, brushColor := black]
whiteStoneStyle = [penColor := black, brushColor := white]

blackStoneHoverStyle = [penColor := colorAverage white boardBgColor,
                        brushColor := colorAverage black boardBgColor]
whiteStoneHoverStyle = [penColor := colorAverage black boardBgColor,
                        brushColor := colorAverage white boardBgColor]

stoneStyle Black = blackStoneStyle
stoneStyle White = whiteStoneStyle

hoverStoneStyle Black = blackStoneHoverStyle
hoverStoneStyle White = whiteStoneHoverStyle

gridStyle = [penColor := black, brushColor := black]

starRadius :: Rational
starRadius = 1/8

data Tool = ToolPlay

instance NFData Tool

data CanvasInfo = CanvasInfo { canvasSize :: Size
                             , canvasBoardUL :: Point
                             , canvasStoneLength :: Int
                             , canvasStoneRadius :: Int
                               -- ^ The offset from a stone's upper-left corner
                               -- to its center point.
                             }

data MouseState = MouseState { mouseCoordX :: Int
                             , mouseCoordY :: Int
                             , mouseIsValidMove :: Bool
                             } deriving (Eq)

instance NFData MouseState where
  rnf mouse = rnf (mouseCoordX mouse) `seq`
              rnf (mouseCoordY mouse) `seq`
              rnf (mouseIsValidMove mouse)

boardFrame cursor = do
  let board = cursorBoard cursor
      info = boardGameInfo board

  canvasInfoVar <- MV.newMVar CanvasInfo { canvasSize = sizeNull
                                         , canvasBoardUL = pointNull
                                         , canvasStoneLength = 0
                                         , canvasStoneRadius = 0
                                         }
  cursorVar <- MVS.newMVar cursor
  toolVar <- MVS.newMVar ToolPlay
  mouseVar <- MVS.newMVar MouseState { mouseCoordX = -1
                                     , mouseCoordY = -1
                                     , mouseIsValidMove = False
                                     }

  frame <- frame [text := "Untitled game"]
  boardPanel <- panel frame []
  set boardPanel [on paint := drawBoard cursorVar canvasInfoVar mouseVar boardPanel]

  let redraw = repaint boardPanel
      updateMouse = updateMouseState cursorVar mouseVar

  set frame [layout := minsize (sz minBoardDiameter minBoardDiameter) $
                       fill $
                       widget boardPanel]
  set boardPanel [on leftKey := goUp cursorVar (updateMouse >> redraw),
                  on rightKey := goDown cursorVar (updateMouse >> redraw),
                  on resize := updateCanvasInfo canvasInfoVar cursorVar boardPanel,
                  on motion := updateMouseLocation canvasInfoVar cursorVar mouseVar False redraw,
                  on click := handleClick canvasInfoVar cursorVar mouseVar redraw]

  menuFile <- menuPane [text := "&File"]
  menuFileNew <- menuItem menuFile [text := "&New\tCtrl+N",
                                    on command := boardFrame $ fromRight $ rootCursor $ rootNode 9 9]
  menuLine menuFile
  menuFileQuit <- menuQuit menuFile [on command := close frame]
  set frame [menuBar := [menuFile]]

goUp :: MVS.MVar Cursor -> IO () -> IO ()
goUp cursorVar onChange = join $ MVS.modifyMVar cursorVar $ \cursor ->
  return $ case cursorParent cursor of
    Nothing -> (cursor, return ())
    Just parent -> (parent, onChange)

goDown :: MVS.MVar Cursor -> IO () -> IO ()
goDown cursorVar onChange = join $ MVS.modifyMVar cursorVar $ \cursor ->
  return $ case cursorChildren cursor of
    [] -> (cursor, return ())
    child:_ -> (child, onChange)

updateMouseLocation :: MV.MVar CanvasInfo
                    -> MVS.MVar Cursor
                    -> MVS.MVar MouseState
                    -> Bool
                    -> IO ()  -- ^ Action to take when the mouse location changes.
                    -> Point  -- ^ Location of the mouse cursor.
                    -> IO ()
updateMouseLocation canvasInfoVar cursorVar mouseVar forceStateUpdate onChanged (Point x y) = do
  CanvasInfo { canvasBoardUL = Point x0 y0
             , canvasStoneLength = sl
             , canvasStoneRadius = sr
             } <- MV.readMVar canvasInfoVar
  let ix = (x - x0) `div` sl
      iy = (y - y0) `div` sl
  join $ MVS.modifyMVar mouseVar $ \mouse ->
    if not forceStateUpdate && ix == mouseCoordX mouse && iy == mouseCoordY mouse
    then return (mouse, return ())
    else do cursor <- MVS.readMVar cursorVar
            let mouse' = mouse { mouseCoordX = ix, mouseCoordY = iy }
            return (updateMouseState' cursor mouse', onChanged)

-- | Reads the coordinate location of the mouse cursor from the
-- 'MouseState' and updates the other variables according to the
-- location.  Returns true if the 'MouseState' changed.
updateMouseState :: MVS.MVar Cursor
                 -> MVS.MVar MouseState
                 -> IO Bool
updateMouseState cursorVar mouseVar = do
  cursor <- MVS.readMVar cursorVar
  MVS.modifyMVar mouseVar $ \mouse ->
    let mouse' = updateMouseState' cursor mouse
    in return $ if mouse' == mouse
                then (mouse, False)
                else (mouse', True)

-- | Updates fields of a 'MouseState' based on its mouse coordinate.
updateMouseState' :: Cursor -> MouseState -> MouseState
updateMouseState' cursor mouse =
  let board = cursorBoard cursor
      color = boardPlayerTurn board
      valid = isValidMove board color (mouseCoordX mouse, mouseCoordY mouse)
  in mouse { mouseIsValidMove = valid }

-- | Updates the mouse state (via 'updateMouseLocation') then, if the mouse is
-- over a point at which it is valid to place a stone, does so.  If there is
-- already a child of the current node in which the current player plays at the
-- clicked point, then we simply navigate to that node instead.
handleClick :: MV.MVar CanvasInfo
            -> MVS.MVar Cursor
            -> MVS.MVar MouseState
            -> IO ()  -- ^ Action to take when the click places a stone, or
                      -- navigates to an existing child node.
            -> Point  -- ^ Location of the mouse cursor
            -> IO ()
handleClick canvasInfoVar cursorVar mouseVar onChanged mousePoint = do
  -- First ensure that the info in the mouse state is current.
  updateMouse False
  mouse <- MVS.readMVar mouseVar
  when (mouseIsValidMove mouse) $ do
    MVS.modifyMVar_ cursorVar $ \cursor -> do
      let xy = (mouseCoordX mouse, mouseCoordY mouse)
      return $ fromMaybe
        (let board = cursorBoard cursor
             player = boardPlayerTurn board
             child = emptyNode { nodeProperties = [colorToMove player xy] }
             cursor' = cursorModifyNode (addChild child) cursor
             childCursor = cursorChild cursor' $ cursorChildCount cursor' - 1
         in childCursor)
        (cursorChildPlayingAt xy cursor)
    updateMouse True
    onChanged
  where updateMouse force = updateMouseLocation canvasInfoVar cursorVar mouseVar force (return ()) mousePoint

drawBoard :: MVS.MVar Cursor
          -> MV.MVar CanvasInfo
          -> MVS.MVar MouseState
          -> Panel a
          -> DC ()
          -> Rect
          -> IO ()
drawBoard cursorVar canvasInfoVar mouseVar boardPanel dc _ = do
  canvasInfo <- MV.readMVar canvasInfoVar
  cursor <- MVS.readMVar cursorVar
  mouse <- MVS.readMVar mouseVar
  let board = cursorBoard cursor
      CanvasInfo { canvasSize = canvasSize
                 , canvasBoardUL = topLeft
                 , canvasStoneLength = sl
                 , canvasStoneRadius = sr
                 } = canvasInfo

  -- Draw the background.
  set dc [penKind := PenSolid, brushKind := BrushSolid]
  drawRect dc (rectFromSize canvasSize) $
    map (:= boardBgColor) [penColor, brushColor]

  -- Draw the board.
  sequence_ $ mapBoardCoords (drawCoord board canvasInfo mouse dc) board

drawCoord :: BoardState
             -> CanvasInfo
             -> MouseState
             -> DC ()
             -> Int
             -> Int
             -> CoordState
             -> IO ()
drawCoord board canvasInfo mouse dc ix iy coord =
  case coordVisibility coord of
    CoordInvisible -> return ()
    CoordDimmed -> dcWith dc
                          [penKind := PenDash DashDot, brushKind := BrushHatch HatchFDiagonal]
                          draw
    CoordVisible -> draw
  where draw = case coordStone coord of
          Just color -> drawStone canvasInfo dc ix iy (stoneStyle color)
          Nothing ->
            let color = boardPlayerTurn board
            in if mouseCoordX mouse == ix &&
                  mouseCoordY mouse == iy &&
                  mouseIsValidMove mouse
               then drawStone canvasInfo dc ix iy (hoverStoneStyle color)
               else drawGrid board canvasInfo dc ix iy (coordStar coord)

drawStone :: CanvasInfo -> DC () -> Int -> Int -> [Prop (DC ())] -> IO ()
drawStone canvasInfo dc ix iy style =
  let topLeft = canvasBoardUL canvasInfo
      sl = canvasStoneLength canvasInfo
      sr = canvasStoneRadius canvasInfo
  in circle dc
            (pointAdd topLeft $ pt (ix * sl + sr) (iy * sl + sr))
            sr
            style

drawGrid :: BoardState -> CanvasInfo -> DC () -> Int -> Int -> Bool -> IO ()
drawGrid board canvasInfo dc ix iy isStar =
  let Point x0 y0 = canvasBoardUL canvasInfo
      sl = canvasStoneLength canvasInfo
      sr = canvasStoneRadius canvasInfo
      x = x0 + ix * sl + sr
      y = y0 + iy * sl + sr
      x1 = if ix == 0 then x else x - sr
      y1 = if iy == 0 then y else y - sr
      x2 = if ix == boardWidth board - 1 then x else x + sr
      y2 = if iy == boardHeight board - 1 then y else y + sr
  in do line dc (pt x1 y) (pt (x2 + 1) y) gridStyle
        line dc (pt x y1) (pt x (y2 + 1)) gridStyle
        when isStar $
          circle dc
                 (pt x y)
                 (ceiling $ fromIntegral sl * starRadius)
                 gridStyle

updateCanvasInfo :: MV.MVar CanvasInfo -> MV.MVar Cursor -> Panel a -> IO ()
updateCanvasInfo canvasInfoVar cursorVar boardPanel = do
  cursor <- MVS.readMVar cursorVar
  MV.modifyMVar_ canvasInfoVar . const . return =<<
    canvasInfo boardPanel (cursorBoard cursor)

canvasInfo :: Panel a -> BoardState -> IO CanvasInfo
canvasInfo boardPanel board = do
  -- Calculate the rendered board's pixel width and height.
  canvasSize@(Size cw ch) <- get boardPanel clientSize
  let bw = boardWidth board   -- In stones.
      bh = boardHeight board
      stoneMaxX = truncate $ fromIntegral (cw - boardPadding * 2) / fromIntegral bw
      stoneMaxY = truncate $ fromIntegral (ch - boardPadding * 2) / fromIntegral bh
      stoneLen = let x = min stoneMaxX stoneMaxY
                 in if odd x then x else x - 1
      stoneRadius = (stoneLen - 1) `div` 2
      stepX = let x = stoneLen * bw `div` 2
              in if even bw then x else x + stoneRadius
      stepY = let y = stoneLen * bh `div` 2
              in if even bh then y else y + stoneRadius
      boardLeft = cw `div` 2 - stepX
      boardTop = ch `div` 2 - stepY
  return CanvasInfo { canvasSize = canvasSize
                    , canvasBoardUL = pt boardLeft boardTop
                    , canvasStoneLength = stoneLen
                    , canvasStoneRadius = stoneRadius
                    }

colorAverage :: WX.Color -> WX.Color -> WX.Color
colorAverage color1 color2 =
  let r1 = colorRed color1
      r2 = colorRed color2
      g1 = colorGreen color1
      g2 = colorGreen color2
      b1 = colorBlue color1
      b2 = colorBlue color2
  in rgb (avg r1 r2) (avg g1 g2) (avg b1 b2)
  where avg x y = x + (y - x) `div` 2
