module Khumba.GoHS.Ui.Wx where

import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import Khumba.GoHS.Sgf
import Khumba.GoHS.Common
import Graphics.UI.WX hiding (Color, when)
import Graphics.UI.WXCore.WxcDefs (wxBU_EXACTFIT)
import qualified Graphics.UI.WX as WX

--data UiState = UiState { uiCursor :: MVar Cursor
--                       }

-- | The minimum board width and height in pixels.
minBoardLength :: Int
minBoardLength = 200

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

boardFrame cursor = do
  let board = cursorBoard cursor
      info = boardGameInfo board

  -- Create variables for UI state.
  canvasInfoVar <- newMVar CanvasInfo { canvasSize = sizeNull
                                      , canvasBoardUL = pointNull
                                      , canvasStoneLength = 0
                                      , canvasStoneRadius = 0
                                      }
  cursorVar <- newMVar cursor
  toolVar <- newMVar ToolPlay
  mouseVar <- newMVar MouseState { mouseCoordX = -1
                                 , mouseCoordY = -1
                                 , mouseIsValidMove = False
                                 }

  -- Create widgets used in the frame.
  frame <- frame [text := "Untitled game"]
  vsplitter <- splitterWindow frame []
  boardPanel <- panel vsplitter [fullRepaintOnResize := True] -- TODO Doesn't seem to work.
  rightPanel <- panel vsplitter []
  navPanel <- panel rightPanel []
  topButton <- buttonEx navPanel wxBU_EXACTFIT [text := "<<"]
  upButton <- buttonEx navPanel wxBU_EXACTFIT [text := "<"]
  downButton <- buttonEx navPanel wxBU_EXACTFIT [text := ">"]
  bottomButton <- buttonEx navPanel wxBU_EXACTFIT [text := ">>"]
  currentVariationText <- staticText rightPanel []
  childVariationsText <- staticText rightPanel []

  -- Declare handlers and curried versions of top-level functions that have been
  -- applied to their required variables, for ease of use in event handlers
  -- below.
  let doRedraw = repaint boardPanel
      updateMouse = updateMouseState cursorVar mouseVar

      onCanvasResize = updateCanvasInfo canvasInfoVar cursorVar boardPanel >> doRedraw
      onBoardChange = do
        cursor <- readMVar cursorVar

        -- Update the navigation buttons.
        let childCount = cursorChildCount cursor
        set topButton [text := if isJust $ cursorParent cursor then "<<" else "[["]
        set upButton [text := if isJust $ cursorParent cursor then "<" else "["]
        set downButton [text := if childCount > 0 then ">" else "]"]
        set bottomButton [text := if childCount > 0 then ">>" else "]]"]

        -- Update the variation labels.
        set currentVariationText [
          text := case cursorParent cursor of
             Nothing -> "Start of game."
             Just parent -> let parentChildCount = cursorChildCount parent
                            in if parentChildCount > 1
                               then "Variation " ++ show (cursorChildIndex cursor + 1) ++
                                    "/" ++ show parentChildCount ++ "."
                               else ""]
        set childVariationsText [text := case childCount of
                                    0 -> "End of variation."
                                    1 -> ""
                                    _ -> show childCount ++ " variations from here."]

        -- Update the mouse state and redraw the board.
        updateMouse
        doRedraw

      doGoTo cursorFn = goTo cursorFn cursorVar >> onBoardChange
      doGoTop = doGoTo (return . cursorRoot)
      doGoUp = goUp cursorVar onBoardChange
      doGoDown = goDown cursorVar onBoardChange
      doGoLeft = goLeft cursorVar onBoardChange
      doGoRight = goRight cursorVar onBoardChange
      doGoBottom = let findBottom cursor = case cursorChildren cursor of
                         child:_ -> findBottom child
                         _ -> cursor
                   in doGoTo (return . findBottom)

  set boardPanel [on resize := onCanvasResize,
                  on paint := drawBoard cursorVar canvasInfoVar mouseVar boardPanel,
                  on motion := updateMouseLocation canvasInfoVar cursorVar mouseVar False doRedraw,
                  on click := handleClick canvasInfoVar cursorVar mouseVar doRedraw]

  set topButton [on command := doGoTop]
  set upButton [on command := void doGoUp]
  set downButton [on command := void doGoDown]
  set bottomButton [on command := doGoBottom]

  -- Create a menubar.
  menuFile <- menuPane [text := "&File"]
  menuFileNew <- menuItem menuFile [text := "&New\tCtrl+N",
                                    on command := boardFrame $ rootCursor emptyNode]
  menuLine menuFile
  -- TODO This is close, not quit.
  menuFileQuit <- menuQuit menuFile [on command := close frame]

  -- Set up the window's layout.
  set frame [menuBar := [menuFile],
             layout := fill $ vsplit vsplitter 5 minBoardLength
               -- TODO minsize isn't working here, minBoardLength above seems to be the min?
               (fill $ {-minsize (sz minBoardLength minBoardLength) $-} widget boardPanel)
               (fill $ container rightPanel $ column 4 [
                   hfill $ container navPanel $ row 0 $ map (hfill . widget)
                     [topButton, upButton, downButton, bottomButton],
                   hfill $ widget currentVariationText,
                   hfill $ widget childVariationsText]),
             clientSize := sz 640 480]

  -- Install global keybindings.
  --
  -- wxWidgets doesn't propagate key events upward.  To work around this,
  -- install key handlers on all focusable widgets where it makes sense.
  -- See: http://wiki.wxwidgets.org/Catching_key_events_globally
  let setKeys :: Window a -> IO ()
      setKeys = flip set [on leftKey := void doGoUp,
                          on rightKey := void doGoDown,
                          on upKey := void doGoLeft,
                          on downKey := void doGoRight]
  setKeys boardPanel
  mapM_ setKeys [topButton, upButton, downButton, bottomButton]

  -- Set up initial UI state, e.g. disable the "<<" button because we're at the
  -- start.
  onBoardChange

-- | Applies the given function to the cursor.  Unlike 'goUp', 'goDown',
-- 'goLeft', 'goRight', etc., this function does not do any on-change event
-- handling.
goTo :: (Cursor -> IO Cursor) -> MVar Cursor -> IO ()
goTo cursorFn cursorVar = modifyMVar_ cursorVar cursorFn

goUp :: MVar Cursor -> IO () -> IO Bool
goUp cursorVar onChange = join $ modifyMVar cursorVar $ \cursor ->
  return $ case cursorParent cursor of
    Nothing -> (cursor, return False)
    Just parent -> (parent, onChange >> return True)

goDown :: MVar Cursor -> IO () -> IO Bool
goDown cursorVar onChange = join $ modifyMVar cursorVar $ \cursor ->
  return $ case cursorChildren cursor of
    [] -> (cursor, return False)
    child:_ -> (child, onChange >> return True)

goLeft :: MVar Cursor -> IO () -> IO Bool
goLeft cursorVar onChange = join $ modifyMVar cursorVar $ \cursor ->
  return $ case cursorParent cursor of
    Nothing -> (cursor, return False)
    Just parent -> case cursorChildIndex cursor of
      0 -> (cursor, return False)
      index -> (cursorChild parent (index - 1), onChange >> return True)

goRight :: MVar Cursor -> IO () -> IO Bool
goRight cursorVar onChange = join $ modifyMVar cursorVar $ \cursor ->
  return $ case cursorParent cursor of
    Nothing -> (cursor, return False)
    Just parent ->
      let count = cursorChildCount parent
          index = cursorChildIndex cursor
      in if index == count - 1
         then (cursor, return False)
         else (cursorChild parent (index + 1), onChange >> return True)

updateMouseLocation :: MVar CanvasInfo
                    -> MVar Cursor
                    -> MVar MouseState
                    -> Bool
                    -> IO ()  -- ^ Action to take when the mouse location changes.
                    -> Point  -- ^ Location of the mouse cursor.
                    -> IO ()
updateMouseLocation canvasInfoVar cursorVar mouseVar forceStateUpdate onChanged (Point x y) = do
  CanvasInfo { canvasBoardUL = Point x0 y0
             , canvasStoneLength = sl
             , canvasStoneRadius = sr
             } <- readMVar canvasInfoVar
  let ix = (x - x0) `div` sl
      iy = (y - y0) `div` sl
  cursor <- readMVar cursorVar
  join $ modifyMVar mouseVar $ \mouse ->
    if not forceStateUpdate && ix == mouseCoordX mouse && iy == mouseCoordY mouse
    then return (mouse, return ())
    else let mouse' = mouse { mouseCoordX = ix, mouseCoordY = iy }
         in return (updateMouseState' cursor mouse', onChanged)

-- | Reads the coordinate location of the mouse cursor from the
-- 'MouseState' and updates the other variables according to the
-- location.  Returns true if the 'MouseState' changed.
updateMouseState :: MVar Cursor
                 -> MVar MouseState
                 -> IO Bool
updateMouseState cursorVar mouseVar = do
  cursor <- readMVar cursorVar
  modifyMVar mouseVar $ \mouse ->
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
handleClick :: MVar CanvasInfo
            -> MVar Cursor
            -> MVar MouseState
            -> IO ()  -- ^ Action to take when the click places a stone, or
                      -- navigates to an existing child node.
            -> Point  -- ^ Location of the mouse cursor
            -> IO ()
handleClick canvasInfoVar cursorVar mouseVar onChanged mousePoint = do
  -- First ensure that the info in the mouse state is current.
  updateMouse False
  mouse <- readMVar mouseVar
  when (mouseIsValidMove mouse) $ do
    modifyMVar_ cursorVar $ \cursor -> do
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

drawBoard :: MVar Cursor
          -> MVar CanvasInfo
          -> MVar MouseState
          -> Panel a
          -> DC ()
          -> Rect
          -> IO ()
drawBoard cursorVar canvasInfoVar mouseVar boardPanel dc _ = do
  canvasInfo <- readMVar canvasInfoVar
  cursor <- readMVar cursorVar
  mouse <- readMVar mouseVar
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

updateCanvasInfo :: MVar CanvasInfo -> MVar Cursor -> Panel a -> IO ()
updateCanvasInfo canvasInfoVar cursorVar boardPanel = do
  cursor <- readMVar cursorVar
  modifyMVar_ canvasInfoVar . const . return =<<
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
