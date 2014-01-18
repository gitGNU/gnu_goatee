-- | A widget that renders an interactive Go board.
module Khumba.GoHS.Ui.Gtk.Goban ( Goban
                                , create
                                , initialize
                                , myDrawingArea
                                ) where

import Control.Monad
import Data.IORef
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (Color, Cursor)
import Khumba.GoHS.Common
import Khumba.GoHS.Sgf hiding (isValidMove)
import Khumba.GoHS.Sgf.Monad (navigationEvent)
import Khumba.GoHS.Ui.Gtk.Common

boardBgColor :: Rgb
boardBgColor = rgb255 229 178 58

blackStoneColor :: Rgb
blackStoneColor = rgb 0 0 0

blackStoneBorderColor :: Rgb
blackStoneBorderColor = rgb 1 1 1

whiteStoneColor :: Rgb
whiteStoneColor = rgb 1 1 1

whiteStoneBorderColor :: Rgb
whiteStoneBorderColor = rgb 0 0 0

stoneColor :: Color -> Rgb
stoneColor color = case color of
  Black -> blackStoneColor
  White -> whiteStoneColor

stoneBorderColor :: Color -> Rgb
stoneBorderColor color = case color of
  Black -> blackStoneBorderColor
  White -> whiteStoneBorderColor

-- | Percentage, in @[0, 1]@.
stoneBorderThickness :: Double
stoneBorderThickness = 0.03

-- | The opacity, in @[0, 1]@, of a stone that should be drawn as transparent,
-- e.g. when hovering over a empty point on the board.
transparentStoneOpacity :: Double
transparentStoneOpacity = 0.7

-- | A GTK widget that renders a Go board.
--
-- @ui@ should be an instance of 'UiCtrl'.
data Goban ui = Goban { myUi :: UiRef ui
                      , myDrawingArea :: DrawingArea
                      , myNavigationHandler :: IORef (Maybe Registration)
                      }

-- | Holds data relating to the state of the mouse hovering over the board.
data HoverState = HoverState { hoverCoord :: Maybe Coord
                               -- ^ The board coordinate corresponding to the
                               -- current mouse position.  Nothing if the mouse
                               -- is not over the board.
                             , hoverIsValidMove :: Bool
                               -- ^ True iff the hovered point is legal to play
                               -- on for the current player.
                             } deriving (Show)

-- | Creates a 'Goban' for rendering Go boards of the given size.
create :: UiCtrl ui => UiRef ui -> IO (Goban ui)
create uiRef = do
  hoverStateRef <- newIORef HoverState { hoverCoord = Nothing
                                       , hoverIsValidMove = False
                                       }

  drawingArea <- drawingAreaNew
  widgetAddEvents drawingArea [LeaveNotifyMask,
                               ButtonPressMask,
                               PointerMotionMask]
  on drawingArea exposeEvent $ liftIO $ do
    drawBoard uiRef hoverStateRef drawingArea
    return True

  on drawingArea motionNotifyEvent $ do
    mouseCoord <- fmap Just eventCoordinates
    liftIO $ handleMouseMove uiRef hoverStateRef drawingArea mouseCoord
    return True
  on drawingArea leaveNotifyEvent $ do
    liftIO $ handleMouseMove uiRef hoverStateRef drawingArea Nothing
    return True

  on drawingArea buttonPressEvent $ do
    mouseXy <- eventCoordinates
    liftIO $ doToolAtPoint uiRef drawingArea mouseXy
    return True

  navigationHandler <- newIORef Nothing

  return Goban { myUi = uiRef
               , myDrawingArea = drawingArea
               , myNavigationHandler = navigationHandler
               }

initialize :: UiCtrl ui => Goban ui -> IO ()
initialize goban = do
  ui <- readUiRef $ myUi goban
  let onChange = widgetQueueDraw $ myDrawingArea goban
  navHandlerRegistration <- register ui navigationEvent $ const $ afterGo onChange
  writeIORef (myNavigationHandler goban) $ Just navHandlerRegistration
  -- TODO Need to update the hover state's validity on cursor and tool (mode?)
  -- changes.
  onChange

-- | Called when the mouse is moved.  Updates the 'HoverState' according to the
-- new mouse location, and redraws the board if necessary.
handleMouseMove :: UiCtrl a
                => UiRef a
                -> IORef HoverState
                -> DrawingArea
                -> Maybe (Double, Double)
                -> IO ()
handleMouseMove uiRef hoverStateRef drawingArea maybeClickCoord = do
  ui <- readUiRef uiRef
  maybeXy <- case maybeClickCoord of
    Nothing -> return Nothing
    Just (mouseX, mouseY) -> do
      board <- fmap cursorBoard (readCursor ui)
      gtkToBoardCoordinates board drawingArea mouseX mouseY
  updateHoverPosition ui hoverStateRef maybeXy >>= flip when
    (widgetQueueDraw drawingArea)

-- | Applies the current tool at the given GTK coordinate, if such an action is
-- valid.
doToolAtPoint :: UiCtrl ui => UiRef ui -> DrawingArea -> (Double, Double) -> IO ()
doToolAtPoint uiRef drawingArea (mouseX, mouseY) = do
  ui <- readUiRef uiRef
  cursor <- readCursor ui
  let board = cursorBoard cursor
  maybeXy <- gtkToBoardCoordinates board drawingArea mouseX mouseY
  whenMaybe maybeXy $ \xy -> do
    tool <- fmap uiTool (readModes ui)

    case tool of
      ToolPlay -> do
        valid <- isValidMove ui xy
        when valid $ playAt ui xy
      _ -> return ()  -- TODO Support other tools.

-- | Updates the hover state for the mouse having moved to the given board
-- coordinate.  Returns true if the board coordinate has changed.
updateHoverPosition :: UiCtrl ui => ui -> IORef HoverState -> Maybe Coord -> IO Bool
updateHoverPosition ui hoverStateRef maybeXy = do
  hoverState <- readIORef hoverStateRef
  if maybeXy == hoverCoord hoverState
    then return False
    else do valid <- case maybeXy of
              Nothing -> return False
              Just xy -> isValidMove ui xy
            writeIORef hoverStateRef HoverState { hoverCoord = maybeXy
                                                , hoverIsValidMove = valid
                                                }
            return True

applyBoardCoordinates :: BoardState -> DrawingArea -> IO (Render ())
applyBoardCoordinates board drawingArea = do
  (canvasWidth, canvasHeight) <- return . mapTuple fromIntegral =<< widgetGetSize drawingArea

  let maxStoneWidth = canvasWidth / fromIntegral (boardWidth board)
      maxStoneHeight = canvasHeight / fromIntegral (boardHeight board)
      maxStoneLength = min maxStoneWidth maxStoneHeight

  return $ do
    -- Set user coordinates so that the top-left stone occupies the rectangle
    -- from (0,0) to (1,1).
    when (canvasWidth > canvasHeight) $ translate ((canvasWidth - canvasHeight) / 2) 0
    when (canvasHeight > canvasWidth) $ translate 0 ((canvasHeight - canvasWidth) / 2)
    scale maxStoneLength maxStoneLength

-- | Takes a GTK coordinate and, using a Cairo rendering context, returns the
-- corresponding board coordinate, or @Nothing@ if the GTK coordinate is not
-- over the board.
gtkToBoardCoordinates :: BoardState -> DrawingArea -> Double -> Double -> IO (Maybe (Int, Int))
gtkToBoardCoordinates board drawingArea x y = do
  drawWindow <- widgetGetDrawWindow drawingArea
  changeCoords <- applyBoardCoordinates board drawingArea
  result@(bx, by) <- fmap (mapTuple floor) $
                     renderWithDrawable drawWindow $
                     changeCoords >> deviceToUser x y
  return $ if bx < 0 || bx >= boardWidth board ||
              by < 0 || by >= boardHeight board
           then Nothing
           else Just result

-- | Fully redraws the board based on the current controller and UI state.
drawBoard :: UiCtrl ui => UiRef ui -> IORef HoverState -> DrawingArea -> IO ()
drawBoard uiRef hoverStateRef drawingArea = do
  ui <- readUiRef uiRef
  cursor <- readCursor ui
  tool <- fmap uiTool (readModes ui)
  hoverState <- readIORef hoverStateRef

  let board = cursorBoard cursor
      cols = fromIntegral $ boardWidth board
      rows = fromIntegral $ boardHeight board

  drawWindow <- widgetGetDrawWindow drawingArea
  changeCoords <- applyBoardCoordinates board drawingArea
  renderWithDrawable drawWindow $ do
    changeCoords

    -- Fill the background a nice woody shade.
    setRgb boardBgColor
    paint

    setSourceRGB 0 0 0
    rectangle 0.5 0.5 (cols - 1) (rows - 1)
    gridLineWidth <- fmap fst $ deviceToUserDistance 1 0
    setLineWidth gridLineWidth
    stroke

    sequence_ $ flip mapBoardCoords board $
      drawCoord board gridLineWidth (gridLineWidth * 2) tool hoverState
  return ()

-- | Draws a single point on the board.
drawCoord :: BoardState -- ^ The board being drawn.
          -> Double     -- ^ The pixel width of the grid in the board's interior.
          -> Double     -- ^ The pixel width of the grid on the board's border.
          -> Tool       -- ^ The current tool.
          -> HoverState -- ^ The current hover state.
          -> Int        -- ^ The x-index of the point to be drawn.
          -> Int        -- ^ The y-index of the point to be drawn.
          -> CoordState -- ^ The point to be drawn.
          -> Render ()
drawCoord board gridWidth gridBorderWidth tool hoverState x y coord = do
  let x' = fromIntegral x
      y' = fromIntegral y
      draw = do
        -- Draw the grid.
        let atLeft = x == 0
            atTop = y == 0
            atRight = x == boardWidth board - 1
            atBottom = y == boardHeight board - 1
            gridX0 = x' + if atLeft then 0.5 else 0
            gridY0 = y' + if atTop then 0.5 else 0
            gridX1 = x' + if atRight then 0.5 else 1
            gridY1 = y' + if atBottom then 0.5 else 1
        -- Temporarily disable antialiasing.  We want grid lines to be sharp.
        setAntialias AntialiasNone
        setSourceRGB 0 0 0
        setLineWidth $ if atTop || atBottom then gridBorderWidth else gridWidth
        moveTo gridX0 (y' + 0.5)
        lineTo gridX1 (y' + 0.5)
        stroke
        setLineWidth $ if atLeft || atRight then gridBorderWidth else gridWidth
        moveTo (x' + 0.5) gridY0
        lineTo (x' + 0.5) gridY1
        stroke
        setAntialias AntialiasDefault

        -- Draw a stone if present.
        translate x' y'
        if (tool == ToolBlack || tool == ToolWhite) &&
           isJust (hoverCoord hoverState) &&
           fst (fromJust $ hoverCoord hoverState) == x &&
           snd (fromJust $ hoverCoord hoverState) == y &&
           coordStone coord /= Just (toolToColor tool)
          then drawStone (toolToColor tool) True
          else case coordStone coord of
            Just color -> drawStone color False
            Nothing -> fromMaybe (return ()) $
                       do if tool == ToolPlay then Just () else Nothing
                          (hx, hy) <- hoverCoord hoverState
                          if x == hx && y == hy && hoverIsValidMove hoverState then Just () else Nothing
                          return $ drawStone (boardPlayerTurn board) True
        translate (-x') (-y')

  case coordVisibility coord of
    CoordVisible -> draw
    CoordInvisible -> return ()
    CoordDimmed -> error "TODO: Implement rendering of CoordDimmed."

-- | Draws a stone from @(0, 0)@ to @(1, 1)@ in user coordinates.
drawStone :: Color -- ^ The color of stone to draw.
          -> Bool  -- ^ If true, the stone is transparent; if false, opaque.
          -> Render ()
drawStone color transparent = do
  let opacity = if transparent then transparentStoneOpacity else 1
  arc 0.5 0.5 (0.5 - stoneBorderThickness / 2) 0 (2 * pi)
  setRgbA (stoneColor color) opacity
  fillPreserve
  setLineWidth stoneBorderThickness
  setRgbA (stoneBorderColor color) opacity
  stroke

type Rgb = (Double, Double, Double)

rgb :: Double -> Double -> Double -> Rgb
rgb = (,,)

rgb255 :: Double -> Double -> Double -> Rgb
rgb255 r g b = (r / 255, g / 255, b / 255)

setRgb :: Rgb -> Render ()
setRgb (r, g, b) = setSourceRGB r g b

setRgbA :: Rgb -> Double -> Render ()
setRgbA (r, g, b) = setSourceRGBA r g b
