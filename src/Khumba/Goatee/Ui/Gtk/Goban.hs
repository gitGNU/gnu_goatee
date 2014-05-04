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

-- | A widget that renders an interactive Go board.
module Khumba.Goatee.Ui.Gtk.Goban (
  Goban,
  create,
  destroy,
  myDrawingArea,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.Rendering.Cairo (
  Antialias (AntialiasDefault, AntialiasNone),
  Render,
  arc,
  closePath,
  deviceToUser,
  deviceToUserDistance,
  fillPreserve,
  liftIO,
  lineTo,
  moveTo,
  paint,
  paintWithAlpha,
  popGroupToSource,
  pushGroup,
  rotate,
  scale,
  setAntialias,
  setLineWidth,
  setSourceRGB,
  stroke,
  translate,
  )
import Graphics.UI.Gtk (
  DrawingArea,
  EventMask (ButtonPressMask, LeaveNotifyMask, PointerMotionMask),
  buttonPressEvent,
  drawingAreaNew,
  eventCoordinates,
  exposeEvent,
  leaveNotifyEvent,
  motionNotifyEvent,
  on,
  renderWithDrawable,
  widgetAddEvents, widgetGetDrawWindow, widgetGetSize, widgetQueueDraw,
  )
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Board hiding (isValidMove)
import Khumba.Goatee.Sgf.Monad hiding (on)
import Khumba.Goatee.Sgf.Types
import Khumba.Goatee.Ui.Gtk.Common

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

-- | Percentage of coordinate size, in @[0, 1]@.
stoneBorderThickness :: Double
stoneBorderThickness = 0.03

-- | The radius of small circles that are overlaid on points to indicate that
-- move variations exist.  Percentage of coordinate size, in @[0, 1]@.
stoneVariationRadius :: Double
stoneVariationRadius = 0.15

-- | The width of the border of a variation circle.  Percentage of coordinate
-- size, in @[0, 1]@.
stoneVariationBorderThickness :: Double
stoneVariationBorderThickness = 0.02

-- | The opacity, in @[0, 1]@, of a stone that should be drawn dimmed because of
-- 'DD'.
dimmedPointOpacity :: Double
dimmedPointOpacity = 0.3

-- | Returns the color that should be used to draw a 'Mark' on either an empty
-- point, or one with a stone of the given color.
coordAnnotationStrokeColor :: Maybe Color -> Rgb
coordAnnotationStrokeColor = maybe blackStoneColor stoneBorderColor

-- | For line and arrow annotations, the width of the line.  1 is the width of a
-- stone.
boardAnnotationLineWidth :: Double
boardAnnotationLineWidth = 0.08

-- | For arrow annotations, the distance to pull back along the length of a line
-- before extending at right angles to form the arrowhead.
boardAnnotationArrowPullback :: Double
boardAnnotationArrowPullback = 0.2

-- | For arrow annotations, the distance to extend away from the baseline in
-- either direction to form the arrowhead.
boardAnnotationArrowWidth :: Double
boardAnnotationArrowWidth = 0.1

-- | A GTK widget that renders a Go board.
--
-- @ui@ should be an instance of 'UiCtrl'.
data Goban ui = Goban { myUi :: ui
                      , myRegistrations :: ViewRegistrations
                      , myDrawingArea :: DrawingArea
                      }

instance UiCtrl ui => UiView (Goban ui) ui where
  viewName = const "Goban"
  viewCtrl = myUi
  viewRegistrations = myRegistrations

-- | Holds data relating to the state of the mouse hovering over the board.
data HoverState = HoverState { hoverCoord :: Maybe Coord
                               -- ^ The board coordinate corresponding to the
                               -- current mouse position.  Nothing if the mouse
                               -- is not over the board.
                             , hoverIsValidMove :: Bool
                               -- ^ True iff the hovered point is legal to play
                               -- on for the current player.
                             } deriving (Show)

-- | Augments a 'CoordState' with data that is only used for rendering purposes.
data RenderedCoord = RenderedCoord {
  renderedCoordState :: CoordState
  , renderedCoordVariation :: Maybe Color
    -- ^ If a variation move exists at this point, then this will be the color
    -- of the move.
  } deriving (Show)

-- | Creates a 'Goban' for rendering Go boards of the given size.
create :: UiCtrl ui => ui -> IO (Goban ui)
create ui = do
  hoverStateRef <- newIORef HoverState { hoverCoord = Nothing
                                       , hoverIsValidMove = False
                                       }

  drawingArea <- drawingAreaNew
  widgetAddEvents drawingArea [LeaveNotifyMask,
                               ButtonPressMask,
                               PointerMotionMask]
  on drawingArea exposeEvent $ liftIO $ do
    drawBoard ui hoverStateRef drawingArea
    return True

  on drawingArea motionNotifyEvent $ do
    mouseCoord <- fmap Just eventCoordinates
    liftIO $ handleMouseMove ui hoverStateRef drawingArea mouseCoord
    return True
  on drawingArea leaveNotifyEvent $ do
    liftIO $ handleMouseMove ui hoverStateRef drawingArea Nothing
    return True

  on drawingArea buttonPressEvent $ do
    mouseXy <- eventCoordinates
    liftIO $ doToolAtPoint ui drawingArea mouseXy
    return True

  registrations <- viewNewRegistrations

  let me = Goban { myUi = ui
                 , myRegistrations = registrations
                 , myDrawingArea = drawingArea
                 }

  initialize me
  return me

initialize :: UiCtrl ui => Goban ui -> IO ()
initialize me = do
  let onChange = afterGo $ update me
  viewRegister me childAddedEvent $ const $ const onChange
  viewRegister me navigationEvent $ const onChange
  viewRegister me propertiesChangedEvent $ const $ const onChange
  -- TODO Need to update the hover state's validity on cursor and tool (mode?)
  -- changes.
  update me

destroy :: UiCtrl ui => Goban ui -> IO ()
destroy = viewUnregisterAll

update :: UiCtrl ui => Goban ui -> IO ()
update = widgetQueueDraw . myDrawingArea

-- | Called when the mouse is moved.  Updates the 'HoverState' according to the
-- new mouse location, and redraws the board if necessary.
handleMouseMove :: UiCtrl a
                => a
                -> IORef HoverState
                -> DrawingArea
                -> Maybe (Double, Double)
                -> IO ()
handleMouseMove ui hoverStateRef drawingArea maybeClickCoord = do
  maybeXy <- case maybeClickCoord of
    Nothing -> return Nothing
    Just (mouseX, mouseY) -> do
      board <- fmap cursorBoard (readCursor ui)
      gtkToBoardCoordinates board drawingArea mouseX mouseY
  updateHoverPosition ui hoverStateRef maybeXy >>= flip when
    (widgetQueueDraw drawingArea)

-- | Applies the current tool at the given GTK coordinate, if such an action is
-- valid.
doToolAtPoint :: UiCtrl ui => ui -> DrawingArea -> (Double, Double) -> IO ()
doToolAtPoint ui drawingArea (mouseX, mouseY) = do
  cursor <- readCursor ui
  let board = cursorBoard cursor
  maybeXy <- gtkToBoardCoordinates board drawingArea mouseX mouseY
  whenMaybe maybeXy $ \xy -> do
    tool <- fmap uiTool (readModes ui)

    case tool of
      ToolMarkCircle -> toggleMark ui xy MarkCircle
      ToolMarkSelected -> toggleMark ui xy MarkSelected
      ToolMarkSquare -> toggleMark ui xy MarkSquare
      ToolMarkTriangle -> toggleMark ui xy MarkTriangle
      ToolMarkX -> toggleMark ui xy MarkX
      ToolPlay -> do
        valid <- isValidMove ui xy
        when valid $ playAt ui xy
      _ -> return ()  -- TODO Support other tools.
  where toggleMark ui xy mark = runUiGo ui $ modifyMark (toggleMark' mark) xy
        toggleMark' mark maybeExistingMark = case maybeExistingMark of
          Just existingMark | existingMark == mark -> Nothing
          _ -> Just mark

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
drawBoard :: UiCtrl ui => ui -> IORef HoverState -> DrawingArea -> IO ()
drawBoard ui hoverStateRef drawingArea = do
  cursor <- readCursor ui
  modes <- readModes ui
  hoverState <- readIORef hoverStateRef

  let board = cursorBoard cursor
      tool = uiTool modes
      variationMode = rootInfoVariationMode $ gameInfoRootInfo $ boardGameInfo $ cursorBoard cursor

      variations :: [(Coord, Color)]
      variations = cursorVariations (variationModeSource variationMode) cursor

      renderedCoords :: [[RenderedCoord]]
      renderedCoords =
        foldr (\((x, y), color) grid ->
                listUpdate (flip listUpdate x $
                            \renderedCoord -> renderedCoord { renderedCoordVariation = Just color })
                           y
                           grid)
              (map (map $ \state -> RenderedCoord state Nothing) $
               mapBoardCoords maybeAddHover board)
              variations

      maybeAddHover :: Int -> Int -> CoordState -> CoordState
      maybeAddHover y x state = case hoverCoord hoverState of
        Just (hx, hy) | x == hx && y == hy ->
          modifyCoordForHover tool board hoverState state
        _ -> state

  drawWindow <- widgetGetDrawWindow drawingArea
  changeCoords <- applyBoardCoordinates board drawingArea
  renderWithDrawable drawWindow $ do
    changeCoords

    -- Fill the background a nice woody shade.
    setRgb boardBgColor
    paint

    -- Draw the grid and all points.
    gridLineWidth <- fst <$> deviceToUserDistance 1 0

    let drawCoord' = drawCoord board gridLineWidth (gridLineWidth * 2)

    -- First draw points that are visible and not dimmed.
    forIndexM_ renderedCoords $ \y row ->
      forIndexM_ row $ \x renderedCoord -> do
        let coord = renderedCoordState renderedCoord
        when (coordVisible coord && not (coordDimmed coord)) $
          drawCoord' x y renderedCoord

    -- Then draw visible but dimmed points.  This is performed under a single
    -- Cairo group for performance reasons.  (Having a group for each dimmed
    -- point is *really* slow with a board full of dimmed points.)
    pushGroup
    forIndexM_ renderedCoords $ \y row ->
      forIndexM_ row $ \x renderedCoord -> do
        let coord = renderedCoordState renderedCoord
        when (coordVisible coord && coordDimmed coord) $
          drawCoord' x y renderedCoord
    popGroupToSource
    paintWithAlpha dimmedPointOpacity

    -- Draw non-CoordState-based annotations.
    unless (null (boardLines board) && null (boardArrows board)) $ do
      setSourceRGB 0 0 0
      setLineWidth boardAnnotationLineWidth
      mapM_ (uncurry drawLine) $ boardLines board
      mapM_ (uncurry drawArrow) $ boardArrows board
  return ()

-- | Draws a single point on the board.
drawCoord :: BoardState
             -- ^ The board being drawn.
          -> Double
             -- ^ The pixel width of the grid in the board's interior.
          -> Double
             -- ^ The pixel width of the grid on the board's border.
          -> Int
             -- ^ The x-index of the point to be drawn.
          -> Int
             -- ^ The y-index of the point to be drawn.
          -> RenderedCoord
             -- ^ The point to be drawn.
          -> Render ()
drawCoord board gridWidth gridBorderWidth x y renderedCoord = do
  let x' = fromIntegral x
      y' = fromIntegral y
      coord = renderedCoordState renderedCoord
      variation = renderedCoordVariation renderedCoord
  -- Translate the grid so that we can draw the stone from (0,0) to (1,1).
  translate x' y'
  -- Draw the grid, stone (if present), and mark (if present).
  drawGrid board gridWidth gridBorderWidth x y
  F.mapM_ drawStone $ coordStone coord
  maybe (return ()) (drawMark $ coordStone coord) $ coordMark coord
  F.mapM_ drawVariation variation
  -- Restore the coordinate system for the next stone.
  translate (-x') (-y')

-- | Given a current tool, board, and hover, modifies a 'CoordState' to reflect
-- that the user is hovering over the point.  The effect depends on the tool.
-- The effect is to suggest what would happen if the point were clicked.
modifyCoordForHover :: Tool -> BoardState -> HoverState -> CoordState -> CoordState
modifyCoordForHover tool board hover coord = case tool of
  ToolPlay -> if hoverIsValidMove hover
              then coord { coordStone = Just (boardPlayerTurn board) }
              else coord
  ToolJump -> coord  -- TODO Hover for ToolJump.
  ToolScore -> coord  -- TODO Hover for ToolScore.
  ToolBlack -> coord { coordStone = Just (toolToColor tool) }
  ToolWhite -> coord { coordStone = Just (toolToColor tool) }
  ToolErase -> coord { coordStone = Nothing }
  ToolArrow -> coord  -- TODO Hover for ToolArrow.
  ToolMarkCircle -> toggleMark MarkCircle coord
  ToolLabel -> coord  -- TODO Hover for ToolLabel.
  ToolLine -> coord  -- TODO Hover for ToolLine.
  ToolMarkX -> toggleMark MarkX coord
  ToolMarkSelected -> toggleMark MarkSelected coord
  ToolMarkSquare -> toggleMark MarkSquare coord
  ToolMarkTriangle -> toggleMark MarkTriangle coord
  ToolVisible -> coord { coordVisible = not $ coordVisible coord }
  ToolDim -> coord { coordDimmed = not $ coordDimmed coord }

-- | Toggles a specific mark on a 'CoordState'.  If the @CoordState@ already has
-- that mark, then it is removed.  If the @CoordState@ has no mark or has
-- another mark, then the given mark is added to the @CoordState@, overwriting
-- an existing mark if there is one.
toggleMark :: Mark -> CoordState -> CoordState
toggleMark mark coord = let justMark = Just mark
                        in if coordMark coord == justMark
                           then coord { coordMark = Nothing }
                           else coord { coordMark = justMark }

-- | Draws the gridlines for a single point on the board.
drawGrid :: BoardState -> Double -> Double -> Int -> Int -> Render ()
drawGrid board gridWidth gridBorderWidth x y = do
  -- Draw the grid.
  let atLeft = x == 0
      atTop = y == 0
      atRight = x == boardWidth board - 1
      atBottom = y == boardHeight board - 1
      gridX0 = if atLeft then 0.5 else 0
      gridY0 = if atTop then 0.5 else 0
      gridX1 = if atRight then 0.5 else 1
      gridY1 = if atBottom then 0.5 else 1
  -- Temporarily disable antialiasing.  We want grid lines to be sharp.
  setAntialias AntialiasNone
  setSourceRGB 0 0 0
  setLineWidth $ if atTop || atBottom then gridBorderWidth else gridWidth
  moveTo gridX0 0.5
  lineTo gridX1 0.5
  stroke
  setLineWidth $ if atLeft || atRight then gridBorderWidth else gridWidth
  moveTo 0.5 gridY0
  lineTo 0.5 gridY1
  stroke
  setAntialias AntialiasDefault

-- | Draws a stone from @(0, 0)@ to @(1, 1)@ in user coordinates.
drawStone :: Color -> Render ()
drawStone color = do
  arc 0.5 0.5 (0.5 - stoneBorderThickness / 2) 0 (2 * pi)
  setRgb $ stoneColor color
  fillPreserve
  setLineWidth stoneBorderThickness
  setRgb $ stoneBorderColor color
  stroke

-- | Draws the given mark on the current point.  The color should be that of the
-- stone on the point, if there is one; it determines the color of the mark.
drawMark :: Maybe Color -> Mark -> Render ()
drawMark stone mark = do
  case mark of
    MarkCircle -> arc 0.5 0.5 0.25 0 (2 * pi)
    MarkTriangle -> do moveTo trianglePoint1X trianglePoint1Y
                       lineTo trianglePoint2X trianglePoint2Y
                       lineTo trianglePoint3X trianglePoint3Y
                       closePath
    MarkSquare -> do moveTo 0.25 0.25
                     lineTo 0.25 0.75
                     lineTo 0.75 0.75
                     lineTo 0.75 0.25
                     closePath
    MarkX -> do moveTo 0.25 0.25
                lineTo 0.75 0.75
                moveTo 0.25 0.75
                lineTo 0.75 0.25
    MarkSelected -> do moveTo 0.2 0.5
                       lineTo 0.5 0.8
                       lineTo 0.8 0.5
                       lineTo 0.5 0.2
                       closePath
  setRgb $ coordAnnotationStrokeColor stone
  setLineWidth 0.1
  stroke

-- The coordinates for inscribing a triangle within a unit circle centered about
-- @(0.5, 0.5)@, with radius @triangleRadius@.
triangleRadius = 0.3
trianglePoint1X = 0.5
trianglePoint1Y = 0.5 - triangleRadius
trianglePoint2X = 0.5 - triangleRadius * cos (pi / 6)
trianglePoint2Y = 0.5 + triangleRadius * 0.5 {-sin (pi / 6)-}
trianglePoint3X = 0.5 + triangleRadius * cos (pi / 6)
trianglePoint3Y = 0.5 + triangleRadius * 0.5 {-sin (pi / 6)-}

-- | Draws a line between the given board points.  Expects the context to be
-- already set up to draw.
drawLine :: Coord -> Coord -> Render ()
drawLine (fromIntegral -> x0, fromIntegral -> y0)
         (fromIntegral -> x1, fromIntegral -> y1) = do
  moveTo (x0 + 0.5) (y0 + 0.5)
  lineTo (x1 + 0.5) (y1 + 0.5)
  stroke

-- | Draws an arrow from the first point to the second point.  Expects the
-- context to be already set up to draw.
drawArrow :: Coord -> Coord -> Render ()
drawArrow (fromIntegral -> x0, fromIntegral -> y0)
          (fromIntegral -> x1, fromIntegral -> y1) = do
  let angle = atan ((y1 - y0) / (x1 - x0)) + if x0 <= x1 then 0 else pi
      len = sqrt ((y1 - y0)**2 + (x1 - x0)**2) - boardAnnotationLineWidth
      tx = x0 + 0.5
      ty = y0 + 0.5
  -- Set up user space so that we can draw the line from (0,0) to
  -- (0,lineLength).
  translate tx ty
  rotate angle
  moveTo 0 0
  lineTo len 0
  stroke
  moveTo len 0
  lineTo (len - boardAnnotationArrowPullback) boardAnnotationArrowWidth
  lineTo (len - boardAnnotationArrowPullback) (-boardAnnotationArrowWidth)
  closePath
  stroke
  rotate (-angle)
  translate (-tx) (-ty)

-- | Draws a small stone centered on the current coordinate being drawn, to
-- indicate that a variation is available where the given player plays a stone
-- here.  The drawn stone is of the given color.
drawVariation :: Color -> Render ()
drawVariation stone = do
  arc 0.5 0.5 stoneVariationRadius 0 (2 * pi)
  setRgb $ stoneColor stone
  fillPreserve
  setLineWidth stoneVariationBorderThickness
  setRgb $ stoneBorderColor stone
  stroke

type Rgb = (Double, Double, Double)

rgb :: Double -> Double -> Double -> Rgb
rgb = (,,)

rgb255 :: Double -> Double -> Double -> Rgb
rgb255 r g b = (r / 255, g / 255, b / 255)

setRgb :: Rgb -> Render ()
setRgb (r, g, b) = setSourceRGB r g b
