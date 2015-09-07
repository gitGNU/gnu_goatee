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

-- | A widget that renders an interactive Go board.
module Game.Goatee.Ui.Gtk.Goban (
  Goban,
  create,
  destroy,
  myWidget,
  ) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<), liftM, unless, void, when)
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Tree (drawTree, unfoldTree)
import Game.Goatee.Common
import Game.Goatee.Lib.Board hiding (isValidMove)
import Game.Goatee.Lib.Monad (
  AnyEvent (..),
  childAddedEvent,
  childDeletedEvent,
  goDown,
  goLeft,
  goRight,
  goToRoot,
  goUp,
  navigationEvent,
  propertiesModifiedEvent,
  )
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Graphics.Rendering.Cairo (
  Antialias (AntialiasDefault, AntialiasNone),
  Render,
  arc,
  closePath,
  deviceToUser,
  deviceToUserDistance,
  fill,
  fillPreserve,
  liftIO,
  lineTo,
  moveTo,
  paint,
  paintWithAlpha,
  popGroupToSource,
  pushGroup,
  rectangle,
  rotate,
  scale,
  setAntialias,
  setLineWidth,
  setSourceRGB,
  stroke,
  translate,
  userToDevice,
  userToDeviceDistance,
  )
import Graphics.UI.Gtk (
  DrawingArea,
  EventMask (ButtonPressMask, LeaveNotifyMask, PointerMotionMask),
  Modifier (Shift),
  MouseButton,
  Widget,
  buttonPressEvent, buttonReleaseEvent,
  drawingAreaNew,
  eventButton, eventCoordinates, eventKeyName, eventModifier,
  exposeEvent,
  keyPressEvent,
  leaveNotifyEvent,
  motionNotifyEvent,
  on,
  renderWithDrawable,
  toWidget,
  widgetAddEvents, widgetGetDrawWindow, widgetGetSize, widgetGrabFocus, widgetQueueDraw,
  widgetSetCanFocus,
  )
import System.Glib (glibToString)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | If false, then the up and down keys will move toward and away
-- from the game tree root, and left and right will move between
-- siblings.  If true, these are reversed.
useHorizontalKeyNavigation :: Bool
useHorizontalKeyNavigation = True

-- Key handler code below requires that these keys don't use modifiers.
keyNavActions :: UiCtrl go ui => Map String (ui -> IO ())
keyNavActions =
  Map.fromList $
  --map (fmap ((>> return ()) .))  -- Drop the booleans these actions return.
  map (fmap $ \action ui -> doUiGo ui $ void action)
      (if useHorizontalKeyNavigation
       then [ ("Up", goLeft)
            , ("Down", goRight)
            , ("Left", goUp)
            , ("Right", goDown 0)
            ]
       else [ ("Up", goUp)
            , ("Down", goDown 0)
            , ("Left", goLeft)
            , ("Right", goRight)
            ]) ++
  [ ("Home", flip doUiGo goToRoot)
  , ("End", flip doUiGo $ whileM (goDown 0) $ return ())
  , ("Page_Up", flip doUiGo $ void $ andM $ replicate 10 goUp)
  , ("Page_Down", flip doUiGo $ void $ andM $ replicate 10 $ goDown 0)
  ]

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

-- | The radius of star points.  Percentage of coordinate size, in @[0, 1]@.
starPointRadius :: Double
starPointRadius = 0.1

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
data Goban ui = Goban
  { myUi :: ui
  , myState :: ViewState
  , myWidget :: Widget
  , myDrawingArea :: DrawingArea
  , myModesChangedHandler :: IORef (Maybe Registration)
  }

instance UiCtrl go ui => UiView go ui (Goban ui) where
  viewName = const "Goban"
  viewCtrl = myUi
  viewState = myState
  viewUpdate = update

-- | Creates a 'Goban' for rendering Go boards of the given size.
create :: UiCtrl go ui => ui -> IO (Goban ui)
create ui = do
  drawingArea <- drawingAreaNew
  widgetSetCanFocus drawingArea True
  widgetAddEvents drawingArea [LeaveNotifyMask,
                               ButtonPressMask,
                               PointerMotionMask]

  state <- viewStateNew
  modesChangedHandler <- newIORef Nothing

  let me = Goban { myUi = ui
                 , myState = state
                 , myWidget = toWidget drawingArea
                 , myDrawingArea = drawingArea
                 , myModesChangedHandler = modesChangedHandler
                 }

  on drawingArea exposeEvent $ liftIO $ do
    drawBoard me
    return True

  on drawingArea motionNotifyEvent $ do
    mouseCoord <- fmap Just eventCoordinates
    liftIO $ handleMouseMove me mouseCoord
    return True

  on drawingArea leaveNotifyEvent $ do
    liftIO $ handleMouseMove me Nothing
    return True

  on drawingArea buttonPressEvent $ do
    mouseButton <- eventButton
    mouseCoord <- eventCoordinates
    liftIO $ handleMouseDown me mouseButton mouseCoord
    return True

  on drawingArea buttonReleaseEvent $ do
    mouseButton <- eventButton
    mouseCoord <- eventCoordinates
    liftIO $ handleMouseUp me mouseButton mouseCoord
    return True

  on drawingArea keyPressEvent $ do
    key <- glibToString <$> eventKeyName
    mods <- eventModifier
    let km = (key, mods)
    let maybeAction = Map.lookup key keyNavActions
    cond (return False)
      [(null mods && isJust maybeAction,
        liftIO $ fromJust maybeAction ui >> return True),

        -- Write a list of the current node's properties to the console.
       (km == ("t", []), liftIO $ do
           cursor <- readCursor ui
           print $ nodeProperties $ cursorNode cursor
           return True),

        -- Draw a tree rooted at the current node to the console.
       (km == ("T", [Shift]), liftIO $ do
           cursor <- readCursor ui
           putStrLn $ drawTree $ flip unfoldTree (cursorNode cursor) $ \node ->
             (show $ nodeProperties node, nodeChildren node)
           return True)]

  initialize me
  return me

initialize :: UiCtrl go ui => Goban ui -> IO ()
initialize me = do
  let ui = myUi me
  register me
    [ AnyEvent childAddedEvent
    , AnyEvent childDeletedEvent
    , AnyEvent navigationEvent
    , AnyEvent propertiesModifiedEvent
    ]
  writeIORef (myModesChangedHandler me) =<<
    liftM Just (registerModesChangedHandler ui "Goban" $ \_ _ -> update me)
  -- TODO Need to update the hover state's validity on cursor and tool (mode?)
  -- changes.
  --update me

destroy :: UiCtrl go ui => Goban ui -> IO ()
destroy me = do
  let ui = myUi me
  F.mapM_ (unregisterModesChangedHandler ui) =<< readIORef (myModesChangedHandler me)
  viewDestroy me

update :: UiCtrl go ui => Goban ui -> IO ()
update me = do
  fireGobanEvent me GobanInvalidate
  redraw me

-- | Notifies the active tool that a mouse button was pressed down over the
-- board.
handleMouseDown :: UiCtrl go ui => Goban ui -> MouseButton -> (Double, Double) -> IO ()
handleMouseDown me mouseButton mouseCoord = do
  widgetGrabFocus $ myDrawingArea me
  maybeCoord <- gtkToBoardCoordinates me mouseCoord
  fireGobanEvent me $ GobanClickStart mouseButton maybeCoord

-- | Notifies the active tool that a mouse click or drag that started with the
-- mouse being pressed down over the board has completed.
handleMouseUp :: UiCtrl go ui => Goban ui -> MouseButton -> (Double, Double) -> IO ()
handleMouseUp me mouseButton mouseCoord = do
  maybeCoord <- gtkToBoardCoordinates me mouseCoord
  fireGobanEvent me $ GobanClickFinish mouseButton maybeCoord

-- | notifies the active tool that the mouse has moved over the board.
handleMouseMove :: UiCtrl go ui => Goban ui -> Maybe (Double, Double) -> IO ()
handleMouseMove me maybeMouseCoord = do
  maybeCoord <- maybe (return Nothing) (gtkToBoardCoordinates me) maybeMouseCoord
  fireGobanEvent me $ GobanMouseMove maybeCoord

-- | Sends an event to the active tool.
fireGobanEvent :: UiCtrl go ui => Goban ui -> GobanEvent -> IO ()
fireGobanEvent me event = do
  AnyTool tool <- readTool $ myUi me
  doRedraw <- toolGobanHandleEvent tool event
  when doRedraw $ redraw me

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
gtkToBoardCoordinates :: UiCtrl go ui => Goban ui -> (Double, Double) -> IO (Maybe (Int, Int))
gtkToBoardCoordinates me (x, y) = do
  let ui = myUi me
      drawingArea = myDrawingArea me
  board <- cursorBoard <$> readCursor ui
  drawWindow <- widgetGetDrawWindow drawingArea
  changeCoords <- applyBoardCoordinates board drawingArea
  result@(bx, by) <- fmap (mapTuple floor) $
                     renderWithDrawable drawWindow $
                     changeCoords >> deviceToUser x y
  return $ if bx < 0 || bx >= boardWidth board ||
              by < 0 || by >= boardHeight board
           then Nothing
           else Just result

-- | Schedules the goban to repaint.
redraw :: UiCtrl go ui => Goban ui -> IO ()
redraw = widgetQueueDraw . myDrawingArea

-- | Fully redraws the board based on the current controller and UI state.
drawBoard :: UiCtrl go ui => Goban ui -> IO ()
drawBoard me = do
  let ui = myUi me
      drawingArea = myDrawingArea me
  cursor <- readCursor ui
  modes <- readModes ui
  AnyTool tool <- readTool ui

  board <- toolGobanRenderGetBoard tool cursor
  let variationMode = rootInfoVariationMode $ gameInfoRootInfo $ boardGameInfo $ cursorBoard cursor

      variations :: [(Coord, Color)]
      variations = if variationModeBoardMarkup variationMode
                   then cursorVariations (variationModeSource variationMode) cursor
                   else []

      -- Positions of stones that have been played at the current node.
      current :: [Coord]
      current = if uiHighlightCurrentMovesMode modes
                then concatMap (\prop -> case prop of
                                   B (Just xy) -> [xy]
                                   W (Just xy) -> [xy]
                                   _ -> []) $
                     cursorProperties cursor
                else []

      -- | Performs processing at the individual coord level based on UI state.
      preprocessCoord :: CoordState -> CoordState
      preprocessCoord =
        let applyStoneViewMode = case uiViewStonesMode modes of
              ViewStonesRegularMode -> id
              ViewStonesOneColorMode -> coerceStone $ uiViewStonesOneColorModeColor modes
              ViewStonesBlindMode -> setStone Nothing
        in applyStoneViewMode

      -- | Replaces an existing stone of color opposite to the one given with a
      -- stone of the given color.
      coerceStone :: Color -> CoordState -> CoordState
      coerceStone color state = if coordStone state == Just (cnot color)
                                then state { coordStone = Just color }
                                else state

      -- | Replaces a coordinate's stone.
      setStone :: Maybe Color -> CoordState -> CoordState
      setStone color state = if coordStone state == color
                             then state
                             else state { coordStone = color }

  -- The state of the board's points, with all data for rendering.
  renderedCoords <-
    toolGobanRenderModifyCoords tool board $
    -- Add current moves.
    (flip .)
    foldr (\(x, y) grid ->
            listUpdate (flip listUpdate x $
                        \renderedCoord -> renderedCoord { renderedCoordCurrent = True })
                        y
                        grid)
          current $
    -- Add variations.
    foldr (\((x, y), color) grid ->
            listUpdate (flip listUpdate x $
                        \renderedCoord -> renderedCoord { renderedCoordVariation = Just color })
                        y
                        grid)
          (map (map $ (\state -> RenderedCoord state False Nothing) . preprocessCoord) $
           boardCoordStates board)
          variations

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
      mapM_ (uncurry drawLine . lineToPair) $ boardLines board
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
      current = renderedCoordCurrent renderedCoord
      variation = renderedCoordVariation renderedCoord
  -- Translate the grid so that we can draw the stone from (0,0) to (1,1).
  translate x' y'
  -- Draw the grid, stone or star (if present), and mark (if present).
  drawGrid board gridWidth gridBorderWidth x y
  maybe (when (coordStar coord) drawStar) drawStone $ coordStone coord
  maybe (return ()) (drawMark $ coordStone coord) $ coordMark coord
  case (current, variation) of
    -- With @VariationMode ShowChildVariations True@, this is the case of an
    -- immediately recaptured ko.  With @ShowCurrentVariations@ and a valid SGF
    -- this case shouldn't happen.
    (True, Just variation') -> do drawCurrent True
                                  drawVariation variation' True
    (True, _) -> drawCurrent False
    (_, Just variation') -> drawVariation variation' False
    _ -> return ()
  -- Restore the coordinate system for the next stone.
  translate (-x') (-y')

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
  (cx, cy) <- roundToPixels 0.5 0.5
  -- Temporarily disable antialiasing.  We want grid lines to be sharp.
  setAntialias AntialiasNone
  setSourceRGB 0 0 0
  setLineWidth $ if atTop || atBottom then gridBorderWidth else gridWidth
  moveTo gridX0 cy
  lineTo gridX1 cy
  stroke
  setLineWidth $ if atLeft || atRight then gridBorderWidth else gridWidth
  moveTo cx gridY0
  lineTo cx gridY1
  stroke
  setAntialias AntialiasDefault

-- | Draws a stone from @(0, 0)@ to @(1, 1)@ in user coordinates.
drawStone :: Color -> Render ()
drawStone color = do
  arc 0.5 0.5 (0.5 - stoneBorderThickness / 2) 0 pi_2
  setRgb $ stoneColor color
  fillPreserve
  setLineWidth stoneBorderThickness
  setRgb $ stoneBorderColor color
  stroke

-- | Draws a dot to indicate that the current point is a star point.
drawStar :: Render ()
drawStar = do
  setSourceRGB 0 0 0
  -- This seems to be a decent point to transition from an antialiased star to
  -- an aliased star (well, box), balancing transitioning too early (having a
  -- jump in size) with too late (and having ugly antialiased bouncing star
  -- points for a range).
  let minRadiusOnScreen = 1.8
  (radiusOnScreen, _) <- userToDeviceDistance starPointRadius 0
  (cx, cy) <- roundToPixels 0.5 0.5
  if radiusOnScreen >= minRadiusOnScreen
    then do arc cx cy starPointRadius 0 pi_2
            fill
    else do setAntialias AntialiasNone
            (pixel, _) <- deviceToUserDistance 1 0
            rectangle (cx - 2 * pixel) (cy - 2 * pixel) (3 * pixel) (3 * pixel)
            fill
            setAntialias AntialiasDefault

-- | Draws the given mark on the current point.  The color should be that of the
-- stone on the point, if there is one; it determines the color of the mark.
drawMark :: Maybe Color -> Mark -> Render ()
drawMark stone mark = do
  case mark of
    MarkCircle -> arc 0.5 0.5 0.25 0 pi_2
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
triangleRadius, trianglePoint1X, trianglePoint1Y, trianglePoint2X, trianglePoint2Y :: Double
trianglePoint3X, trianglePoint3Y :: Double
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

-- | Draws a dot via 'drawSmallDot' to indicate that a variation is available
-- where the given player plays a stone here.  The dot is of the given color.
-- If the boolean is true, then a semicircle is drawn that will not overlap with
-- a similar semicircle drawn by 'drawCurrent'.
drawVariation :: Color -> Bool -> Render ()
drawVariation stone half =
  let angle0 = if half then pi_1_75 else 0
      angle1 = if half then pi_0_75 else pi_2
  in drawSmallDot (stoneColor stone) (stoneBorderColor stone) angle0 angle1

-- | Draws a dot via 'drawSmallDot' to indicate that the current coordinate was
-- played on in the current node.  If the boolean is true, then a semicircle is
-- drawn that will not overlap with a similar semicircle drawn by
-- 'drawVariation'.
drawCurrent :: Bool -> Render ()
drawCurrent half =
  let angle0 = if half then pi_0_75 else 0
      angle1 = if half then pi_1_75 else pi_2
  in drawSmallDot (0,0,1) (0,0,0) angle0 angle1

-- | Draws a small filled arc centered on the current coordinate being drawn.
drawSmallDot :: Rgb -> Rgb -> Double -> Double -> Render ()
drawSmallDot fill border angle0 angle1 = do
  arc 0.5 0.5 stoneVariationRadius angle0 angle1
  setRgb fill
  fillPreserve
  setLineWidth stoneVariationBorderThickness
  setRgb border
  stroke

roundToPixels :: Double -> Double -> Render (Double, Double)
roundToPixels =
  (uncurry deviceToUser . mapTuple (fromIntegral . (round :: Double -> Int)) <=<) .
  userToDevice

type Rgb = (Double, Double, Double)

rgb :: Double -> Double -> Double -> Rgb
rgb = (,,)

rgb255 :: Double -> Double -> Double -> Rgb
rgb255 r g b = (r / 255, g / 255, b / 255)

setRgb :: Rgb -> Render ()
setRgb (r, g, b) = setSourceRGB r g b

pi_0_75, pi_1_75, pi_2 :: Floating a => a
pi_0_75 = pi * 0.75
pi_1_75 = pi * 1.75
pi_2 = pi * 2
