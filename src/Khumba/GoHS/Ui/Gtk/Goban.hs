module Khumba.GoHS.Ui.Gtk.Goban ( Goban
                                , create
                                , myDrawingArea
                                ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (Color, Cursor)
import Khumba.GoHS.Common (mapTuple)
import Khumba.GoHS.Sgf
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

-- | A GTK widget that renders a Go board.
--
-- @ui@ should be an instance of 'UiCtrl'.
data Goban ui = Goban { myUi :: UiRef ui
                      , myDrawingArea :: DrawingArea
                      }

instance UiCtrl ui => UiView (Goban ui) where
  viewCursorChanged goban cursor = widgetQueueDraw $ myDrawingArea goban

-- | Creates a 'Goban' for rendering Go boards of the given size.
create :: UiCtrl ui => UiRef ui -> IO (Goban ui)
create uiRef = do
  let clickHandler = boardClickHandler uiRef

  drawingArea <- drawingAreaNew
  -- TODO Enable mouse events for the DrawingArea as mentioned in the docs for
  -- Graphics.UI.Gtk.Misc.DrawingArea.  Also handle configureEvent (resizes)?
  on drawingArea exposeEvent $ liftIO $ do
    cursor <- readCursor =<< readUiRef uiRef
    drawBoard uiRef drawingArea
    return True

  return Goban { myUi = uiRef
               , myDrawingArea = drawingArea
               }

boardClickHandler :: UiCtrl a => UiRef a -> Int -> Int -> IO ()
boardClickHandler uiRef x y = do
  ui <- readUiRef uiRef
  playAt ui (x, y)

drawBoard :: UiCtrl ui => UiRef ui -> DrawingArea -> IO ()
drawBoard uiRef drawingArea = do
  ui <- readUiRef uiRef
  cursor <- readCursor ui

  (canvasWidth, canvasHeight) <- return . mapTuple fromIntegral =<< widgetGetSize drawingArea
  let board = cursorBoard cursor
      cols = fromIntegral $ boardWidth board
      rows = fromIntegral $ boardHeight board
      maxStoneWidth = canvasWidth / cols
      maxStoneHeight = canvasHeight / rows
      maxStoneLength = min maxStoneWidth maxStoneHeight

  drawWindow <- widgetGetDrawWindow drawingArea
  renderWithDrawable drawWindow $ do
    -- Set user coordinates so that the top-left stone occupies the rectangle
    -- from (0,0) to (1,1).
    when (canvasWidth > canvasHeight) $ translate ((canvasWidth - canvasHeight) / 2) 0
    when (canvasHeight > canvasWidth) $ translate 0 ((canvasHeight - canvasWidth) / 2)
    scale maxStoneLength maxStoneLength

    -- Fill the background a nice woody shade.
    setRgb boardBgColor
    paint

    setSourceRGB 0 0 0
    rectangle 0.5 0.5 (cols - 1) (rows - 1)
    gridLineWidth <- fmap fst $ deviceToUserDistance 1 0
    setLineWidth gridLineWidth
    stroke

    sequence_ $ flip mapBoardCoords board $
      drawCoord board gridLineWidth (gridLineWidth * 2)
  return ()

drawCoord :: BoardState -> Double -> Double -> Int -> Int -> CoordState -> Render ()
drawCoord board gridWidth gridBorderWidth x y coord = do
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
        case coordStone coord of
          Just color -> drawStone x' y' color
          Nothing -> return ()

  case coordVisibility coord of
    CoordInvisible -> return ()
    -- TODO CoordDimmed
    CoordVisible -> draw

drawStone :: Double -> Double -> Color -> Render ()
drawStone x y color = do
  arc (x + 0.5) (y + 0.5) (0.5 - stoneBorderThickness / 2) 0 (2 * pi)
  setRgb $ stoneColor color
  fillPreserve
  setLineWidth stoneBorderThickness
  setRgb $ stoneBorderColor color
  stroke

type Rgb = (Double, Double, Double)

rgb :: Double -> Double -> Double -> Rgb
rgb = (,,)

rgb255 :: Double -> Double -> Double -> Rgb
rgb255 r g b = (r / 255, g / 255, b / 255)

setRgb :: Rgb -> Render ()
setRgb (r, g, b) = setSourceRGB r g b
