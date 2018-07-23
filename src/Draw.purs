module Draw where

import Config (Coordinate, Direction(..))
import Config as Config
import Prelude
import Shapes
import Effect (Effect, foreachE)
import Data.Array (range)
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Graphics.Canvas

coordinateToRectangle :: Coordinate -> Rectangle
coordinateToRectangle c =
  { x     : fst c
  , y     : snd c
  , width : Config.blockWidth
  , height: Config.blockHeight
  }

drawLine :: Coordinate -> Coordinate -> Context2D -> Effect Unit
drawLine from to ctx = do
  beginPath ctx
  moveTo    ctx (fst from) (snd from)
  lineTo    ctx (fst to)   (snd to)
  stroke    ctx

drawLines :: Int -> Direction -> Context2D -> Effect Unit
drawLines numL dir ctx = foreachE (range 0 numL) line
  where
    line                = \i -> do drawLine (fst $ pos i dir) (snd $ pos i dir) ctx
    startVertical   i   = Tuple 0.0 (offset i dir)
    startHorizontal i   = Tuple (offset i dir) 0.0
    endVertical     i   = Tuple Config.canvasWidth (offset i dir)
    endHorizontal   i   = Tuple (offset i dir) Config.canvasHeight
    pos    i Horizontal = Tuple (startHorizontal i) (endHorizontal i)
    pos    i Vertical   = Tuple (startVertical i)   (endVertical i)
    offset i Horizontal = (Config.canvasWidth  / (toNumber numL)) * (toNumber i)
    offset i Vertical   = (Config.canvasHeight / (toNumber numL)) * (toNumber i)

drawGrid :: Int -> Int -> Context2D -> Effect Unit
drawGrid numH numV ctx = do
  drawLines numH Horizontal ctx
  drawLines numV Vertical   ctx

drawShape :: forall a. TetrisShape -> Context2D -> Effect Unit
drawShape s ctx = drawShape' (initialPos s)
  where
    -- rect = coordinateToRectangle $ initialPos s
    drawRect r = do
      rect     ctx r
      fillRect ctx r
      stroke   ctx

    drawShape' (Shape a b c d) = do
      drawRect $ coordinateToRectangle a
      drawRect $ coordinateToRectangle b
      drawRect $ coordinateToRectangle c
      drawRect $ coordinateToRectangle d
