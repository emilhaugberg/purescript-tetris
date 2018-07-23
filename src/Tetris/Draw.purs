module Tetris.Draw where

import Tetris.Shape as Tetris
import Config (Coordinate, Direction(..))
import Config as Config
import Prelude
import Effect (Effect, foreachE)
import Data.Array (range)
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Graphics.Canvas

coordinateToRectangle :: Coordinate -> Rectangle
coordinateToRectangle c =
  { x     : c.x
  , y     : c.y
  , width : Config.blockWidth
  , height: Config.blockHeight
  }

drawLine :: Coordinate -> Coordinate -> Context2D -> Effect Unit
drawLine from to ctx = do
  beginPath ctx
  moveTo    ctx from.x from.y
  lineTo    ctx to.x   to.y
  stroke    ctx

drawLines :: Int -> Direction -> Context2D -> Effect Unit
drawLines numL dir ctx = foreachE (range 0 numL) line
  where
    line                = \i -> do drawLine (fst $ pos i dir) (snd $ pos i dir) ctx
    startVertical   i   = {x: 0.0, y: (offset i dir)}
    startHorizontal i   = {x: (offset i dir), y: 0.0}
    endVertical     i   = {x: Config.canvasWidth, y: (offset i dir)}
    endHorizontal   i   = {x: (offset i dir), y: Config.canvasHeight}
    pos    i Horizontal = Tuple (startHorizontal i) (endHorizontal i)
    pos    i Vertical   = Tuple (startVertical i)   (endVertical i)
    offset i Horizontal = (Config.canvasWidth  / (toNumber numL)) * (toNumber i)
    offset i Vertical   = (Config.canvasHeight / (toNumber numL)) * (toNumber i)

drawGrid :: Int -> Int -> Context2D -> Effect Unit
drawGrid numH numV ctx = do
  drawLines numH Horizontal ctx
  drawLines numV Vertical   ctx

drawShape :: forall a. Tetris.Shape -> Context2D -> Effect Unit
drawShape s ctx = drawRects (Tetris.initialPos s)
  where
    drawRects r = foreachE (Tetris.blockToArr $ Tetris.initialPos s) \i -> do
      let r = coordinateToRectangle i

      rect     ctx r
      setFillStyle ctx (Tetris.shapeToColor s)
      fillRect ctx r
      stroke   ctx
