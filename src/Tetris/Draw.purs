module Tetris.Draw where

import Config
import Config as Config
import Prelude
import Effect (Effect, foreachE)

import Tetris.Shape as Tetris
import Tetris.Types as Tetris
import Tetris.Row as Tetris

import Data.Array (range)
import Data.Traversable (traverse)
import Data.FunctorWithIndex
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe
import Graphics.Canvas

import Effect.Console (log)

coordinateToRectangle :: Tetris.Coordinate -> Rectangle
coordinateToRectangle c =
  { x     : c.x
  , y     : c.y
  , width : Config.blockWidth
  , height: Config.blockHeight
  }

drawLine :: Tuple Tetris.Coordinate Tetris.Coordinate -> Context2D -> Effect Unit
drawLine points ctx = do
  beginPath ctx
  moveTo    ctx from.x from.y
  lineTo    ctx to.x   to.y
  stroke    ctx
  where
    from = fst points
    to   = snd points

drawLines :: Int -> Tetris.LineDirection -> Context2D -> Effect Unit
drawLines numL dir ctx = foreachE (range 0 numL) line
  where
    line                       = \i -> do drawLine (pos i dir) ctx
    startVertical   i          = {x: 0.0, y: (offset i Config.canvasHeight)}
    startHorizontal i          = {x: (offset i Config.canvasWidth), y: 0.0}
    endVertical     i          = {x: Config.canvasWidth, y: (offset i Config.canvasHeight)}
    endHorizontal   i          = {x: (offset i Config.canvasWidth), y: Config.canvasHeight}
    pos    i Tetris.Horizontal = Tuple (startHorizontal i) (endHorizontal i)
    pos    i Tetris.Vertical   = Tuple (startVertical i)   (endVertical i)
    offset i max               = (max  / (toNumber numL)) * (toNumber i)

drawGrid :: Int -> Int -> Context2D -> Effect Unit
drawGrid numH numV ctx = do
  drawLines numH Tetris.Horizontal ctx
  drawLines numV Tetris.Vertical   ctx

drawShape :: Tetris.Block Tetris.Coordinate -> Tetris.Shape -> Context2D -> Effect Unit
drawShape bc s ctx = drawRects bc
  where
    drawRects r = foreachE (Tetris.blockToArr bc) \i -> do
      let r = coordinateToRectangle i

      rect     ctx r
      setFillStyle ctx (Tetris.shapeToColor s)
      fillRect ctx r
      stroke   ctx

drawShapes :: Array Tetris.BlockShape -> Context2D -> Effect Unit
drawShapes arr ctx = foreachE arr \i -> do
  drawShape i.pos i.shape ctx

drawShapes' :: Array Tetris.BlockShape -> Context2D -> Effect Unit
drawShapes' arr ctx = void $ do
  let rl = fromMaybe (mapWithIndex (\i _ -> Tuple i Tetris.White) (range 0 200)) $ Tetris.toRowList arr

  foreachE rl (\t -> do
--    _ <- log $ show $ fst t
    let r = coordinateToRectangle $ Tetris.rowPosToCoordinate $ fst t
    _ <- log $ show $ Tetris.rowPosToCoordinate $ fst t
    rect ctx r
    setFillStyle ctx (show $ snd t)
    fillRect ctx r
    stroke ctx)
