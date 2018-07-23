module Main where

import Prelude
import Config as Config

import Data.Maybe (Maybe(..))
import Data.Traversable

import Effect (Effect)
import Effect.Console (log)
import Effect.Ref
import Effect.Timer

import Graphics.Canvas

import Tetris.Shape as Tetris
import Tetris.Draw  as Tetris

updatePos :: Tetris.Block Config.Coordinate -> Tetris.Block Config.Coordinate
updatePos = map \p -> {x: p.x, y: p.y + Config.blockHeight}

main :: Partial => Effect Unit
main = void  do
  Just canvas <- getCanvasElementById "tetris-canvas"
  ctx         <- getContext2D canvas
  shape       <- new Tetris.L
  pos         <- new $ Tetris.initialPos Tetris.L

  setInterval 500 $ void do
    clearRect ctx {x: 0.0, y: 0.0, width: Config.canvasWidth, height: Config.canvasHeight}

    Tetris.drawGrid  Config.numHorizontalBlocks Config.numVerticalBlocks ctx

    s <- read shape
    p <- read pos
    _ <- modify updatePos pos

    Tetris.drawShape p s ctx
