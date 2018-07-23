module Main where

import Prelude
import Config as Config
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas
import Tetris.Shape as Tetris
import Tetris.Draw as Tetris

main :: Partial => Effect Unit
main = void $ do
  Just canvas <- getCanvasElementById "tetris-canvas"
  ctx         <- getContext2D canvas

  Tetris.drawGrid Config.numHorizontalBlocks Config.numVerticalBlocks ctx
  Tetris.drawShape Tetris.L ctx
