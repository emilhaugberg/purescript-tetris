module Main where

import Prelude
import Draw as Draw
import Config as Config
import Shapes
import Data.Maybe (Maybe(..))
import Data.Int
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas

main :: (Partial) => Effect Unit
main = void $ do
  Just canvas <- getCanvasElementById "tetris-canvas"
  ctx         <- getContext2D canvas

  Draw.drawGrid Config.numHorizontalBlocks Config.numVerticalBlocks ctx
  Draw.drawShape L ctx
