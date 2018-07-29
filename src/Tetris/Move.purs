module Tetris.Move where

import Prelude
import Config as C

import Data.Foldable
import Data.Maybe

import Tetris.Shape
import Tetris.Rotate
import Tetris.Types

keyCodeToAction :: KeyCode -> Maybe Action
keyCodeToAction kc = case kc of
  37 -> Just (Move Left)
  39 -> Just (Move Right)
  40 -> Just (Move Down)
  38 -> Just Rotate
  _  -> Nothing

moveBlocks :: Direction -> Block Coordinate -> Block Coordinate
moveBlocks dir bc = map f bc
  where
    f c = case dir of
      Left  -> mv Left  {x: c.x - C.blockWidth, y: c.y}                 c
      Right -> mv Right {x: c.x + C.blockWidth, y: c.y}                 c
      Down  -> mv Down  {x: c.x,                y: c.y + C.blockHeight} c

    mv dir coord c = if movePossible dir bc then coord else c

nextCoord :: KeyCode -> Shape -> Rotation -> Block Coordinate -> Block Coordinate
nextCoord kc sh rt b = case keyCodeToAction kc of
  Just (Move dir) -> moveBlocks dir b
  Just (Rotate)   -> rotation sh rt b
  _               -> b

movePossible :: Direction -> Block Coordinate -> Boolean
movePossible dir bc = foldl f true $ blockToArr bc
  where
    f = case dir of
      Left  -> \b coord -> b && (coord.x > 0.0)
      Right -> \b coord -> b && (coord.x + C.blockWidth < C.canvasWidth)
      Down  -> \b coord -> b && (coord.y + C.blockHeight < C.canvasHeight)
