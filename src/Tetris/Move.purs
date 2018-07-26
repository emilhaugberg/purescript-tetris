module Tetris.Move where

import Prelude
import Config as C

import Data.Foldable
import Data.Maybe

import Tetris.Shape
import Tetris.Rotate

data Action    = Move Direction | Rotate
data Direction = Left | Right | Down
type KeyCode   = Int

keyCodeToAction :: KeyCode -> Maybe Action
keyCodeToAction kc = case kc of
  37 -> Just (Move Left)
  39 -> Just (Move Right)
  40 -> Just (Move Down)
  38 -> Just Rotate
  _  -> Nothing

moveBlocks :: Direction -> Block C.Coordinate -> Block C.Coordinate
moveBlocks dir bc = map f bc
  where
    f c = case dir of
      Left  -> if movePossible Left  bc then {x: c.x - C.blockWidth, y: c.y} else c
      Right -> if movePossible Right bc then {x: c.x + C.blockWidth, y: c.y} else c
      Down  -> if movePossible Down  bc then {x: c.x, y: c.y + C.blockHeight} else c

nextCoord :: KeyCode -> Shape -> Rotation -> Block C.Coordinate -> Block C.Coordinate
nextCoord kc sh rt b = case keyCodeToAction kc of
  Just (Move dir) -> moveBlocks dir b
  Just (Rotate)   -> rotation sh rt b
  _               -> b

movePossible :: Direction -> Block C.Coordinate -> Boolean
movePossible dir bc = foldl f true $ blockToArr bc
  where
    f = case dir of
      Left  -> \b coord -> b && (coord.x > 0.0)
      Right -> \b coord -> b && (coord.x + C.blockWidth < C.canvasWidth)
      Down  -> \b coord -> b && (coord.y + C.blockHeight < C.canvasHeight)
