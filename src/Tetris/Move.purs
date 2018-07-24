module Tetris.Move where

import Prelude
import Config as C

import Data.Foldable
import Data.Maybe

import Tetris.Shape

data Direction = Left | Right | Down
type KeyCode = Int

keyCodeToDirection :: KeyCode -> Maybe Direction
keyCodeToDirection kc = case kc of
  37 -> Just Left
  39 -> Just Right
  40 -> Just Down
  _  -> Nothing

moveBlocks' :: Direction -> Block C.Coordinate -> Block C.Coordinate
moveBlocks' dir bc = map f bc
  where
    f c = case dir of
      Left  -> if movePossible Left  bc then {x: c.x - C.blockWidth, y: c.y} else c
      Right -> if movePossible Right bc then {x: c.x + C.blockWidth, y: c.y} else c
      Down  -> if movePossible Down  bc then {x: c.x, y: c.y + C.blockHeight} else c

moveBlocks :: KeyCode -> Block C.Coordinate -> Block C.Coordinate
moveBlocks kc b = case keyCodeToDirection kc of
  Just dir -> moveBlocks' dir b
  Nothing  -> b

movePossible :: Direction -> Block C.Coordinate -> Boolean
movePossible dir bc = foldl f true $ blockToArr bc
  where
    f = case dir of
      Left  -> \b coord -> b && (coord.x > 0.0)
      Right -> \b coord -> b && (coord.x + C.blockWidth < C.canvasWidth)
      Down  -> \b coord -> b && (coord.y + C.blockHeight < C.canvasHeight)
