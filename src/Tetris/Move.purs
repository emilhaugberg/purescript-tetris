module Tetris.Move where

import Prelude
import Config as C

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
moveBlocks' dir = map \c -> case dir of
  Left  -> {x: c.x - C.blockWidth, y: c.y}
  Right -> {x: c.x + C.blockWidth, y: c.y}
  Down  -> {x: c.x, y: c.y + C.blockHeight}

moveBlocks :: KeyCode -> Block C.Coordinate -> Block C.Coordinate
moveBlocks kc b = case keyCodeToDirection kc of
  Just dir -> moveBlocks' dir b
  Nothing  -> b
