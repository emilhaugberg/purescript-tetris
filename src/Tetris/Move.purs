module Tetris.Move where

import Prelude
import Config as C

import Data.Array
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
  32 -> Just Pause
  _  -> Nothing

moveBlocks :: Direction -> Block Coordinate -> Block Coordinate
moveBlocks dir bc = map f bc
  where
    f c = case dir of
      Left  -> if movePossible Left  bc then {x: c.x - C.blockWidth, y: c.y} else c
      Right -> if movePossible Right bc then {x: c.x + C.blockWidth, y: c.y} else c
      Down  -> if movePossible Down  bc then {x: c.x, y: c.y + C.blockHeight} else c

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

checkIfCollided :: BlockShape -> Array BlockShape -> Boolean
checkIfCollided bs prev = foldl (||) false (concat f1) || foldl (\a b -> b.y >= (C.canvasHeight - C.blockHeight) || a) false (blockToArr bs.pos)
  where
    f1                        = map (\a -> map (\b -> hasCollided a b) bsArrs) (blockToArr bs.pos)
    bsArrs                    = concat $ map (blockToArr <<< (\a -> a.pos)) prev
    hasCollided coord1 coord2 = coord1.x == coord2.x && coord1.y == coord2.y - C.blockHeight
