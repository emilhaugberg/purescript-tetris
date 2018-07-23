module Tetris.Shape where

import Prelude
import Config
import Math

import Data.Tuple

data Block a = Block a a a a
data Shape   = Z | T | L | S | MirroredL | Line | Square

derive instance functorBlock :: Functor Block

initialPos :: Shape -> Block Coordinate
initialPos = blocksToCoord <<< initialPos'

blockToArr :: forall a. Block a -> Array a
blockToArr (Block a b c d) = [a, b, c, d]

shapeToColor :: Shape -> String
shapeToColor = case _ of
   Z         -> "#00FFFF"
   T         -> "#0000FF"
   L         -> "#FFA500"
   S         -> "#FFFF00"
   MirroredL -> "#008000"
   Line      -> "#800080"
   Square    -> "#FF0000"

initialPos' :: Shape -> Block Number
initialPos' Z = Block (-4.0) 0.0 1.0 5.0
initialPos' T = Block (-4.0) 0.0 1.0 4.0
initialPos' L = Block (-4.0) 0.0 4.0 5.0
initialPos' _ = Block test test test test
  where
    test = (-4.0)

blocksToCoord :: Block Number -> Block Coordinate
blocksToCoord = map f
  where
    f     i = {x: x i, y: y i}
    x       = (+) centerX <<< (*) blockWidth <<< floor <<< \i -> i / 4.0
    y     i = (abs (i % 4.0)) * blockHeight
    centerX = (roundToNearest (canvasWidth / 2.0) blockWidth) - blockWidth
