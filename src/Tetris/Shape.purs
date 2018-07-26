module Tetris.Shape where

import Prelude
import Config
import Math

import Data.Tuple

data Block a = Block a a a a
data Shape   = Z | T | L | S | MirroredL | Line | Square

derive instance functorBlock :: Functor Block

initialPos :: Shape -> Block Coordinate
initialPos = blocksToCoord centerX 0.0 <<< initialPos'
  where
    centerX = (roundToNearest (canvasWidth / 2.0) blockWidth) - blockWidth

blockToArr :: forall a. Block a -> Array a
blockToArr (Block a b c d) = [a, b, c, d]

shapeToColor :: Shape -> String
shapeToColor = case _ of
   Z         -> "#00FFFF"
   T         -> "#0000FF"
   S         -> "#FFFF00"
   L         -> "#FFA500"
   MirroredL -> "#008000"
   Line      -> "#800080"
   Square    -> "#FF0000"

intToShape :: Int -> Shape
intToShape i = case i of
  1 -> Z
  2 -> T
  3 -> L
  4 -> S
  5 -> MirroredL
  6 -> Line
  7 -> Square
  _ -> Z

initialPos' :: Shape -> Block Number
initialPos' Z         = Block (-4.0)   0.0  1.0 5.0
initialPos' T         = Block (-4.0)   0.0  1.0 4.0
initialPos' S         = Block   0.0    1.0  5.0 6.0
initialPos' L         = Block (-4.0)   0.0  4.0 5.0
initialPos' MirroredL = Block (-4.0) (-1.0) 0.0 4.0
initialPos' Line      = Block (-4.0)   0.0  4.0 8.0
initialPos' Square    = Block   0.0    1.0  4.0 5.0

blocksToCoord :: Number -> Number -> Block Number -> Block Coordinate
blocksToCoord cx cy = map f
  where
    f     i = {x: x i, y: y i}
    x       = (+) cx <<< (*) blockWidth <<< floor <<< \i -> i / 4.0
    y     i = cy + (abs (i % 4.0)) * blockHeight
