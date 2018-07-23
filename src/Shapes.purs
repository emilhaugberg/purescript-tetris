module Shapes where

import Prelude
import Config
import Math ((%), floor)
import Data.Array
import Data.Int
import Data.Ord (abs)
import Data.Tuple (Tuple(..))

data Shape a     = Shape a a a a

instance functorShape :: Functor Shape where
  map f (Shape a b c d) = Shape (f a) (f b) (f c) (f d)

data TetrisShape = Z | T | L | S | MirroredL | Line | Square
data TetrisColor = Cyan | Blue | Orange | Yellow | Green | Purple | Red

shapeToColor :: TetrisShape -> TetrisColor
shapeToColor Z         = Cyan
shapeToColor T         = Blue
shapeToColor L         = Orange
shapeToColor S         = Yellow
shapeToColor MirroredL = Green
shapeToColor Line      = Purple
shapeToColor Square    = Red

colorToHex :: TetrisColor -> String
colorToHex Cyan   = "#00FFFF"
colorToHex Blue   = "#0000FF"
colorToHex Orange = "#FFA500"
colorToHex Yellow = "#FFFF00"
colorToHex Green  = "#008000"
colorToHex Purple = "#800080"
colorToHex Red    = "#FF0000"

shapeToHexColor :: TetrisShape -> String
shapeToHexColor = colorToHex <<< shapeToColor

initialPos :: TetrisShape -> Shape Coordinate
initialPos = posToCoord <<< initialPos'

initialPos' :: TetrisShape -> Shape Number
initialPos' Z = Shape (-4.0) 0.0 1.0 5.0
initialPos' T = Shape (-4.0) 0.0 1.0 4.0
initialPos' L = Shape (-4.0) 0.0 4.0 5.0
initialPos' _ = Shape test test test test
  where
    test = (-4.0)

posToCoord :: Shape Number -> Shape Coordinate
posToCoord = map f
  where
    f     i = Tuple (calcX i) (calcY i)
    calcX   = (+) middleX <<< (*) blockWidth <<< floor <<< \i -> i / 4.0
    calcY i = (abs (i % 4.0)) * blockHeight
    middleX = (roundToNearest (canvasWidth / 2.0) blockWidth) - blockWidth
