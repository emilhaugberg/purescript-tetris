module Config where

import Data.Tuple

-- TYPES
type X          = Number
type Y          = Number
type Coordinate = Tuple X Y

data Direction  = Horizontal | Vertical

-- GAME CONFIG
numHorizontalBlocks :: Int
numHorizontalBlocks = 10

numVerticalBlocks :: Int
numVerticalBlocks = 20

canvasHeight :: Number
canvasHeight = 800.0

canvasWidth :: Number
canvasWidth = 400.0
