module Config where

import Prelude
import Math

import Data.Int (toNumber)
import Data.Tuple

-- GAME CONFIG
numHorizontalBlocks :: Int
numHorizontalBlocks = 10

numVerticalBlocks   :: Int
numVerticalBlocks   = 20

canvasHeight        :: Number
canvasHeight        = 800.0

canvasWidth         :: Number
canvasWidth         = 400.0

blockWidth          :: Number
blockWidth          = canvasWidth / toNumber numHorizontalBlocks

blockHeight         :: Number
blockHeight         = canvasHeight / toNumber numVerticalBlocks

-- HELPER FUNCTIONS
roundToNearest :: Number -> Number -> Number
roundToNearest x nearest = round (x / nearest) * nearest
