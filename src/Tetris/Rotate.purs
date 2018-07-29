module Tetris.Rotate where

import Prelude
import Config

import Data.Foldable
import Data.Ord

import Tetris.Shape
import Tetris.Types
import Tetris.Rotate.Helper

rotation :: Shape -> Rotation -> Block Coordinate -> Block Coordinate
rotation sh rot bc = checkSides $ blocksToCoord x y coordNum
  where
    x        = coords.x
    y        = coords.y
    coordNum = rotationToCoords rot sh
    coords   = coordFromRotationAngle bc sh rot

coordFromRotationAngle :: Block Coordinate -> Shape -> Rotation -> { x :: Number, y :: Number }
coordFromRotationAngle bc sh rot =
  (flip coordFromRotationAngle') rot <<< rotationPointsFromShape sh <<< rotationCoords $ bc

rotationToCoords :: Rotation -> Shape -> Block Number
rotationToCoords rt sh = case rt of
  One   -> tr.one
  Two   -> tr.two
  Three -> tr.three
  Four  -> tr.four
  where
    tr = rotations sh

nextRotation :: Rotation -> Int -> Rotation
nextRotation rt kc = if kc == 38 then nextRotation' rt else rt

checkOutOfBounds :: Number
                 -> (Number -> Number -> Boolean)
                 -> ((Number -> Number) -> Coordinate -> Coordinate)
                 -> (Number -> Number -> Number)
                 -> (Number -> Coordinate -> Number)
                 -> Block Coordinate
                 -> Block Coordinate
checkOutOfBounds m comp mapF f1 f2 b = map (mapF f) b
  where
    f   = if   comp max m
          then f1 ((((abs max) - m) / blockWidth) * blockWidth)
          else identity
    max = foldl f2 50.0 (blockToArr b)

cobX :: (Number -> Number) -> Coordinate -> Coordinate
cobX f coord = {x: f coord.x, y: coord.y}

cobY :: (Number -> Number) -> Coordinate -> Coordinate
cobY f coord = {x: coord.x, y: f coord.y}

checkOutOfBoundsRight  = checkOutOfBounds (canvasWidth - blockWidth)   (>=) cobX (flip (-)) (\a b -> max a b.x)
checkOutOfBoundsBottom = checkOutOfBounds (canvasHeight - blockHeight) (>=) cobY (flip (-)) (\a b -> max a b.y)
checkOutOfBoundsLeft   = checkOutOfBounds 0.0                          (<)  cobX  (+)       (\a b -> min a b.x)
checkOutOfBoundsTop    = checkOutOfBounds 0.0                          (<)  cobY  (+)       (\a b -> min a b.y)

checkSides = checkOutOfBoundsRight <<< checkOutOfBoundsBottom <<< checkOutOfBoundsTop <<< checkOutOfBoundsLeft
