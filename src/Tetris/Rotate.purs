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

nextRotation' :: Rotation -> Rotation
nextRotation' One   = Two
nextRotation' Two   = Three
nextRotation' Three = Four
nextRotation' Four  = One

rotationPointsFromShape :: Shape -> ShapesRotationPoints -> RotationPoints
rotationPointsFromShape L         srp = srp.l
rotationPointsFromShape Z         srp = srp.z
rotationPointsFromShape T         srp = srp.t
rotationPointsFromShape S         srp = srp.s
rotationPointsFromShape MirroredL srp = srp.mirrl
rotationPointsFromShape Line      srp = srp.line
rotationPointsFromShape Square    srp = srp.square

coordFromRotationAngle' :: RotationPoints -> Rotation -> { x :: Number, y :: Number }
coordFromRotationAngle' z One   = z.one
coordFromRotationAngle' z Two   = z.two
coordFromRotationAngle' z Three = z.three
coordFromRotationAngle' z Four  = z.four

checkOutOfBounds :: Number
                 -> ((Number -> Number) -> Coordinate -> Coordinate)
                 -> (Number -> Number -> Number)
                 -> (Number -> Coordinate -> Number)
                 -> Block Coordinate
                 -> Block Coordinate
checkOutOfBounds m mapF f1 f2 b = map (mapF f) b
  where
    f           = if   outOfBounds < 0.0 || outOfBounds >= m
                  then f1 (((abs outOfBounds) - m / blockWidth) * blockWidth)
                  else identity
    outOfBounds = foldl f2 m (blockToArr b)

cobX :: (Number -> Number) -> Coordinate -> Coordinate
cobX f coord = {x: f coord.x, y: coord.y}

cobY :: (Number -> Number) -> Coordinate -> Coordinate
cobY f coord = {x: coord.x, y: f coord.y}

moveOutOfBoundsRight  = checkOutOfBounds (canvasWidth  - blockWidth)  cobX (flip (-)) (\a b -> max a b.x)
moveOutOfBoundsBottom = checkOutOfBounds (canvasHeight - blockHeight) cobY (flip (-)) (\a b -> max a b.y)
moveOutOfBoundsLeft   = checkOutOfBounds 0.0                          cobX (+)        (\a b -> min a b.x)
moveOutOfBoundsTop    = checkOutOfBounds 0.0                          cobY (+)        (\a b -> min a b.y)

checkSides = moveOutOfBoundsTop <<< moveOutOfBoundsBottom <<< moveOutOfBoundsLeft <<< moveOutOfBoundsRight
