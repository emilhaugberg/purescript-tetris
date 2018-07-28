module Tetris.Rotate where

import Prelude
import Config

import Tetris.Shape
import Tetris.Rotation.Helper

type RotationPoint = Int
data Rotation      = One | Two | Three | Four

derive instance eqRotation :: Eq Rotation

rotation :: Shape -> Rotation -> Block Coordinate -> Block Coordinate
rotation sh rot bc = blocksToCoord x y coordNum
  where
    x        = coords.x
    y        = coords.y
    coordNum = rotationToCoords rot sh
    coords   = f3 bc sh rot

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

f1 :: Shape -> SOM -> RotationPoints
f1 L y = y.l
f1 Z y = y.z
f1 T y = y.t
f1 S y = y.s
f1 MirroredL y = y.mirrl
f1 Line y = y.line
f1 Square y = y.square

f2 :: RotationPoints -> Rotation -> { x :: Number, y :: Number }
f2 z One = z.one
f2 z Two = z.two
f2 z Three = z.three
f2 z Four = z.four

f3 :: Block Coordinate -> Shape -> Rotation -> { x :: Number, y :: Number }
f3 bc sh rot = x1
  where
    x1 = (flip f2) rot <<< f1 sh <<< rotationCoords $ bc
