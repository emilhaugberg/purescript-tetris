module Tetris.Rotate where

import Prelude
import Config

import Tetris.Shape

type Transformation =
  { one   :: Block Number
  , two   :: Block Number
  , three :: Block Number
  , four  :: Block Number
  }

type RotationPoint = Int
data Rotation      = One | Two | Three | Four

derive instance eqRotation :: Eq Rotation

rotation :: Shape -> Rotation -> Block Coordinate -> Block Coordinate
rotation sh rot bc = blocksToCoord x y coordNum
  where
    x        = startX sh rot bc
    y        = startY sh rot bc
    coordNum = rotationToCoords rot sh

startX :: Shape -> Rotation -> Block Coordinate -> Number
startX sh rt (Block a b c d) = case sh of
  Square -> a.x
  Line   -> l rt (Block a b c d)
  _      -> a.x

l :: Rotation -> Block Coordinate -> Number
l One (Block a b c d)   = b.x
l Two (Block a b c d)   = c.x
l Three (Block a b c d) = c.x - blockWidth
l Four (Block a b c d)  = b.x

startY :: Shape -> Rotation -> Block Coordinate ->Number
startY sh rt (Block a b c d) = case sh of
  Square -> a.y
  Line   -> if rt == One || rt == Three then b.y else a.y
  _      -> a.y

rotations :: Shape -> Transformation
rotations sh = case sh of
  Square -> {one: initSq  , two: initSq , three: initSq   , four: initSq }
  Line   -> {one: initLine, two: sndLine, three: initLine , four: sndLine}
  _      -> {one: initSq  , two: initSq , three: initSq   , four: initSq }

  where
    initSq   = initialPos' Square
    initLine = initialPos' Line
    sndLine  = Block 0.0 1.0 2.0 3.0

rotationToCoords :: Rotation -> Shape -> Block Number
rotationToCoords rt sh = case rt of
  One   -> tr.one
  Two   -> tr.two
  Three -> tr.three
  Four  -> tr.four

  where
    tr = rotations sh

nextRotation :: Rotation -> Rotation
nextRotation One   = Two
nextRotation Two   = Three
nextRotation Three = Four
nextRotation Four  = One

rotationPoint :: Shape -> RotationPoint
rotationPoint sh = case sh of
  Square -> 0
  T      -> 1
  Line   -> 1
  _      -> 3
