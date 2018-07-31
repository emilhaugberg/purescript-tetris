module Tetris.Rotate.Helper where

import Config
import Prelude

import Data.Enum
import Data.Maybe

import Tetris.Shape
import Tetris.Types

nextRotation' :: Rotation -> Rotation
nextRotation' = fromMaybe One <<< succ

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

rotationCoords :: Block Coordinate -> ShapesRotationPoints
rotationCoords (Block a b c d) =
  { l:      {one: {x: b.x, y: a.y}, two: {x: b.x, y: b.y - blockHeight        }, three: {x: b.x             , y: b.y - blockHeight}, four: {x: b.x, y: c.y                      }}
  , z:      {one: {x: b.x, y: b.y}, two: {x: d.x, y: c.y - (blockHeight * 2.0)}, three: {x: c.x             , y: b.y              }, four: {x: b.x, y: c.y - (blockHeight * 2.0)}}
  , t:      {one: {x: b.x, y: b.y}, two: {x: b.x, y: b.y - blockHeight        }, three: {x: b.x             , y: c.y              }, four: {x: b.x, y: b.y -  blockHeight       }}
  , s:      {one: {x: c.x, y: c.y}, two: {x: d.x, y: c.y - (blockHeight)      }, three: {x: b.x             , y: c.y              }, four: {x: b.x, y: c.y -  blockHeight       }}
  , mirrl:  {one: {x: b.x, y: b.y}, two: {x: c.x, y: c.y - blockHeight        }, three: {x: c.x             , y: c.y              }, four: {x: b.x, y: c.y -  blockHeight       }}
  , line:   {one: {x: c.x, y: c.y}, two: {x: c.x, y: c.y - (blockHeight * 2.0)}, three: {x: c.x - blockWidth, y: c.y              }, four: {x: b.x, y: c.y - (blockHeight * 2.0)}}
  , square: {one: {x: b.x, y: b.y}, two: {x: c.x, y: c.y                      }, three: {x: c.x - blockWidth, y: b.y - blockHeight}, four: {x: b.x, y: c.y                      }}
  }

rotations :: Shape -> Transformation
rotations sh = case sh of
  Z         -> {one: Block (-4.0)   0.0  1.0 5.0, two: Block 0.0      1.0 (-1.0) (-2.0), three: Block (-4.0) 0.0   1.0    5.0 , four: Block   0.0    1.0 (-1.0) (-2.0)}
  S         -> {one: Block (-1.0)   1.0  0.0 4.0, two: Block (-4.0) (-1.0) 1.0     2.0 , three: Block (-1.0) 1.0   0.0    4.0 , four: Block (-4.0) (-1.0)  1.0    2.0}
  T         -> {one: Block (-4.0)   0.0  1.0 4.0, two: Block (-1.0)   0.0  1.0     2.0 , three: Block (-1.0) 0.0   1.0    5.0 , four: Block   0.0    1.0   2.0    5.0}
  L         -> {one: Block (-4.0)   0.0  4.0 5.0, two: Block 0.0      1.0  2.0   (-2.0), three: Block   6.0  2.0 (-1.0) (-2.0), four: Block   4.0    2.0   1.0    0.0}
  MirroredL -> {one: Block (-4.0) (-1.0) 0.0 4.0, two: Block (-4.0)   0.0  1.0     2.0 , three: Block (-1.0) 1.0   4.0    5.0 , four: Block   0.0    1.0   2.0    6.0}
  Line      -> {one: Block (-4.0)   0.0  4.0 8.0, two: Block 0.0      1.0  2.0     3.0 , three: Block (-4.0) 0.0   4.0    8.0 , four: Block   0.0    1.0   2.0    3.0}
  Square    -> {one: Block 0.0      1.0  4.0 5.0, two: Block 0.0      1.0  4.0     5.0 , three: Block   0.0  1.0   4.0    5.0 , four: Block   0.0    1.0   4.0    5.0}
