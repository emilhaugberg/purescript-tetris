module Tetris.Rotation.Helper where

import Config
import Prelude
import Tetris.Shape

type Z =
  { one   :: {x :: Number,  y :: Number}
  , two   :: {x :: Number,  y :: Number}
  , three :: {x :: Number,  y :: Number}
  , four  :: {x :: Number,  y :: Number}
  }

type SOM = {
  l      :: Z,
  z      :: Z,
  t      :: Z,
  s      :: Z,
  mirrl  :: Z,
  line   :: Z,
  square :: Z
}

rotationCoords :: Block Coordinate -> SOM
rotationCoords (Block a b c d) =
  { l: {one: {x: b.x, y: a.y}, two: {x: b.x, y: b.y - blockHeight}, three: {x: b.x, y: b.y - blockHeight}, four: {x: b.x, y: c.y}}
  , z: {one: {x: b.x, y: b.y}, two: {x: d.x, y: c.y - (blockHeight * 2.0)}, three: {x: c.x, y: b.y}, four: {x: b.x, y: c.y - (blockHeight * 2.0)}}
  , t: {one: {x: b.x, y: b.y}, two: {x: b.x, y: b.y - blockHeight}, three: {x: b.x, y: c.y}, four: {x: b.x, y: b.y - blockHeight}}
  , s: {one: {x: c.x, y: c.y}, two: {x: d.x, y: c.y - (blockHeight)}, three: {x: b.x, y: c.y}, four: {x: b.x, y: c.y - (blockHeight)}}
  , mirrl: {one: {x: b.x, y: b.y}, two: {x: c.x, y: c.y - blockHeight}, three: {x: c.x, y: c.y}, four: {x: b.x, y: c.y - blockHeight}}
  , line: {one: {x: c.x, y: c.y}, two: {x: c.x, y: c.y - (blockHeight * 2.0)}, three: {x: c.x - blockWidth, y: c.y}, four: {x: b.x, y: c.y - (blockHeight * 2.0)}}
  , square: {one: {x: b.x, y: b.y}, two: {x: c.x, y: c.y}, three: {x: c.x - blockWidth, y: b.y - blockHeight}, four: {x: b.x, y: c.y}}
  }
