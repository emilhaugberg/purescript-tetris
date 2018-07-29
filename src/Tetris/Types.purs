module Tetris.Types where

import Prelude
import Data.Ord

data Rotation       = One | Two | Three | Four
data Shape          = Z | T | L | S | MirroredL | Line | Square
data Action         = Move Direction | Rotate
data Direction      = Left | Right | Down
data LineDirection  = Horizontal | Vertical
data Block a        = Block a a a a

derive instance eqRotation   :: Eq Rotation
derive instance functorBlock :: Functor Block

type RotationPoint        = Int

type RotationPoints       =
  { one   :: {x :: Number,  y :: Number}
  , two   :: {x :: Number,  y :: Number}
  , three :: {x :: Number,  y :: Number}
  , four  :: {x :: Number,  y :: Number}
  }

type ShapesRotationPoints =
 { l      :: RotationPoints
 , z      :: RotationPoints
 , t      :: RotationPoints
 , s      :: RotationPoints
 , mirrl  :: RotationPoints
 , line   :: RotationPoints
 , square :: RotationPoints
}

type Transformation       =
  { one   :: Block Number
  , two   :: Block Number
  , three :: Block Number
  , four  :: Block Number
  }

type KeyCode              = Int

type BlockShape           =
  { shape    :: Shape
  , pos      :: Block Coordinate
  , rotation :: Rotation
  }

type State                 =
  { current :: BlockShape
  , previous:: Array BlockShape
  }

type X                     = Number
type Y                     = Number
type Coordinate            = {x :: X, y :: Y}
