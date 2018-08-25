module Tetris.Types where

import Prelude
import Data.Enum
import Data.Ord
import Data.Maybe

-- ROTATION
data Rotation       = One | Two | Three | Four
derive instance eqRotation   :: Eq Rotation
derive instance ordRotation  :: Ord Rotation

instance enumRotation :: Enum Rotation where
  succ One   = Just Two
  succ Two   = Just Three
  succ Three = Just Four
  succ Four  = Nothing
  pred One   = Nothing
  pred Two   = Just One
  pred Three = Just Two
  pred Four  = Just Three

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

-- SHAPES
data Block a        = Block a a a a
derive instance functorBlock :: Functor Block

data Shape          = Z | T | L | S | MirroredL | Line | Square

type BlockShape           =
  { shape    :: Shape
  , pos      :: Block Coordinate
  , rotation :: Rotation
  }

-- MOVES/ACTIONS
data Action         = Move Direction | Rotate | Pause

data Direction      = Left | Right | Down

data LineDirection  = Horizontal | Vertical


type KeyCode              = Int


type State                 =
  { current :: BlockShape
  , previous:: Array BlockShape
  }

type X                     = Number
type Y                     = Number
type Coordinate            = {x :: X, y :: Y}

-- ROWS
data Color = Cyan | Blue | Orange | Yellow | Green | Purple | Red | White

instance showColor :: Show Color where
  show Cyan   = "#00FFFF"
  show Blue   = "#0000FF"
  show Orange = "#FFFF00"
  show Yellow = "#FFA500"
  show Green  = "#008000"
  show Purple = "#800080"
  show Red    = "#FF0000"
  show White  = "#FFFFFF"

data RB = RB Color | Empty

type RowList = Array RB
newtype Row = Row RowList
