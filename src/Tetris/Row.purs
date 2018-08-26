module Tetris.Row where

import Config
import Prelude
import Data.Array (range, uncons, updateAt)
import Data.Traversable
import Data.Tuple
import Data.Foldable (foldl)
import Data.FunctorWithIndex
import Data.Maybe
import Data.Int
import Math ((%), floor)
import Tetris.Shape as Tetris
import Tetris.Types as Tetris

coordinateToRowPos :: Tetris.Coordinate -> Int
coordinateToRowPos c = ceil $ (c.x % 10.0) * (c.y / 20.0)

rowPosToCoordinate :: Int -> Tetris.Coordinate
rowPosToCoordinate i = {x: (floor $ toNumber i % 10.0) * blockWidth, y: (floor (toNumber i / 100.0)) * blockHeight}

initi :: Maybe (Array Tetris.Color)
initi = pure $ map (const Tetris.White) $ range 0 200

update' :: Tetris.BlockShape -> Maybe (Array Tetris.Color) -> Maybe (Array Tetris.Color)
update' bs cs = do
  foldl (\b a -> join $ updateAt (coordinateToRowPos a) (Tetris.shapeToColorType bs.shape) <$> b) cs (Tetris.blockToArr bs.pos)

f1 :: Maybe (Array Tetris.Color) -> Array Tetris.BlockShape -> Maybe (Array Tetris.Color)
f1 cs [] = cs
f1 cs xs = case uncons xs of
  Just {head, tail} -> f1 (update' head cs) tail
  _                 -> f1 cs []

toRowList :: Array Tetris.BlockShape -> Maybe (Array (Tuple Int Tetris.Color))
toRowList xs = mapWithIndex Tuple <$> f1 initi xs
