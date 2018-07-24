module Main where

import Prelude
import Config as Config

import Data.Maybe (Maybe(..))
import Data.Traversable
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref
import Effect.Timer
import Graphics.Canvas
import Tetris.Shape as Tetris
import Tetris.Draw  as Tetris
import Tetris.Move  as Tetris
import Web.Event.Event
import Web.Event.EventTarget
import Web.Event.Internal.Types

keyupEvent :: EventType
keyupEvent = EventType "keydown"

foreign import window :: EventTarget
foreign import keyCode :: Event -> Int

updatePos :: Tetris.Block Config.Coordinate -> Tetris.Block Config.Coordinate
updatePos = map \p -> {x: p.x, y: p.y + Config.blockHeight}

keyPress :: Ref (Tetris.Block Config.Coordinate) -> Event -> Effect Unit
keyPress ref e = void $ modify (Tetris.moveBlocks (keyCode e)) ref

eventL ref = eventListener (keyPress ref)

main :: Partial => Effect Unit
main = void  do
  Just canvas <- getCanvasElementById "tetris-canvas"
  ctx         <- getContext2D canvas
  shape       <- new Tetris.L
  pos         <- new $ Tetris.initialPos Tetris.L
  evF         <- eventL pos

  addEventListener keyupEvent evF false window
  _ <- setInterval 50 $ void do
    clearRect ctx {x: 0.0, y: 0.0, width: Config.canvasWidth, height: Config.canvasHeight}
    Tetris.drawGrid  Config.numHorizontalBlocks Config.numVerticalBlocks ctx

    s <- read shape
    p <- read pos

    Tetris.drawShape p s ctx

  setInterval 1000 $ void do
    modify updatePos pos
