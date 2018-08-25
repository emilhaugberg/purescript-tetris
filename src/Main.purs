module Main where

import Prelude
import Config
import Math ((%))

import Data.Array
import Data.Maybe (Maybe(..))
import Data.Traversable
import Data.Unit

import Effect (Effect)
import Effect.Console (log)
import Effect.Random
import Effect.Ref
import Effect.Timer

import Graphics.Canvas

import Tetris.Shape  as Tetris
import Tetris.Draw   as Tetris
import Tetris.Move   as Tetris
import Tetris.Rotate as Tetris
import Tetris.Types  as Tetris

import Web.Event.Event
import Web.Event.EventTarget
import Web.Event.Internal.Types

foreign import window  :: EventTarget
foreign import keyCode :: Event -> Int

step c = {current: {shape: c.current.shape, pos: Tetris.moveBlocks Tetris.Down c.current.pos, rotation: c.current.rotation}, previous: c.previous}

initialState :: Tetris.Shape -> Tetris.State
initialState s = {current: {shape: s, pos: Tetris.initialPos s, rotation: Tetris.Two}, previous: []}

keydownEvent :: EventType
keydownEvent = EventType "keydown"

updatePos :: Tetris.Block Tetris.Coordinate -> Tetris.Block Tetris.Coordinate
updatePos = map \p -> {x: p.x, y: p.y + blockHeight}

keyPress :: Ref Tetris.State -> Event -> Effect Unit
keyPress ref e = void $ modify move' ref
  where
    move' c =
      { current: { shape   : c.current.shape
                 , pos     : Tetris.nextCoord (keyCode e) c.current.shape c.current.rotation c.current.pos
                 , rotation: Tetris.nextRotation c.current.rotation (keyCode e)}
      , previous: c.previous
      }

checkCollision :: Tetris.Shape -> Ref Tetris.State -> Effect Unit
checkCollision sh ref = void $ modify coll ref
  where
    coll state = if Tetris.checkIfCollided state.current state.previous then next state else state
    next st    = {current: {shape: sh, pos: Tetris.initialPos sh, rotation: Tetris.Two}, previous: snoc st.previous st.current}

eventL :: Ref Tetris.State -> Effect EventListener
eventL ref = eventListener (keyPress ref)

randomShape :: Effect Tetris.Shape
randomShape = Tetris.intToShape <$> randomInt 1 7

stop :: IntervalId -> Effect Unit
stop = clearInterval

checkTop :: Tetris.State -> IntervalId -> Effect Unit
checkTop st id = do
  if atTop && (Tetris.checkIfCollided st.current st.previous) then clearInterval id else pure unit
  where
    atTop = foldl (\b a -> a.y <= 0.0 || b) false $ Tetris.blockToArr st.current.pos

main :: Partial => Effect Unit
main = void do
  Just canvas <- getCanvasElementById "tetris-canvas"
  ctx         <- getContext2D canvas
  sh          <- randomShape
  state       <- new (initialState sh)
  timer       <- new 0.0
  evF         <- eventL state

  addEventListener keydownEvent evF false window

  id <- setInterval 50 $ void do
    clearRect ctx {x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight}
    Tetris.drawGrid  numHorizontalBlocks numVerticalBlocks ctx

    sh' <- randomShape

    checkCollision sh' state

    s <- read state

    Tetris.drawShapes' s.previous ctx
    Tetris.drawShape s.current.pos s.current.shape ctx

    t <- read timer

    _ <- modify (if t % 1000.0 == 0.0 then step else identity) state

    modify ((+) 50.0) timer

  setInterval 50 $ void do
    st <- read state
    checkTop st id
