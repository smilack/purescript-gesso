module Gesso.Example.Timing where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Number ((%))
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Gesso (launch)
import Gesso.Application (WindowMode(..), defaultBehavior)
import Gesso.Geometry (Rect, Scaler, Scalers, (*~>), (-~>), (/~>), (<~*), origin)
import Gesso.State (States)
import Gesso.Time (Delta, hz)
import Graphics.Canvas (Context2D, clearRect, fillText, lineTo, moveTo, setFillStyle, setFont, setStrokeStyle, strokePath, setTextBaseline, TextBaseline(..))
import Prim.Row (class Cons)
import Record (set, merge)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  start <- _now
  launch
    { name: "timing"
    , window: Fullscreen
    , initialState:
        { start
        , reg: (100.0 / 6.0)
        , fixed: (100.0 / 6.0)
        }
    , viewBox: merge origin
        { width: 300.0
        , height: 90.0
        }
    , behavior: defaultBehavior
        { render = render
        , update = saveDelta @"reg"
        , fixed =
            { interval: hz 60.0
            , function: saveDelta @"fixed"
            }
        }
    }

foreign import _now :: Effect Number

type Data = { start :: Number, reg :: Number, fixed :: Number }

saveDelta
  :: forall @l t r s
   . IsSymbol l
  => Cons l Number t r
  => Delta
  -> s
  -> { | r }
  -> Effect (Maybe { | r })
saveDelta { delta } _ = pure <<< Just <<< set (Proxy @l) delta

-- Scale timing data to fit onto graphs:
--
--   x: 1. Adjust time scale so graph doesn't move too quickly.
--      2. Wrap around from right to left.
--
--   y: 1. Get difference from target of 60 Hz.
--      2. Scale up for emphasis.
--      3. Set position relative to x-axis.
graph :: { x :: Number -> Number, y :: Number -> Number -> Number }
graph =
  { x: \x -> (x / 30.0) % 300.0
  , y: \baseline y -> (y - 100.0 / 6.0) * 3.0 + baseline
  }

render :: Context2D -> Delta -> Scalers -> States Data -> Effect Unit
render ctx { now, last } { canvas } { previous, current } = do
  let
    x1 = graph.x $ last - current.start
    x2 = graph.x $ now - current.start
    fixed = canvas <~*
      { x1
      , x2
      , y1: graph.y 30.0 previous.fixed
      , y2: graph.y 30.0 current.fixed
      }
    reg = canvas <~*
      { x1
      , x2
      , y1: graph.y 70.0 previous.reg
      , y2: graph.y 70.0 current.reg
      }
    sweep =
      { x: x2 -~> canvas
      , y: 0.0
      , width: 20.0 /~> canvas
      , height: canvas.height
      }

  clearRect ctx sweep

  -- don't draw line from end to start as it wraps around
  when (x2 > x1) do
    setStrokeStyle ctx "black"
    drawLine ctx fixed
    drawLine ctx reg

  drawLabels ctx canvas

drawLine
  :: Context2D
  -> { x1 :: Number, x2 :: Number, y1 :: Number, y2 :: Number }
  -> Effect Unit
drawLine ctx { x1, y1, x2, y2 } =
  strokePath ctx do
    moveTo ctx x1 y1
    lineTo ctx x2 y2

drawLabels :: Context2D -> Scaler -> Effect Unit
drawLabels ctx canvas = do
  setTextBaseline ctx BaselineMiddle
  for_ [ 30.0, 70.0 ] \b -> do
    let
      -- slight adjustment to make the dash line up
      y' t = (graph.y b t) - 0.1
    drawLabel "16pt Arial" { x: -1.0, y: y' (100.0 / 7.5) } "- 75 Hz—"
    drawLabel "16pt Arial" { x: -1.0, y: y' (100.0 / 6.0) } "- 60 Hz—"
    drawLabel "16pt Arial" { x: -1.0, y: y' (100.0 / 5.0) } "- 50 Hz—"

  setTextBaseline ctx BaselineTop
  drawLabel "26pt Arial" { x: 2.0, y: 1.0 } "Δt Received by update functions"
  drawLabel "20pt Arial" { x: 4.0, y: 12.0 } "Fixed update (60 Hz)"
  drawLabel "20pt Arial" { x: 4.0, y: 52.0 } "Per-frame update"
  where
  drawLabel font pos text = do
    let { x, y } = pos *~> canvas

    setFont ctx font
    rel@{ width, height } <- toRelativeBoundingRect <$> measureText ctx text

    clearRect ctx { width, height, x: x + rel.x, y: y + rel.y }
    setFillStyle ctx "black"
    fillText ctx text x y

-- Extra Canvas stuff

type TextMetrics =
  { width :: Number
  , actualBoundingBoxLeft :: Number
  , actualBoundingBoxRight :: Number
  , fontBoundingBoxAscent :: Number
  , fontBoundingBoxDescent :: Number
  , actualBoundingBoxAscent :: Number
  , actualBoundingBoxDescent :: Number
  , emHeightAscent :: Number
  , emHeightDescent :: Number
  , hangingBaseline :: Number
  , alphabeticBaseline :: Number
  , ideographicBaseline :: Number
  }

foreign import measureTextImpl :: EffectFn2 Context2D String TextMetrics

measureText :: Context2D -> String -> Effect TextMetrics
measureText = runEffectFn2 measureTextImpl

toRelativeBoundingRect :: TextMetrics -> Rect
toRelativeBoundingRect
  { actualBoundingBoxLeft
  , actualBoundingBoxRight
  , actualBoundingBoxAscent
  , actualBoundingBoxDescent
  } =
  { x: -actualBoundingBoxLeft
  , y: -actualBoundingBoxAscent
  , width: actualBoundingBoxRight + actualBoundingBoxLeft
  , height: actualBoundingBoxDescent + actualBoundingBoxAscent
  }
