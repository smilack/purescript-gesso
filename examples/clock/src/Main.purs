module Main where

import Prelude
import Data.Array (range)
import Data.Enum (fromEnum)
import Data.Foldable (traverse_, sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time as Time
import Effect (Effect)
import Effect.Now (nowTime)
import Gesso (runGessoAff, awaitBody, run) as G
import Gesso.Application as GApp
import Gesso.AspectRatio as GAR
import Gesso.Canvas (component, Input) as GC
import Gesso.Dimensions as GDim
import Gesso.Interactions as GInt
import Gesso.Time as GTime
import Graphics.Canvas as Canvas
import Math (cos, sin)

main :: Effect Unit
main =
  G.runGessoAff do
    body <- G.awaitBody
    G.run GC.component input body

type AppState
  = { color :: String
    , mousePos :: Maybe { x :: Number, y :: Number }
    }

initialState :: AppState
initialState = { color: "blue", mousePos: Nothing }

input :: GC.Input AppState
input =
  { name: "test-app"
  , appState: initialState
  , app:
      GApp.mkApplication
        $ GApp.defaultApp
            { window = GApp.fullscreen
            , render = Just $ GApp.continuous renderFn
            }
  , viewBox:
      GDim.fromPointAndSize
        GDim.origin
        (GDim.fromHeightAndRatio { height: 360.0, aspectRatio: GAR.w16h9 })
  , interactions:
      GInt.default { mouse = [ GInt.mousePosition ] }
  }

renderFn :: AppState -> GTime.Delta -> GDim.Scaler -> Canvas.Context2D -> Effect Unit
renderFn { color, mousePos } { now } { x_, y_, w_, h_, screen, toVb } context = do
  Canvas.setLineWidth context 3.0
  Canvas.setFillStyle context "#FFDDDD"
  Canvas.setStrokeStyle context "#00FF00"
  Canvas.fillRect context { x: screen.x, y: screen.y, width: screen.width, height: screen.height }
  Canvas.strokeRect context { x: screen.x, y: screen.y, width: screen.width, height: screen.height }
  Canvas.setFillStyle context "#DDFFDD"
  Canvas.setStrokeStyle context "#FF0000"
  Canvas.fillRect context { x: x_ 0.0, y: y_ 0.0, width: w_ 640.0, height: h_ 360.0 }
  Canvas.strokeRect context { x: x_ 0.0, y: y_ 0.0, width: w_ 640.0, height: h_ 360.0 }
  Canvas.setFillStyle context color
  Canvas.fillRect context { x: x_ 100.0, y: y_ 50.0, width: w_ 200.0, height: h_ 25.0 }
  Canvas.fillRect context
    { x: x_ $ 150.0 + 50.0 * cos t
    , y: y_ $ 150.0 + 50.0 * sin t
    , width: w_ 10.0
    , height: h_ 10.0
    }
  traverse_ (Canvas.fillRect context) (mouseRect <$> mousePos)
  time <- nowTime
  let
    clock = { x: x_ 300.0, y: y_ 175.0 }

    hour = toNumber $ (_ `mod` 12) $ (_ + 7) $ fromEnum $ Time.hour time

    hrAng = (_ - 1.571) $ (_ * 6.283) $ (_ / 12.0) $ hour

    hr = { x: clock.x + w_ (cos hrAng * 40.0), y: clock.y + h_ (sin hrAng * 40.0) }

    minute = toNumber $ fromEnum $ Time.minute time

    mnAng = (_ - 1.571) $ (_ * 6.283) $ (_ / 60.0) $ minute

    min = { x: clock.x + w_ (cos mnAng * 60.0), y: clock.y + h_ (sin mnAng * 60.0) }

    second = toNumber $ fromEnum $ Time.second time

    scAng = (_ - 1.571) $ (_ * 6.283) $ (_ / 60.0) $ second

    sec = { x: clock.x + w_ (cos scAng * 60.0), y: clock.y + h_ (sin scAng * 60.0) }
  Canvas.setStrokeStyle context "black"
  Canvas.fillText context (show hour) (x_ 30.0) (y_ 30.0)
  Canvas.fillText context (show minute) (x_ 30.0) (y_ 40.0)
  Canvas.fillText context (show second) (x_ 30.0) (y_ 50.0)
  Canvas.strokePath context do
    Canvas.arc context { x: clock.x, y: clock.y, start: 0.0, end: 6.283, radius: w_ 75.0 }
  Canvas.strokePath context do
    Canvas.moveTo context clock.x clock.y
    Canvas.lineTo context hr.x hr.y
  Canvas.strokePath context do
    Canvas.moveTo context clock.x clock.y
    Canvas.lineTo context min.x min.y
  Canvas.setStrokeStyle context "red"
  Canvas.setLineWidth context 1.0
  Canvas.strokePath context do
    Canvas.moveTo context clock.x clock.y
    Canvas.lineTo context sec.x sec.y
  sequence_ $ map drawNum $ range 1 12
  where
  mouseRect { x, y } = { x: toVb.x_ x, y: toVb.y_ y, width: w_ 30.0, height: h_ 30.0 }

  drawNum i = do
    let
      ang = (_ - 1.571) $ (_ * 6.283) $ (_ / 12.0) $ toNumber $ (_ `mod` 12) $ i
    Canvas.fillText context (show i) (x_ $ 299.0 + 70.0 * cos ang) (y_ $ 177.0 + 70.0 * sin ang)

  t = (unwrap now) * 6.28 / 1000.0 / 2.0
