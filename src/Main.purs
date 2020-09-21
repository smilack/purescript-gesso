module Main where

import Prelude
import Control.Coroutine as CR
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Debug.Trace (trace, traceM, spy)
import Effect (Effect)
import Gesso.Application as App
import Gesso.AspectRatio as AR
import Gesso.Canvas as GC
import Gesso.Dimensions as Dims
import Gesso.Time as T
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Math (cos, sin)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    io <- runUI GC.component init body
    io.subscribe
      $ CR.consumer
      $ case _ of
          GC.StateUpdated appState' -> do
            pure Nothing
          GC.MouseMove p -> do
            let
              x = Dims.getX p

              y = Dims.getY p
            _ <- io.query $ H.tell $ GC.UpdateAppState (initialState { mouse = Just { x, y } })
            pure Nothing
    -- -- io.dispose - kill Halogen app
    pure unit

type AppState
  = { color :: String
    , mouse :: Maybe { x :: Number, y :: Number }
    }

initialState :: AppState
initialState = { color: "blue", mouse: Nothing }

init :: GC.Input AppState
init =
  GC.Input
    { name: "test-app"
    , appState: initialState
    , app:
        App.mkApplication
          $ App.defaultApp
              { window = App.fullscreen
              , render = Just renderFn
              , viewBox =
                Dims.fromPointAndSize
                  Dims.origin
                  (Dims.fromHeightAndRatio { height: 360.0, aspectRatio: AR.w16h9 })
              }
    }

renderFn :: App.RenderStyle AppState
renderFn = App.continuous render
  where
  render :: AppState -> T.Delta -> Dims.Scaler -> Canvas.Context2D -> Effect Unit
  render { color, mouse } { now } { x_, y_, w_, h_, screen } context = do
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
    traverse_ (Canvas.fillRect context) (mouseRect <$> mouse)
    where
    mouseRect { x, y } = { x, y, width: w_ 30.0, height: h_ 30.0 }

    t = (unwrap now) * 6.28 / 1000.0 / 2.0
