module Main where

import Prelude
import Control.Coroutine as CR
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Debug.Trace (trace, traceM, spy)
import Effect (Effect)
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
          GC.FrameStart delta -> do
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
    $ { viewBox:
          Dims.fromPointAndSize
            Dims.origin
            (Dims.fromHeightAndRatio { height: 360.0, aspectRatio: AR.w16h9 })
      , renderFn
      , appState: initialState
      }

renderFn :: GC.RenderStyle AppState
renderFn = GC.Continuous render
  where
  render :: AppState -> T.Delta -> Dims.Scaler -> Canvas.Context2D -> Effect Unit
  render { color, mouse } { now } { x_, y_, margin } context = do
    Canvas.setFillStyle context "#DDFFDD"
    Canvas.fillRect context { x: x_ 5.0, y: y_ 5.0, width: x_ 630.0, height: y_ 630.0 }
    Canvas.setFillStyle context color
    Canvas.fillRect context { x: x_ 100.0, y: y_ 50.0, width: x_ 200.0, height: y_ 25.0 }
    Canvas.fillRect context
      { x: x_ $ 150.0 + 50.0 * cos t
      , y: y_ $ 150.0 + 50.0 * sin t
      , width: x_ 10.0
      , height: y_ 10.0
      }
    traverse_ (Canvas.fillRect context) (mouseRect <$> mouse)
    where
    mouseRect { x, y } = { x, y, width: x_ 30.0, height: y_ 30.0 }

    t = (unwrap now) * 6.28 / 1000.0 / 2.0
