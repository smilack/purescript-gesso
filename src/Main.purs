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
    $ { origin: Dims.origin
      , aspectRatio: AR.w1h1
      , renderFn
      , appState: initialState
      }

renderFn :: GC.RenderStyle AppState
renderFn = GC.Continuous render
  where
  render :: AppState -> T.Delta -> Dims.ViewBox -> Canvas.Context2D -> Effect Unit
  render { color, mouse } { now } viewBox context = do
    Canvas.setFillStyle context "#DDFFDD"
    Canvas.fillRect context { x: scaleW 5.0, y: scaleH 5.0, width: scaleW 630.0, height: scaleH 630.0 }
    Canvas.setFillStyle context color
    Canvas.fillRect context { x: scaleW 100.0, y: scaleH 50.0, width: scaleW 200.0, height: scaleH 25.0 }
    Canvas.fillRect context
      { x: scaleW $ 150.0 + 50.0 * cos t
      , y: scaleH $ 150.0 + 50.0 * sin t
      , width: scaleW 10.0
      , height: scaleH 10.0
      }
    traverse_ (Canvas.fillRect context) (mouseRect <$> mouse)
    where
    mouseRect { x, y } = { x, y, width: scaleW 30.0, height: scaleH 30.0 }

    t = (unwrap now) * 6.28 / 1000.0 / 2.0

    scaleW = (_ / 640.0) <<< (_ * Dims.getWidth viewBox)

    scaleH = (_ / 640.0) <<< (_ * Dims.getHeight viewBox)
