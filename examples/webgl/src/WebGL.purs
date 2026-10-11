module Gesso.Example.WebGL (main) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Gesso (launch_)
import Gesso.Application (WindowMode(..), defaultBehavior)
import Gesso.Canvas.Context (getDefaultWebGL)
import Gesso.Geometry (Scalers, fromMouseEvent, null)
import Gesso.Interactions (Interaction, MouseEvent, MouseInteraction, onMouseDown)
import Gesso.State (States)
import Gesso.Time (Delta)
import Graphics.WebGL.Raw (GL)
import Graphics.WebGL.Raw as GL

type Color = { r :: Number, g :: Number, b :: Number }

main :: Effect Unit
main = launch_
  { name: "hello"
  , initialState: { r: 0.0, g: 0.0, b: 0.0 }
  , viewBox: null
  , window: Fullscreen
  , behavior: defaultBehavior
      { render = render
      , getContext = getDefaultWebGL
      , interactions { mouse = [ getColor ] }
      }
  }

-- getColor :: MouseEvent -> Delta -> Scalers -> Color -> Effect (Maybe Color)
getColor :: MouseInteraction Color
getColor = onMouseDown $ fromMouseEvent >>>
  \{ x, y } _ { drawing, canvas } _ -> do
    logShow { x, y }
    logShow drawing.rect
    logShow canvas.rect
    pure $ Just $ { r: 0.0, g: x / canvas.width, b: y / canvas.height }

render :: GL -> Delta -> Scalers -> States Color -> Effect Unit
render gl _ { drawing } { current: { r, g, b } } = do
  GL.viewport gl (round drawing.width) (round drawing.height)
  GL.clearColor gl r g b 1.0
  GL.clear gl GL.colorBufferBit
  pure unit
