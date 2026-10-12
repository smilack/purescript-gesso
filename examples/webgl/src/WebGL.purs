module Gesso.Example.WebGL (main) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Gesso (launch_)
import Gesso.Application (WindowMode(..), defaultBehavior)
import Gesso.Canvas.Context (getDefaultWebGL)
import Gesso.Geometry (Scalers, fromMouseEvent, null)
import Gesso.Interactions (MouseInteraction, onMouseDown)
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
      , init = init
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

init :: GL -> Delta -> Scalers -> Color -> Effect (Maybe Color)
init gl _ { canvas } _ = do
  logShow canvas.rect
  GL.viewport gl (round canvas.width) (round canvas.height)
  GL.clearColor gl 0.0 0.0 0.0 1.0
  GL.clear gl GL.colorBufferBit

  vertexShader <- GL.createShader gl GL.vertexShader
  GL.shaderSource gl vertexShader vertexShaderSource
  GL.compileShader gl vertexShader

  fragmentShader <- GL.createShader gl GL.fragmentShader
  GL.shaderSource gl fragmentShader fragmentShaderSource
  GL.compileShader gl fragmentShader

  program <- GL.createProgram gl
  GL.attachShader gl program vertexShader
  GL.attachShader gl program fragmentShader
  GL.linkProgram gl program

  -- GL.detachShader gl program vertexShader
  -- GL.detachShader gl program fragmentShader

  GL.deleteShader gl vertexShader
  GL.deleteShader gl fragmentShader

  GL.enableVertexAttribArray gl 0
  buffer <- GL.createBuffer gl
  GL.bindBuffer gl GL.arrayBuffer buffer
  GL.vertexAttribPointer gl 0 1 GL.float false 0 0

  GL.useProgram gl program
  GL.drawArrays gl 0 0 1

  GL.deleteBuffer gl buffer
  GL.deleteProgram gl program

  pure Nothing

render :: GL -> Delta -> Scalers -> States Color -> Effect Unit
render gl _ { drawing } { current: { r, g, b } } = do
  -- GL.viewport gl (round drawing.width) (round drawing.height)
  -- GL.clearColor gl r g b 1.0
  -- GL.clear gl GL.colorBufferBit
  pure unit

vertexShaderSource :: String
vertexShaderSource =
  "#version 100\n\
  \void main() {\n\
  \  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);\n\
  \  gl_PointSize = 64.0;\n\
  \}"

fragmentShaderSource :: String
fragmentShaderSource =
  "#version 100\n\
  \void main() {\n\
  \  gl_FragColor = vec4(0.18, 0.54, 0.34, 1.0);\n\
  \}"
