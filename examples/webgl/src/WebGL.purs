module Gesso.Example.WebGL (main) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (cos, pi, sin, (%))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (logShow)
import Gesso (launch_)
import Gesso.Application (WindowMode(..), nominalBehavior)
import Gesso.Canvas.Context (getDefaultWebGL)
import Gesso.Geometry (Scalers, fromMouseEvent, null)
import Gesso.Interactions (MouseInteraction, onMouseDown)
import Gesso.State (States)
import Gesso.Time (Delta)
import Graphics.WebGL.Raw (GL)
import Graphics.WebGL.Raw as GL

type State = Maybe
  { program :: GL.Program
  , positionAttrs :: Int
  , positionBuffer :: GL.Buffer
  , uColor :: GL.Uniform
  , uAngle :: GL.Uniform
  }

main :: Effect Unit
main = launch_
  { name: "hello"
  , initialState: Nothing
  , viewBox: null
  , window: Fullscreen
  , behavior: nominalBehavior
      { render = render
      , init = init
      , getContext = getDefaultWebGL
      }
  }

init :: GL -> Delta -> Scalers -> State -> Effect (Maybe State)
init gl _ { canvas } _ = do
  GL.viewport gl (round canvas.width) (round canvas.height)
  GL.clearColor gl 0.0 0.0 0.0 1.0
  GL.clear gl GL.colorBufferBit

  vertexShader <- createShader GL.vertexShader vertexShaderSource
  fragmentShader <- createShader GL.fragmentShader fragmentShaderSource
  program <- createProgram [ vertexShader, fragmentShader ]

  positionAttrs <- GL.attribLocation gl program "a_position"
  positionBuffer <- GL.createBuffer gl
  GL.bindBuffer gl GL.arrayBuffer positionBuffer
  let positions = [ 0.0, 0.67, 0.57, -0.33, -0.57, -0.33 ]
  GL.bufferDataF32 gl GL.arrayBuffer positions GL.staticDraw

  uColor <- GL.uniformLocation gl program "u_color"
  uAngle <- GL.uniformLocation gl program "u_angle"

  pure $ Just (Just { program, positionAttrs, positionBuffer, uColor, uAngle })

  where
  createShader shaderType source = do
    shader <- GL.createShader gl shaderType
    GL.shaderSource gl shader source
    GL.compileShader gl shader
    pure shader

  createProgram shaders = do
    program <- GL.createProgram gl
    traverse_ (GL.attachShader gl program) shaders
    GL.linkProgram gl program
    pure program

render :: GL -> Delta -> Scalers -> States State -> Effect Unit
render gl { now } { canvas } { current } = case current of
  Nothing -> pure unit
  Just { program, positionAttrs, positionBuffer, uColor, uAngle } -> do
    GL.viewport gl (round canvas.width) (round canvas.height)
    GL.clearColor gl 0.0 0.0 0.0 0.0
    GL.clear gl GL.colorBufferBit

    let
      r = (cos (now / 1000.0) + 1.0) / 2.0
      g = (sin (now / 900.0) + 1.0) / 2.0
      b = (sin (now / 800.0) + 1.0) / 2.0

    GL.useProgram gl program
    GL.uniform4fv gl uColor [ r, g, b, 1.0 ]
    GL.uniform1f gl uAngle $ (now / 2000.0) % (2.0 * pi)

    GL.enableVertexAttribArray gl positionAttrs
    GL.bindBuffer gl GL.arrayBuffer positionBuffer
    GL.vertexAttribPointer gl positionAttrs 2 GL.float false 0 0
    GL.drawArrays gl GL.triangles 0 3

vertexShaderSource :: String
vertexShaderSource =
  """
attribute vec2 a_position;

uniform float u_angle;

void main() {
   vec2 u_rotation = vec2(sin(u_angle), cos(u_angle));
   vec2 rotatedPosition = vec2(
     a_position.x * u_rotation.y + a_position.y * u_rotation.x,
     a_position.y * u_rotation.y - a_position.x * u_rotation.x
   );
   gl_Position = vec4(rotatedPosition, 0, 1);
}"""

fragmentShaderSource :: String
fragmentShaderSource =
  """
precision mediump float;

uniform vec4 u_color;

void main() {
  gl_FragColor = u_color;
}"""
