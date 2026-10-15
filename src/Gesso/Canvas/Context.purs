-- | TODO docs
module Gesso.Canvas.Context
  ( getDefault2D
  , get2D
  , defaultContext2DAttributes
  , Context2DAttributes
  , Context2DColorSpace
  , Context2DColorType
  , getDefaultWebGL
  , getWebGL
  , getDefaultWebGL2
  , getWebGL2
  , defaultWebGLContextAttributes
  , getDefaultWebGPU
  , getWebGPU
  , module Exports
  , getCanvasHTMLElementById
  , sRGB
  , displayP3
  , unorm8
  , float16
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Maybe (Maybe)
import Data.Nullable as Nullable
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Graphics.Canvas (CanvasElement, getCanvasElementById, Context2D)
import Graphics.Canvas (getCanvasElementById) as Exports
import Graphics.WebGL.Raw (ContextAttributes, getContext) as GL
import Graphics.WebGL.Raw (GL)
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (GPUCanvasContext)
import Web.GPU.GPUCanvasContext (configure) as GPU
import Web.GPU.HTMLCanvasElement (getContext) as GPU
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement (fromElement)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- ┌───────────────────┐
-- │ CanvasHTMLElement │
-- └───────────────────┘

getCanvasHTMLElementById :: String -> Effect (Maybe HTMLCanvasElement)
getCanvasHTMLElementById id =
  window
    >>= (document >>> map toNonElementParentNode)
    >>= (getElementById id >>> map (bindFlipped fromElement))

-- ┌────┐
-- │ 2D │
-- └────┘

getDefault2D :: String -> Effect (Maybe Context2D)
getDefault2D = get2D defaultContext2DAttributes

foreign import getContext2DImpl :: EffectFn2 Context2DAttributes CanvasElement Context2D

getContext2D :: Context2DAttributes -> CanvasElement -> Effect Context2D
getContext2D = runEffectFn2 getContext2DImpl

get2D :: Context2DAttributes -> String -> Effect (Maybe Context2D)
get2D attrs id = getCanvasElementById id >>= traverse (getContext2D attrs)

-- https://html.spec.whatwg.org/multipage/canvas.html#canvasrenderingcontext2dsettings
defaultContext2DAttributes :: Context2DAttributes
defaultContext2DAttributes =
  { alpha: true
  , colorSpace: sRGB
  , colorType: unorm8
  , desynchronized: false
  , willReadFrequently: false
  }

type Context2DAttributes =
  { alpha :: Boolean
  , colorSpace :: Context2DColorSpace
  , colorType :: Context2DColorType
  , desynchronized :: Boolean
  , willReadFrequently :: Boolean
  }

newtype Context2DColorSpace = Context2DColorSpace String

sRGB :: Context2DColorSpace
sRGB = Context2DColorSpace "srgb"

displayP3 :: Context2DColorSpace
displayP3 = Context2DColorSpace "display-p3"

newtype Context2DColorType = Context2DColorType String

unorm8 :: Context2DColorType
unorm8 = Context2DColorType "unorm8"

float16 :: Context2DColorType
float16 = Context2DColorType "float16"

-- ┌───────┐
-- │ WebGL │
-- └───────┘

getDefaultWebGL :: String -> Effect (Maybe GL)
getDefaultWebGL = getWebGL defaultWebGLContextAttributes

getWebGL :: GL.ContextAttributes -> String -> Effect (Maybe GL)
getWebGL = getWebGLContext "webgl"

getDefaultWebGL2 :: String -> Effect (Maybe GL)
getDefaultWebGL2 = getWebGL2 defaultWebGLContextAttributes

getWebGL2 :: GL.ContextAttributes -> String -> Effect (Maybe GL)
getWebGL2 = getWebGLContext "webgl2"

-- https://registry.khronos.org/webgl/specs/latest/1.0/#WEBGLCONTEXTATTRIBUTES
defaultWebGLContextAttributes :: GL.ContextAttributes
defaultWebGLContextAttributes =
  { alpha: true
  , depth: true
  , stencil: false
  , antialias: true
  , premultipliedAlpha: true
  , preserveDrawingBuffer: false
  }

getWebGLContext :: String -> GL.ContextAttributes -> String -> Effect (Maybe GL)
getWebGLContext glType attrs id = do
  mCanvas <- getCanvasElementById id
  mmGL <- sequence $ getContext' <$> mCanvas
  pure $ join mmGL
  where
  getContext' :: CanvasElement -> Effect (Maybe GL)
  getContext' canvas = Nullable.toMaybe <$> GL.getContext canvas glType attrs

-- ┌────────┐
-- │ WebGPU │
-- └────────┘

getDefaultWebGPU :: String -> Effect (Maybe GPUCanvasContext)
getDefaultWebGPU id = runMaybeT do
  canvas <- MaybeT $ getCanvasHTMLElementById id
  MaybeT $ GPU.getContext canvas

getWebGPU :: GPUCanvasConfiguration -> String -> Effect (Maybe GPUCanvasContext)
getWebGPU config id = runMaybeT do
  context <- MaybeT $ getDefaultWebGPU id
  lift $ GPU.configure context config
  pure context
