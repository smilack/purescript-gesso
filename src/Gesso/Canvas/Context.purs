-- | Functions for getting a variety of canvas rendering contexts.
module Gesso.Canvas.Context
  ( getDefault2D
  , get2D
  , getDefaultWebGL
  , getWebGL
  , getDefaultWebGL2
  , getWebGL2
  , defaultWebGLContextAttributes
  , getDefaultWebGPU
  , getWebGPU
  , module Exports
  ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Maybe (Maybe)
import Data.Nullable as Nullable
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Gesso.Canvas.Context.Context2D (CanvasRenderingContext2DSettings, defaultContext2DSettings)
import Gesso.Canvas.Context.Context2D (CanvasRenderingContext2DSettings, PredefinedColorSpace, CanvasColorType, sRGB, displayP3, unorm8, float16, defaultContext2DSettings) as Exports
import Gesso.Canvas.Element (getCanvasHTMLElementById)
import Graphics.Canvas (CanvasElement, getCanvasElementById, Context2D)
import Graphics.Canvas (getCanvasElementById) as Exports
import Graphics.WebGL.Raw (ContextAttributes, getContext) as GL
import Graphics.WebGL.Raw (GL)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (GPUCanvasContext)
import Web.GPU.GPUCanvasContext (configure) as GPU
import Web.GPU.HTMLCanvasElement (getContext) as GPU

-- ┌────┐
-- │ 2D │
-- └────┘

-- | Get a `CanvasRenderingContext2D` with default settings.
getDefault2D :: String -> Effect (Maybe Context2D)
getDefault2D = get2D defaultContext2DSettings

-- | Get a `CanvasRenderingContext2D` with custom settings.
get2D :: CanvasRenderingContext2DSettings -> String -> Effect (Maybe Context2D)
get2D attrs id = getCanvasElementById id >>= traverse (getContext2D attrs)

foreign import getContext2DImpl :: EffectFn2 CanvasRenderingContext2DSettings CanvasElement Context2D

getContext2D :: CanvasRenderingContext2DSettings -> CanvasElement -> Effect Context2D
getContext2D = runEffectFn2 getContext2DImpl

-- ┌───────┐
-- │ WebGL │
-- └───────┘

-- | Get a `WebGLRenderingContext` with default settings.
getDefaultWebGL :: String -> Effect (Maybe GL)
getDefaultWebGL = getWebGL defaultWebGLContextAttributes

-- | Get a `WebGLRenderingContext` with custom settings.
getWebGL :: GL.ContextAttributes -> String -> Effect (Maybe GL)
getWebGL = getWebGLContext "webgl"

-- | Get a `WebGL2RenderingContext` with default settings.
getDefaultWebGL2 :: String -> Effect (Maybe GL)
getDefaultWebGL2 = getWebGL2 defaultWebGLContextAttributes

-- | Get a `WebGL2RenderingContext` with custom settings.
getWebGL2 :: GL.ContextAttributes -> String -> Effect (Maybe GL)
getWebGL2 = getWebGLContext "webgl2"

-- | Default values for `WebGLRenderingContext`, based on the
-- | [WebGL Specification](https://registry.khronos.org/webgl/specs/latest/1.0/#WEBGLCONTEXTATTRIBUTES)
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

-- | Get a `GPUCanvasContext` with default settings.
getDefaultWebGPU :: String -> Effect (Maybe GPUCanvasContext)
getDefaultWebGPU id = runMaybeT do
  canvas <- MaybeT $ getCanvasHTMLElementById id
  MaybeT $ GPU.getContext canvas

-- | Get a `GPUCanvasContext` with custom settings.
getWebGPU :: GPUCanvasConfiguration -> String -> Effect (Maybe GPUCanvasContext)
getWebGPU config id = runMaybeT do
  context <- MaybeT $ getDefaultWebGPU id
  lift $ GPU.configure context config
  pure context
