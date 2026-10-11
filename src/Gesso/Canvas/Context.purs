-- | TODO docs
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
  , getCanvasHTMLElementById
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, getCanvasElementById, Context2D, getContext2D)
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

-- TODO: going to need a custom Context2D in order to pass attrs - need to make sure it's compatible with Graphics.Canvas

getDefault2D :: String -> Effect (Maybe Context2D)
getDefault2D = get2D Nothing

get2D :: forall attrs. attrs -> String -> Effect (Maybe Context2D)
get2D _ id = getCanvasElementById id >>= traverse getContext2D

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
