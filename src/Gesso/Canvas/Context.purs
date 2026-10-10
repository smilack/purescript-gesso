-- | TODO docs
module Gesso.Canvas.Context {-(
  )-} where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe)
import Data.Nullable as Nullable
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Graphics.Canvas as Graphics.Canvas
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

getCanvasElement :: String -> Effect (Maybe CanvasElement)
getCanvasElement = getCanvasElementById

getCanvasHTMLElement :: String -> Effect (Maybe HTMLCanvasElement)
getCanvasHTMLElement id =
  window
    >>= (document >>> map toNonElementParentNode)
    >>= (getElementById id >>> map (bindFlipped fromElement))

-- make a ContextKind?

class RenderingContext :: Type -> Type -> Type -> Constraint
class RenderingContext ctxtype config context | ctxtype -> config context where
  getContext :: String -> Maybe config -> Effect (Maybe context)

data Context2D = Context2D
data WebGL = WebGL (Maybe GL.ContextAttributes)
data WebGL2 = WebGL2 (Maybe GL.ContextAttributes)
data WebGPU = WebGPU (Maybe GPUCanvasConfiguration)

-- TODO replace Unit
instance RenderingContext Context2D Unit Graphics.Canvas.Context2D where
  getContext id _ = getCanvasElement id >>= traverse Graphics.Canvas.getContext2D

getWebGlContext :: String -> String -> Maybe GL.ContextAttributes -> Effect (Maybe GL)
getWebGlContext glType id mConfig = do
  mCanvas <- getCanvasElementById id
  mmGl <- sequence $ getContext' <$> mCanvas <*> mConfig
  pure $ join mmGl
  where
  getContext' :: CanvasElement -> GL.ContextAttributes -> Effect (Maybe GL)
  getContext' can att = Nullable.toMaybe <$> GL.getContext can glType att

instance RenderingContext WebGL GL.ContextAttributes GL where
  getContext = getWebGlContext "webgl"

instance RenderingContext WebGL2 GL.ContextAttributes GL where
  getContext = getWebGlContext "webgl2"

instance RenderingContext WebGPU GPUCanvasConfiguration GPUCanvasContext where
  getContext id config = runMaybeT do
    canvas <- MaybeT $ getCanvasHTMLElement id
    context <- MaybeT $ GPU.getContext canvas
    MaybeT $ traverse (GPU.configure context) config
    pure context
