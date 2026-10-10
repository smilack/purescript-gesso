-- | TODO docs
module Gesso.Canvas.Context {-(
  )-} where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Gesso.Application (WindowMode(..)) as App
import Gesso.Geometry (Rect, Size) as Geo
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Graphics.Canvas as Canvas
import Graphics.Canvas as Graphics.Canvas
import Graphics.WebGL.Raw (GL)
import Graphics.WebGL.Raw (ContextAttributes, getContext) as GL
import Halogen.HTML (AttrName(..), attr)
import Web.DOM (Element)
import Web.DOM.Element (DOMRect, getBoundingClientRect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration) as WebGPU
import Web.GPU.GPUCanvasContext (GPUCanvasContext, configure) as WebGPU
import Web.GPU.HTMLCanvasElement (getContext) as WebGPU
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement (fromElement)
import Web.HTML.HTMLDocument (toNonElementParentNode, HTMLDocument)
import Web.HTML.HTMLDocument.VisibilityState as VisibilityState
import Web.HTML.Window (document)

getCanvasElement :: String -> Effect (Maybe CanvasElement)
getCanvasElement = getCanvasElementById

getCanvasHTMLElement :: String -> Effect (Maybe HTMLCanvasElement)
getCanvasHTMLElement id =
  window
    >>= (document >>> map toNonElementParentNode)
    >>= (getElementById id >>> map (bindFlipped fromElement))

-- make a ContextKind?

class Context :: Type -> Type -> Type -> Constraint
class Context ctxtype config context | ctxtype -> config context where
  getContext :: String -> Maybe config -> Effect (Maybe context)

data Context2D = Context2D
data WebGL = WebGL (Maybe GL.ContextAttributes)
data WebGL2 = WebGL2 (Maybe GL.ContextAttributes)
data WebGPU = WebGPU (Maybe WebGPU.GPUCanvasConfiguration)

instance Context Context2D Unit Graphics.Canvas.Context2D where
  getContext id _ = getCanvasElement id >>= traverse Graphics.Canvas.getContext2D

getWebGlContext :: String -> CanvasElement -> GL.ContextAttributes -> Effect (Maybe GL)
getWebGlContext string canvas attrs = Nullable.toMaybe <$> GL.getContext canvas string attrs

instance Context WebGL GL.ContextAttributes GL where
  getContext canvasId maybeContextAttrs =
    do
      maybeCanvas <- getCanvasElementById canvasId :: Effect (Maybe CanvasElement)

      let
        maybeGetContextForCanvas =
          traverse GL.getContext maybeCanvas :: String -> Maybe (GL.ContextAttributes -> Effect (Nullable GL))

        maybeGetWebGlContext =
          maybeGetContextForCanvas "webgl" :: Maybe (GL.ContextAttributes -> Effect (Nullable GL))

        maybeEffectNullableGl =
          maybeGetWebGlContext <*> maybeContextAttrs :: Maybe (Effect (Nullable GL))

        maybeEffectMaybeGl =
          map (map Nullable.toMaybe) maybeEffectNullableGl :: Maybe (Effect (Maybe GL))

        effectMaybeMaybeGl =
          sequence maybeEffectMaybeGl :: Effect (Maybe (Maybe GL))

      map join effectMaybeMaybeGl :: Effect (Maybe GL)

instance Context WebGL2 GL.ContextAttributes GL where
  getContext id config = runMaybeT do

    (elem {-:: ?em-} ) <- MaybeT $ (getCanvasElement id {-:: ?f-} )
    let
      (a {-:: ?a-} ) = map Nullable.toMaybe <$> GL.getContext elem "webgl2" <$> config
    -- (b {-:: ?b-} ) <- MaybeT $ join <$> sequence a
    -- pure b
    MaybeT $ join <$> sequence a

-- do
--   elem <- mElem
--   cfg <- config
--   Nullable.toMaybe <$> WebGL.getContext elem "webgl2" cfg

-- instance Context WebGPU WebGPU.GPUCanvasConfiguration WebGPU.GPUCanvasContext where
--   getContext id config = do
--     mElem <- getCanvasHTMLElement id
--     do
--       elem <- mElem
--       ctx <- WebGPU.getContext elem
--       cfg <- config
--       WebGPU.configure ctx cfg
--       pure ctx
