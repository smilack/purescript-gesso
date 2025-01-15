module Gesso.Canvas.Element
  ( Canvas
  , getCanvasByAppName
  , getCanvasClientRect
  , getContextByAppName
  , style
  ) where

import Prelude

import CSS (CSS)
import CSS as CSS
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Gesso.Application as Application
import Gesso.Dimensions as Dimensions
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Halogen.HTML (AttrName(..), attr)
import Halogen.HTML.Properties (IProp)
import Web.DOM.Element (Element, getBoundingClientRect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- | Wrapper for a `Web.DOM.Element.Element` to tag elements that came from this
-- | module.
newtype Canvas = Canvas Element

-- | Wrapper for `getElementById` which returns a `Canvas`.
-- |
-- | `getElementById` from `Web.DOM.NonElementParentNode`, which returns a
-- | `Web.DOM.Element.Element`, is different from `getCanvasElementById` from
-- | `Graphics.Canvas`, which returns a `Graphics.Canvas.CanvasElement`, so
-- | they're unfortunately incompatible.
getCanvasByAppName :: String -> Effect (Maybe Canvas)
getCanvasByAppName name =
  window
    >>= document
      >>> map toNonElementParentNode
    >>= getElementById name
      >>> map (map Canvas)

-- | Get the bounding client rect for a `Canvas` element and convert it to a
-- | `ClientRect` value.
getCanvasClientRect :: Canvas -> Effect Dimensions.ClientRect
getCanvasClientRect (Canvas canvas) =
  Dimensions.fromDOMRect <$> getBoundingClientRect canvas

-- | Attempt to get the `Context2D` for this component's `canvas` element.
getContextByAppName :: String -> Effect (Maybe Context2D)
getContextByAppName name = getCanvasElementById name >>= traverse getContext2D

style :: forall r i. Application.WindowMode -> IProp (style :: String | r) i
style =
  attr (AttrName "style")
    <<< fromMaybe ""
    <<< CSS.renderedInline
    <<< CSS.rules []
    <<< CSS.runS
    <<< windowCss

-- | Get the appropriate CSS for the screen element based on the `WindowMode`.
windowCss :: Application.WindowMode -> CSS
windowCss = case _ of
  Application.Fixed size -> fix size
  Application.Stretch -> stretched
  Application.Fullscreen -> full
  where
  common = do
    CSS.key (CSS.fromString "outline") "none"

  fix size = do
    Dimensions.toSizeCss size
    common

  stretched = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    common

  full = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.position CSS.absolute
    CSS.left $ CSS.pct 50.0
    CSS.top $ CSS.pct 50.0
    CSS.transform $ CSS.translate (CSS.pct $ -50.0) (CSS.pct $ -50.0)
    common
