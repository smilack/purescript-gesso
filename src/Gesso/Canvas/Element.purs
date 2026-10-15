-- | Internal helper functions that the Canvas component uses to style and
-- | position itself and access the canvas's drawing context.
module Gesso.Canvas.Element
  ( Canvas
  , getCanvasByAppName
  , getCanvasHTMLElementById
  , getCanvasClientRect
  , style
  , toSizeProps
  , visibilityState
  , WindowMode(..)
  ) where

import Prelude

import CSS (CSS)
import CSS as CSS
import Control.Bind (bindFlipped)
import Data.Int (round)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Gesso.Geometry (Rect, Size, Area) as Geo
import Halogen.HTML (AttrName(..), attr)
import Halogen.HTML.Properties (IProp, CSSPixel)
import Halogen.HTML.Properties (width, height) as HP
import Web.DOM.Element (Element, DOMRect, getBoundingClientRect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (fromElement, HTMLCanvasElement)
import Web.HTML.HTMLDocument (toNonElementParentNode, HTMLDocument)
import Web.HTML.HTMLDocument.VisibilityState as VisibilityState
import Web.HTML.Window (document)

-- | Wrapper for a `Web.DOM.Element.Element` to tag elements that came from this
-- | module.
newtype Canvas = Canvas Element

-- | Wrapper for `getElementById` which returns a `Web.HTML.HTMLCanvasElement`.
getCanvasHTMLElementById :: String -> Effect (Maybe HTMLCanvasElement)
getCanvasHTMLElementById = mapCanvasElement (bindFlipped fromElement)

-- | Wrapper for `getElementById` which returns a `Canvas`.
getCanvasByAppName :: String -> Effect (Maybe Canvas)
getCanvasByAppName = mapCanvasElement (map Canvas)

-- | Generic wrapper for `getElementById` that can be transformed into a
-- | different type depending on the type of element needed.
mapCanvasElement
  :: forall a. (Maybe Element -> Maybe a) -> String -> Effect (Maybe a)
mapCanvasElement f id =
  window
    >>= (document >>> map toNonElementParentNode)
    >>= (getElementById id >>> map f)

-- | Get the bounding client rect for a `Canvas` element and convert it to a
-- | `ClientRect` value.
getCanvasClientRect :: Canvas -> Effect Geo.Rect
getCanvasClientRect (Canvas canvas) =
  fromDOMRect <$> getBoundingClientRect canvas

fromDOMRect :: DOMRect -> Geo.Rect
fromDOMRect { left, top, width, height } = { x: left, y: top, width, height }

-- | Get size and positioning CSS based on the app's window mode.
style :: forall r i. WindowMode -> IProp (style :: String | r) i
style =
  attr (AttrName "style")
    <<< fromMaybe ""
    <<< CSS.renderedInline
    <<< CSS.rules []
    <<< CSS.runS
    <<< windowCss

-- | Get the appropriate CSS for the screen element based on the `WindowMode`.
windowCss :: WindowMode -> CSS
windowCss = case _ of
  Fixed size -> fix size
  Stretch -> stretched
  Fullscreen -> full
  Plain -> common
  where
  common = do
    CSS.key (CSS.fromString "outline") "none"

  fix { width, height } = do
    CSS.width $ CSS.px width
    CSS.height $ CSS.px height
    common

  full = do
    CSS.position CSS.fixed
    CSS.top $ CSS.px 0.0
    CSS.right $ CSS.px 0.0
    CSS.bottom $ CSS.px 0.0
    CSS.left $ CSS.px 0.0
    CSS.width $ CSS.vw 100.0
    CSS.height $ CSS.vh 100.0
    common

  stretched = do
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
    CSS.position CSS.absolute
    CSS.left $ CSS.pct 50.0
    CSS.top $ CSS.pct 50.0
    CSS.transform $ CSS.translate (CSS.pct $ -50.0) (CSS.pct $ -50.0)
    common

-- | Convert a `width` and `height` to HTML properties.
toSizeProps
  :: forall i r s
   . { | Geo.Size Number s }
  -> Array (IProp (width :: CSSPixel, height :: CSSPixel | r) i)
toSizeProps { width, height } =
  [ HP.width $ round width
  , HP.height $ round height
  ]

-- | Read the visibility state of a document.
-- The `visibilityState` function in `Web.HTML.HTMLDocument` is broken in the
-- most recently published version (4.1.1); it always returns `Visible` because
-- it actually checks `document.readyState`, so `VisibilityState.parse` fails.
-- When the fixed version is published, this function can be retired.
visibilityState :: HTMLDocument -> Effect VisibilityState.VisibilityState
visibilityState doc =
  (fromMaybe VisibilityState.Visible <<< VisibilityState.parse)
    <$> (runEffectFn1 _visibilityState doc)

-- See `visibilityState`
foreign import _visibilityState :: EffectFn1 HTMLDocument String

-- | The modes that determine the CSS for sizing and positioning a Gesso canvas
-- | component:
-- |
-- | - `Fixed` creates an element of the specified size.
-- | - `Stretch` expands to fill its container¹
-- | - `FullScreen` takes up the entire page from the top left corner to the
-- |   bottom right.
-- | - `Plain` has no size or positioning CSS, so it must be styled from outside
-- |   of the application.
-- |
-- | ¹ More specifically, `Stretch` makes the canvas fill its
-- | [containing block](https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Display/Containing_block).
-- | The easiest way to deal with this is to give the parent element
-- | `position: relative;`.
-- |
-- | [style](#v:style) generates the
-- | CSS for the canvas element.
data WindowMode
  = Fixed Geo.Area
  | Stretch
  | Fullscreen
  | Plain
