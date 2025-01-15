module Gesso.Canvas.Element
  ( getContext
  , getCanvasElement
  , getCanvasClientRect
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
import Web.DOM.Element (Element, DOMRect, getBoundingClientRect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- | Attempt to get the `Context2D` for this component's `canvas` element.
getContext :: String -> Effect (Maybe Context2D)
getContext name = do
  mcanvas <- getCanvasElementById name
  mcontext <- traverse getContext2D mcanvas
  pure mcontext

-- | Attempt to find the `canvas` element on the page.
getCanvasElement :: String -> Effect (Maybe Element)
getCanvasElement name = do
  doc <- document =<< window
  mcanvas <- getElementById name $ toNonElementParentNode doc
  pure mcanvas

-- | Attempt to get the bounding client rect for an HTML element and convert it
-- | to a `ClientRect` value.
getCanvasClientRect :: Maybe Element -> Effect (Maybe Dimensions.ClientRect)
getCanvasClientRect mcanvas = do
  (mbounding :: Maybe DOMRect) <- traverse getBoundingClientRect mcanvas
  pure $ Dimensions.fromDOMRect <$> mbounding

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

