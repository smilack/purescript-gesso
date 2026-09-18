-- | This is the main entry point for Gesso applications and contains functions
-- | for running `Aff` values. For a full-page application where Gesso is the
-- | root component, typical usage of this module would be:
-- | ```purescript
-- | main :: Effect Unit
-- | main = Gesso.launch appSpec
-- | ```
-- | Or to confine Gesso to an element on the page, use `launchIn` with a query
-- | selector:
-- | ```purescript
-- | main :: Effect Unit
-- | main = Gesso.launchIn "#some-element-id" appSpec
-- | ```
-- | If it's necessary to perform other `Aff` actions, the `run` function is
-- | available:
-- | ```purescript
-- | runGessoAff do
-- |   body <- awaitBody
-- |   Gesso.run appSpec body
-- | ```
-- | When Gesso is a subcomponent of another Halogen component, run Halogen and
-- | include Gesso as a child component in the standard way.
module Gesso
  ( module Exports
  , launch
  , launchIn
  , launchInElement
  , launch_
  , launchIn_
  , launchInElement_
  , make
  , makeIn
  , makeInElement
  , make_
  , makeIn_
  , makeInElement_
  ) where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, apathize, error, launchAff, throwError)
import Gesso.Application as GApp
import Gesso.Canvas (CanvasIO, wrapHalogenIO, component)
import Gesso.Canvas (CanvasIO) as Exports
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Exports
import Halogen.Aff (awaitLoad, selectElement) as HAff
import Halogen.VDom.Driver (runUI)
import PointFree ((<..))
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode (QuerySelector(..)) as Exports
import Web.HTML.HTMLElement (HTMLElement)

-- | Launch a Gesso component in the page body, in an `Effect` context,
-- | returning a `Fiber` for a `CanvasIO` record for the component.
launch
  :: forall state input output
   . GApp.AppSpec state input output
  -> Effect (Fiber (CanvasIO input output Aff))
launch = launchAff <<< make

-- | Launch a Gesso component in the first element matching the given query
-- | selector, in an `Effect` context, returning a `Fiber` for a `CanvasIO`
-- | record for the component.
launchIn
  :: forall state input output
   . String
  -> GApp.AppSpec state input output
  -> Effect (Fiber (CanvasIO input output Aff))
launchIn = launchAff <.. makeIn

-- | Launch a Gesso component in the given element, in an `Effect` context,
-- | returning a `Fiber` for a `CanvasIO` record for the component.
launchInElement
  :: forall state input output
   . HTMLElement
  -> GApp.AppSpec state input output
  -> Effect (Fiber (CanvasIO input output Aff))
launchInElement = launchAff <.. makeInElement

-- | Launch a Gesso component in the page body, in an `Effect` context,
-- | discarding the `Fiber`.
launch_
  :: forall state input output
   . GApp.AppSpec state input output
  -> Effect Unit
launch_ = void <<< launch

-- | Launch a Gesso component in the first element matching the given query
-- | selector, in an `Effect` context, discarding the `Fiber`.
launchIn_
  :: forall state input output
   . String
  -> GApp.AppSpec state input output
  -> Effect Unit
launchIn_ = void <.. launchIn

-- | Launch a Gesso component in the given element, in an `Effect` context,
-- | discarding the `Fiber`.
launchInElement_
  :: forall state input output
   . HTMLElement
  -> GApp.AppSpec state input output
  -> Effect Unit
launchInElement_ = void <.. launchInElement

-- | Launch a Gesso component in the page body, in an `Aff` context, returning a
-- | `CanvasIO` record for the component.
make
  :: forall state input output
   . GApp.AppSpec state input output
  -> Aff (CanvasIO input output Aff)
make = makeIn "body"

-- | Launch a Gesso component in the first element matching the given query
-- | selector, in an `Aff` context, returning a `CanvasIO` record for the
-- | component.
makeIn
  :: forall state input output
   . String
  -> GApp.AppSpec state input output
  -> Aff (CanvasIO input output Aff)
makeIn selector spec = do
  HAff.awaitLoad
  target <- HAff.selectElement (QuerySelector selector)
  element <- maybe err pure target
  makeInElement element spec
  where
  err = throwError $ error $ "Could not find " <> selector

-- | Launch a Gesso component in the given element, in an `Aff` context,
-- | returning a `CanvasIO` record for the component.
makeInElement
  :: forall state input output
   . HTMLElement
  -> GApp.AppSpec state input output
  -> Aff (CanvasIO input output Aff)
makeInElement element spec =
  map wrapHalogenIO $ runUI component spec element

-- | Launch a Gesso component in the page body, in an `Aff` context, discarding
-- | the result.
make_ :: forall state input output. GApp.AppSpec state input output -> Aff Unit
make_ = apathize <<< make

-- | Launch a Gesso component in the first element matching the given query
-- | selector, in an `Aff` context, discarding the result.
makeIn_
  :: forall state input output
   . String
  -> GApp.AppSpec state input output
  -> Aff Unit
makeIn_ = apathize <.. makeIn

-- | Launch a Gesso component in the given element, in an `Aff` context,
-- | discarding the result.
makeInElement_
  :: forall state input output
   . HTMLElement
  -> GApp.AppSpec state input output
  -> Aff Unit
makeInElement_ = apathize <.. makeInElement
