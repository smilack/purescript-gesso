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
  ( launch
  , launchIn
  , module Exports
  , run
  , runGessoAff
  ) where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Gesso.Application as GApp
import Gesso.Canvas as GCan
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Exports
import Halogen.Aff (awaitLoad, runHalogenAff, selectElement) as HAff
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode (QuerySelector(..)) as Exports
import Web.HTML.HTMLElement (HTMLElement)

-- | Launch a Gesso application in the page body.
launch
  :: forall state input ouput
   . GApp.AppSpec state input ouput
  -> Effect Unit
launch = launchIn "body"

-- | Launch a Gesso application in a given element. The String argument should
-- | be a valid query selector for some element on the page.
launchIn
  :: forall state input ouput
   . String
  -> GApp.AppSpec state input ouput
  -> Effect Unit
launchIn selector input = runGessoAff do
  HAff.awaitLoad
  target <- HAff.selectElement (QuerySelector selector)
  element <- maybe err pure target
  run input element
  where
  err = throwError $ error $ "Could not find " <> selector

-- | Run an `Aff` value such as the one produced by `run`. Alias for
-- | `Halogen.Aff.runHalogenAff`.
runGessoAff :: forall x. Aff x -> Effect Unit
runGessoAff = HAff.runHalogenAff

-- | An `Aff` which starts a Gesso application in the provided element. Used
-- | when performing other `Aff` effects at the same time as running the
-- | application.
run
  :: forall state input ouput
   . GApp.AppSpec state input ouput
  -> HTMLElement
  -> Aff Unit
run spec element = do
  _ <- runUI GCan.component spec element
  pure unit
