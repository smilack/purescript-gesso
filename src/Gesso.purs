-- | This is the main entry point for Gesso applications and contains functions
-- | for running `Aff` values. For a full-page Halogen application where Gesso
-- | is the root component, typical usage of this module would be:
-- | ```purescript
-- | main :: Effect Unit
-- | main = Gesso.launch appSpec
-- | ```
-- | Or it could be confined to an element on the page, for example:
-- | ```purescript
-- | main :: Effect Unit
-- | main = Gesso.launchIn "#some-element-id" appSpec
-- | ```
-- | If it's necessary to perform other `Aff` actions, the `run` function is
-- | available.
-- | ```purescript
-- | runGessoAff do
-- |   body <- awaitBody
-- |   Gesso.run appSpec body
-- | ```
-- | When Gesso is a subcomponent of another Halogen component, run Halogen
-- | normally and include Gesso with a `Slot`.
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

-- | Launch a Halogen application in the page body with the Gesso canvas as the
-- | root component.
launch :: forall state i o. GApp.AppSpec state i o -> Effect Unit
launch = launchIn "body"

-- | Launch a Halogen application in a given element with the Gesso canvas as
-- | the root component. The String argument should be a valid query selector
-- | for some element on the page.
launchIn :: forall state i o. String -> GApp.AppSpec state i o -> Effect Unit
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

-- | An `Aff` which starts a Halogen application in the provided element, using
-- | a Gesso component with the given spec as the top-level component.
run
  :: forall state i o
   . GApp.AppSpec state i o
  -> HTMLElement
  -> Aff Unit
run spec element = do
  _ <- runUI GCan.component spec element
  pure unit
