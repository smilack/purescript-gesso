-- | This is the main entry point for Gesso applications and contains functions
-- | for running `Aff` values and creating the global environment that Gesso
-- | can use to share state.
module Gesso
  ( runGessoAff
  , run
  , runWithM
  , canvas
  , module Halogen.Aff
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Gesso.Canvas as GCan
import Halogen as H
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Halogen.Aff
import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (HTMLElement)

-- | Run a Gesso `Aff` value such as the one produced by `run`
runGessoAff :: forall x. Aff x -> Effect Unit
runGessoAff = HAff.runHalogenAff

-- | Create a Gesso component as a top-level Halogen component in the provided
-- | element.
-- | 
-- | - `component` is any Gesso component such as [`canvas`](#v:canvas)
-- | - `input` is the component's input type
-- | - `element` is the parent element for the component, such as the result of
-- |   [`awaitBody`](#v:awaitBody)
run
  :: forall input q o
   . H.Component q input o Aff
  -> input
  -> HTMLElement
  -> Aff Unit
run component input element = do
  _ <- runUI component input element
  pure unit

-- | Run a Gesso component with any monad implementing ManageState.
-- |
-- | - `runM` is a Natural Transformation from the monad to Aff, runGessoM is an
-- |   example
-- | - `component` is any Gesso component such as [`canvas`](#v:canvas)
-- | - `input` is the component's input type
-- | - `element` is the parent element for the component, such as the result of
-- |   [`awaitBody`](#v:awaitBody)
runWithM
  :: forall q i o m
   . MonadAff m
  => m ~> Aff
  -> H.Component q i o m
  -> i
  -> HTMLElement
  -> Aff Unit
runWithM runM component input element = do
  _ <- runUI (H.hoist runM component) input element
  pure unit

-- | The Gesso Canvas component. Wraps HTML Canvas and provides an interface for
-- | state, updates, and rendering.
canvas
  :: forall localState appInput appOutput m
   . MonadAff m
  => H.Component (GCan.Query appInput) (GCan.Input localState appInput appOutput) (GCan.Output appOutput) m
canvas = GCan.component
