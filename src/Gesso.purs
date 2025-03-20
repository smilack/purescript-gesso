-- | This is the main entry point for Gesso applications and contains functions
-- | for running `Aff` values. For a Halogen application where Gesso is the
-- | root component, typical usage of this module would be:
-- |
-- | ```purescript
-- | runGessoAff do
-- |   body <- awaitBody
-- |   run canvas canvasInput body
-- | ```
-- |
-- | See [`Gesso.Canvas.Input`](Gesso.Canvas.html#t:Input) for the type of
-- | `canvasInput`.
module Gesso
  ( runGessoAff
  , run
  , canvas
  , module Halogen.Aff
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as GApp
import Gesso.Canvas as GCan
import Halogen as H
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Halogen.Aff
import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (HTMLElement)

-- | Run a Gesso `Aff` value such as the one produced by `run`
runGessoAff :: forall x. Aff x -> Effect Unit
runGessoAff = HAff.runHalogenAff

-- | Create a Gesso component, such as [`canvas`](#v:canvas), as a top-level
-- | Halogen component in the provided element.
run
  :: forall input q o
   . H.Component q input o Aff
  -> input
  -> HTMLElement
  -> Aff Unit
run component input element = do
  _ <- runUI component input element
  pure unit

-- | The Gesso Canvas component. Wraps HTML Canvas and provides an interface for
-- | state, updates, and rendering.
canvas
  :: forall localState appInput appOutput m
   . MonadAff m
  => H.Component
       (GCan.CanvasInput appInput)
       (GApp.AppSpec localState appInput appOutput)
       (GCan.CanvasOutput appOutput)
       m
canvas = GCan.component
