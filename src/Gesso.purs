-- | This is the main entry point for Gesso applications and contains functions
-- | for running `Aff` values and creating the global environment that Gesso
-- | can use to share state.
module Gesso
  ( runGessoAff
  , run
  , runWithState
  , runWithM
  , hoist
  , mkEnv
  , canvas
  , module Halogen.Aff
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Gesso.Canvas as GCan
import Gesso.Environment (Environment)
import Gesso.GessoM (GessoM, runGessoM, class ManageState)
import Halogen as H
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Halogen.Aff
import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)
import Record (union) as Record
import Web.HTML.HTMLElement (HTMLElement)

-- | Run a Gesso `Aff` value such as the one produced by `run` or `runWithState`
runGessoAff :: forall x. Aff x -> Effect Unit
runGessoAff = HAff.runHalogenAff

-- | Create a Gesso component as a top-level Halogen component in the provided
-- | element. The component will not use a shared global state.
-- | 
-- | - `component` is any Gesso component such as [`canvas`](#v:canvas)
-- | - `input` is the component's input type
-- | - `element` is the parent element for the component, such as the result of
-- |   [`awaitBody`](#v:awaitBody)
run
  :: forall input q o
   . H.Component q input o (GessoM Unit ())
  -> input
  -> HTMLElement
  -> Aff Unit
run component input element = do
  env <- liftEffect $ mkPlainEnv unit
  _ <- runUI (hoist env component) input element
  pure unit

-- | Create a Gesso component as a top-level Halogen component in the provided
-- | element. The caller and component will have access to a shared global
-- | mutable state. The Ref for the state is returned in the `env` record.
-- | 
-- | - `component` is any Gesso component such as [`canvas`](#v:canvas)
-- | - `input` is the component's input type
-- | - `globalState` is the initial value of the global state
-- | - `fields` is any additional fields to add to the `env` record
-- | - `element` is the parent element for the component, such as the result of
-- |   [`awaitBody`](#v:awaitBody)
runWithState
  :: forall input globalState more q o
   . H.Component q input o (GessoM globalState more)
  -> input
  -> globalState
  -> { | more }
  -> HTMLElement
  -> Aff (Environment globalState more)
runWithState component input globalState fields element = do
  env <- liftEffect $ mkEnv fields $ globalState
  io <- runUI (hoist env component) input element
  pure env

-- | Create an Environment record containing any additional fields needed. This
-- | is already called by [`run`](#v:run) and [`runWithState`](#v:runWithState)
-- | but may be useful if you intend to run under a different monad using
-- | [`runWithM`](#v:runWithM).
-- | 
-- | - `fields` is any additional fields to add to the `env` record
-- | - `initialState` is the initial value of the global state
mkEnv :: forall globalState more. { | more } -> globalState -> Effect (Environment globalState more)
mkEnv fields initialState = do
  globalState <- Ref.new initialState
  stateBus <- Bus.make
  pure $ Record.union { globalState, stateBus } fields

-- | [`mkEnv`](#v:mkEnv) with no additional fields.
mkPlainEnv :: forall globalState. globalState -> Effect (Environment globalState ())
mkPlainEnv = mkEnv {}

-- | Hoist a Gesso component to run in an `Aff` context.
hoist
  :: forall globalState q i o e
   . Environment globalState e
  -> H.Component q i o (GessoM globalState e)
  -> H.Component q i o Aff
hoist = H.hoist <<< runGessoM

-- | Run a Gesso component with any monad implementing ManageState.
-- |
-- | - `runM` is a Natural Transformation from the monad to Aff, runGessoM is an
-- |   example
-- | - `component` is any Gesso component such as [`canvas`](#v:canvas)
-- | - `input` is the component's input type
-- | - `element` is the parent element for the component, such as the result of
-- |   [`awaitBody`](#v:awaitBody)
runWithM
  :: forall globalState q i o m
   . MonadAff m
  => ManageState m globalState
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
  :: forall localState appInput appOutput globalState m
   . MonadAff m
  => ManageState m globalState
  => H.Component (GCan.Query appInput) (GCan.Input localState globalState appInput appOutput) (GCan.Output appOutput) m
canvas = GCan.component
