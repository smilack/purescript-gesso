module Gesso
  ( module Halogen.Aff
  , runGessoAff
  , run
  , mkPlainEnv
  , hoist
  , runWith
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Gesso.Environment (Environment)
import Gesso.GessoM (GessoM, runGessoM, class ManageState)
import Halogen as H
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Halogen.Aff
import Halogen.Aff as HAff
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (HTMLElement)

runGessoAff :: forall x. Aff x -> Effect Unit
runGessoAff = HAff.runHalogenAff

-- Top-level app - no other UI. Returns env
run ::
  forall input globalState q o.
  H.Component HTML q input o (GessoM globalState ()) ->
  input ->
  globalState ->
  HTMLElement ->
  Aff (Environment globalState ())
run component config globalState element = do
  env <- liftEffect $ mkPlainEnv globalState
  io <- runUI (hoist env component) config element
  pure env

mkPlainEnv :: forall globalState. globalState -> Effect (Environment globalState ())
mkPlainEnv initialState = do
  globalState <- Ref.new initialState
  stateBus <- Bus.make
  pure { globalState, stateBus }

hoist ::
  forall globalState q i o e.
  Environment globalState e ->
  H.Component HTML q i o (GessoM globalState e) ->
  H.Component HTML q i o Aff
hoist = H.hoist <<< runGessoM

--Supply your own ManageState monad
runWith ::
  forall globalState q i o m.
  MonadAff m =>
  ManageState m globalState =>
  m ~> Aff ->
  H.Component HTML q i o m ->
  i ->
  HTMLElement ->
  Aff Unit
runWith runM component config element = do
  _ <- runUI (H.hoist runM component) config element
  pure unit
