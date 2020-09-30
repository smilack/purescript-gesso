module Gesso
  ( module Halogen.Aff
  , runGessoAff
  , run
  , mkPlainEnv
  , hoist
  , runWith
  , Input
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

-- I don't love this
type Input appState r
  = { appState :: appState | r }

-- Top-level app - no other UI. Returns env
run ::
  forall appState r q o.
  H.Component HTML q (Input appState r) o (GessoM appState ()) ->
  (Input appState r) ->
  HTMLElement ->
  Aff (Environment appState ())
run component config element = do
  env <- liftEffect $ mkPlainEnv config.appState
  io <- runUI (hoist env component) config element
  pure env

mkPlainEnv :: forall appState. appState -> Effect (Environment appState ())
mkPlainEnv initialState = do
  appState <- Ref.new initialState
  stateBus <- Bus.make
  pure { appState, stateBus }

hoist ::
  forall appState q i o e.
  Environment appState e ->
  H.Component HTML q i o (GessoM appState e) ->
  H.Component HTML q i o Aff
hoist = H.hoist <<< runGessoM

--Supply your own ManageState monad
runWith ::
  forall appState q i o m.
  MonadAff m =>
  ManageState m appState =>
  m ~> Aff ->
  H.Component HTML q i o m ->
  i ->
  HTMLElement ->
  Aff Unit
runWith runM component config element = do
  _ <- runUI (H.hoist runM component) config element
  pure unit
