module Gesso
  ( module Halogen.Aff
  , runGessoAff
  , run
  , Input
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Gesso.Canvas as GC
import Gesso.Environment (Environment)
import Gesso.GessoM (GessoM, runGessoM)
import Halogen as H
import Halogen.Aff (awaitBody, awaitLoad, selectElement) as Halogen.Aff
import Halogen.Aff as HAff
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (HTMLElement)

runGessoAff :: forall x. Aff x -> Effect Unit
runGessoAff = HAff.runHalogenAff

type Input appState r
  = { appState :: appState | r }

-- Top-level app - no other UI
run ::
  forall appState r q o.
  H.Component HTML q (Input appState r) o (GessoM appState ()) ->
  (Input appState r) ->
  HTMLElement ->
  Aff (Environment appState ())
--
-- how do I make it so I don't have to pass initial twice?
-- (in config and alone)
--
run component config element = do
  env <- liftEffect $ mkPlainEnv config.appState
  io <- runUI (H.hoist (runGessoM env) component) config element
  pure env

-- Mid-level?
--  ::
--  =
--
-- Halogen component, can be embedded in others
--  ::
--  =
mkPlainEnv :: forall appState. appState -> Effect (Environment appState ())
mkPlainEnv initial = do
  appState <- Ref.new initial
  stateBus <- Bus.make
  pure { appState, stateBus }
