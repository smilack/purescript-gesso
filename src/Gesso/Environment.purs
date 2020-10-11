module Gesso.Environment (Environment(..), busEventSource) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff, killFiber, error)
import Effect.Aff.Bus (BusRW, BusR', read)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen.Query.EventSource (EventSource, affEventSource, Finalizer(..), emit)

type Environment appState more
  = { appState :: Ref appState
    , stateBus :: BusRW appState
    | more
    }

busEventSource ::
  forall appState m r.
  MonadAff m =>
  BusR' r appState ->
  EventSource m appState
busEventSource bus =
  affEventSource \emitter -> do
    fiber <- forkAff $ forever $ emit emitter =<< read bus
    pure $ Finalizer (killFiber (error "Event source closed") fiber)
