module Gesso.Environment (Environment(..), busEventSource) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff, killFiber, error)
import Effect.Aff.Bus (BusRW, BusR', read)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen.Query.EventSource (EventSource, affEventSource, Finalizer(..), emit)

type Environment globalState more
  = { globalState :: Ref globalState
    , stateBus :: BusRW globalState
    | more
    }

busEventSource ::
  forall globalState m r.
  MonadAff m =>
  BusR' r globalState ->
  EventSource m globalState
busEventSource bus =
  affEventSource \emitter -> do
    fiber <- forkAff $ forever $ emit emitter =<< read bus
    pure $ Finalizer (killFiber (error "Event source closed") fiber)
