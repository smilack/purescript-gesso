-- | The global Environment that `GessoM` and the `ManageState` typeclass run
-- | under.
module Gesso.Environment (Environment(..), busEventSource) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff, killFiber, error)
import Effect.Aff.Bus (BusRW, BusR', read)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen.Query.EventSource (EventSource, affEventSource, Finalizer(..), emit)

-- | The environment must contain a `Ref` to the global state of the application
-- | and a Bus for notifying subscribers of changes, but may contain other
-- | fields as well.
type Environment globalState more
  = { globalState :: Ref globalState
    , stateBus :: BusRW globalState
    | more
    }

-- | Convert a bus to a Halogen event source. It's unlikely that you want to
-- | call this function - `getEventSource` from `ManageState` in `GessoM` gives
-- | you the event source directly.
busEventSource ::
  forall globalState m r.
  MonadAff m =>
  BusR' r globalState ->
  EventSource m globalState
busEventSource bus =
  affEventSource \emitter -> do
    fiber <- forkAff $ forever $ emit emitter =<< read bus
    pure $ Finalizer (killFiber (error "Event source closed") fiber)
