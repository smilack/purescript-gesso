-- | The global Environment that `GessoM` and the `ManageState` typeclass run
-- | under.
module Gesso.Environment (Environment(..), busEmitter) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff)
import Effect.Aff.Bus (BusRW, BusR', read)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Subscription as HS

-- | The environment must contain a `Ref` to the global state of the application
-- | and a Bus for notifying subscribers of changes, but may contain other
-- | fields as well.
type Environment globalState more
  =
  { globalState :: Ref globalState
  , stateBus :: BusRW globalState
  | more
  }

-- | Create an emitter from a bus. It's unlikely that you want to call this
-- | function - `getEventSource` from `ManageState` in `GessoM` gives you the
-- | emitter directly.
busEmitter
  :: forall m globalState r
   . MonadAff m
  => BusR' r globalState
  -> m (HS.Emitter globalState)
busEmitter bus = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ forkAff $ forever
      $ do
        val <- read bus
        H.liftEffect $ HS.notify listener val
  pure emitter
