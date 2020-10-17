module Gesso.GessoM
  ( GessoM
  , runGessoM
  , class ManageState
  , getBus
  , getEventSource
  , getState
  , putState
  ) where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT, class MonadAsk, asks, ask)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref as Ref
import Gesso.Environment (Environment, busEventSource)
import Halogen (HalogenM, lift)
import Halogen.Query.EventSource (EventSource)
import Type.Equality (class TypeEquals, from)

newtype GessoM globalState more a
  = GessoM (ReaderT (Environment globalState more) Aff a)

runGessoM :: forall globalState more. Environment globalState more -> GessoM globalState more ~> Aff
runGessoM env (GessoM m) = runReaderT m env

derive newtype instance functorGessoM :: Functor (GessoM globalState more)

derive newtype instance applyGessoM :: Apply (GessoM globalState more)

derive newtype instance applicativeGessoM :: Applicative (GessoM globalState more)

derive newtype instance bindGessoM :: Bind (GessoM globalState more)

derive newtype instance monadGessoM :: Monad (GessoM globalState more)

derive newtype instance monadEffectGessoM :: MonadEffect (GessoM globalState more)

derive newtype instance monadAffGessoM :: MonadAff (GessoM globalState more)

instance monadAskGessoM :: TypeEquals e (Environment globalState more) => MonadAsk e (GessoM globalState more) where
  ask = GessoM $ asks from

class
  Monad m <= ManageState m a | m -> a where
  getBus :: m (Bus.BusRW a)
  getEventSource :: forall n. MonadAff n => m (EventSource n a)
  getState :: m a
  putState :: a -> m Unit

instance manageStateHalogenM :: ManageState m globalState => ManageState (HalogenM state action slots output m) globalState where
  getBus = lift getBus
  getEventSource = lift getEventSource
  getState = lift getState
  putState = lift <<< putState

instance manageStateGessoM :: ManageState (GessoM globalState more) globalState where
  getBus = do
    env <- ask
    pure env.stateBus
  getEventSource = do
    env <- ask
    pure $ busEventSource env.stateBus
  getState = do
    env <- ask
    liftEffect $ Ref.read env.globalState
  putState globalState' = do
    env <- ask
    liftEffect $ Ref.write globalState' env.globalState
    liftAff $ Bus.write globalState' env.stateBus
