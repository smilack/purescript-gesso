-- | Capabilities of the `ManageState` typeclass, one of the communication
-- | methods available between Gesso components and the outside application, and
-- | implementations of it for `HalogenM` and a new monad called `GessoM`.
module Gesso.GessoM
  ( GessoM
  , runGessoM
  , class ManageState
  , getBus
  , getEmitter
  , getState
  , putState
  , modifyState
  , modifyState_
  ) where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT, class MonadAsk, asks, ask)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Gesso.Environment (Environment, busEmitter)
import Halogen (HalogenM, lift)
import Halogen.Subscription (Emitter)
import Type.Equality (class TypeEquals, from)

-- | `GessoM` is a wrapper around a `ReaderT` that reads from an `Environment`.
newtype GessoM globalState more a
  = GessoM (ReaderT (Environment globalState more) Aff a)

-- | A `NaturalTransformation` from `GessoM` to `Aff`
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

-- | `ManageState` is a typeclass that provides functions for interacting with a
-- | Gesso `Environment`.
-- |
-- | - `getBus` returns the bus that notifies of updates to the global state
-- | - `getEmitter` returns a Halogen Subscriptions `Emitter` created from the
-- |   bus
-- | - `getState` returns the current value of the global state `Ref`
-- | - `putState` sets the value of the global state `Ref`
-- | - `modifyState` updates the current value of the global state `Ref` by
-- |   running a function on the current value, and returns the updated value.
-- | - `modifyState_` updates the current value of the global state `Ref` by
-- |   running a function on the current value, and returns `Unit`.
class
  Monad m <= ManageState m a | m -> a where
  getBus :: m (Bus.BusRW a)
  getEmitter :: m (Emitter a)
  getState :: m a
  putState :: a -> m Unit
  modifyState :: (a -> a) -> m a
  modifyState_ :: (a -> a) -> m Unit

instance manageStateHalogenM :: ManageState m globalState => ManageState (HalogenM state action slots output m) globalState where
  getBus = lift getBus
  getEmitter = lift getEmitter
  getState = lift getState
  putState = lift <<< putState
  modifyState = lift <<< modifyState
  modifyState_ = lift <<< modifyState_

instance manageStateGessoM :: ManageState (GessoM globalState more) globalState where
  getBus = do
    env <- ask
    pure env.stateBus
  getEmitter = do
    env <- ask
    busEmitter env.stateBus
  getState = do
    env <- ask
    liftEffect $ Ref.read env.globalState
  putState globalState' = do
    env <- ask
    liftEffect $ Ref.write globalState' env.globalState
    liftAff $ Bus.write globalState' env.stateBus
  modifyState fn = do
    env <- ask
    globalState' <- liftEffect $ Ref.modify fn env.globalState
    liftAff $ Bus.write globalState' env.stateBus
    pure globalState'
  modifyState_ fn = do
    env <- ask
    globalState' <- liftEffect $ Ref.modify fn env.globalState
    liftAff $ Bus.write globalState' env.stateBus
    pure unit
