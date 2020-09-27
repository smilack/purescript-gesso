module Gesso.GessoM (GessoM, runGessoM, class ManageState, getState, putState) where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT, class MonadAsk, asks, ask)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref as Ref
import Gesso.Environment (Environment)
import Halogen (HalogenM, lift)
import Type.Equality (class TypeEquals, from)

newtype GessoM appState more a
  = GessoM (ReaderT (Environment appState more) Aff a)

runGessoM :: forall appState more. Environment appState more -> GessoM appState more ~> Aff
runGessoM env (GessoM m) = runReaderT m env

derive newtype instance functorGessoM :: Functor (GessoM appState more)

derive newtype instance applyGessoM :: Apply (GessoM appState more)

derive newtype instance applicativeGessoM :: Applicative (GessoM appState more)

derive newtype instance bindGessoM :: Bind (GessoM appState more)

derive newtype instance monadGessoM :: Monad (GessoM appState more)

derive newtype instance monadEffectGessoM :: MonadEffect (GessoM appState more)

derive newtype instance monadAffGessoM :: MonadAff (GessoM appState more)

instance monadAskGessoM :: TypeEquals e (Environment appState more) => MonadAsk e (GessoM appState more) where
  ask = GessoM $ asks from

class
  Monad m <= ManageState m a | m -> a where
  getState :: m a
  putState :: a -> m Unit

instance manageStateHalogenM :: ManageState m appState => ManageState (HalogenM state action slots output m) appState where
  getState = lift getState
  putState = lift <<< putState

instance manageStateGessoM :: ManageState (GessoM appState more) appState where
  getState = do
    env <- ask
    liftEffect $ Ref.read env.appState
  putState appState' = do
    env <- ask
    liftEffect $ Ref.write appState' env.appState
    liftAff $ Bus.write appState' env.stateBus
