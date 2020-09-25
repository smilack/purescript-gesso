module Gesso.GessoM where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT, class MonadAsk, asks, ask)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Gesso.Env (Env)
import Halogen (HalogenM, lift)
import Type.Equality (class TypeEquals, from)

newtype GessoM appState a
  = GessoM (ReaderT (Env appState) Aff a)

runGessoM :: forall appState. Env appState -> GessoM appState ~> Aff
runGessoM env (GessoM m) = runReaderT m env

derive newtype instance functorGessoM :: Functor (GessoM appState)

derive newtype instance applyGessoM :: Apply (GessoM appState)

derive newtype instance applicativeGessoM :: Applicative (GessoM appState)

derive newtype instance bindGessoM :: Bind (GessoM appState)

derive newtype instance monadGessoM :: Monad (GessoM appState)

derive newtype instance monadEffectGessoM :: MonadEffect (GessoM appState)

derive newtype instance monadAffGessoM :: MonadAff (GessoM appState)

instance monadAskGessoM :: TypeEquals e (Env appState) => MonadAsk e (GessoM appState) where
  ask = GessoM $ asks from

class
  Monad m <= ManageState m a | m -> a where
  getState :: m a
  putState :: a -> m Unit

instance manageStateHalogenM :: ManageState m appState => ManageState (HalogenM st act slots msg m) appState where
  getState = lift getState
  putState = lift <<< putState

instance manageStateGessoM :: ManageState (GessoM appState) appState where
  getState = do
    env <- ask
    liftEffect $ Ref.read env.appState
  putState appState' = do
    env <- ask
    liftEffect $ Ref.write appState' env.appState
