module Gesso.AppM where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT, class MonadAsk, asks, ask)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Gesso.Env (Env)
import Halogen (HalogenM, lift)
import Type.Equality (class TypeEquals, from)

newtype AppM appState a
  = AppM (ReaderT (Env appState) Aff a)

-- forall appState. Env appState -> NaturalTransformation (AppM appState) Aff
-- forall appState. Env appState -> (forall a. AppM appState a -> Aff a)
runAppM :: forall appState. Env appState -> AppM appState ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor (AppM appState)

derive newtype instance applyAppM :: Apply (AppM appState)

derive newtype instance applicativeAppM :: Applicative (AppM appState)

derive newtype instance bindAppM :: Bind (AppM appState)

derive newtype instance monadAppM :: Monad (AppM appState)

derive newtype instance monadEffectAppM :: MonadEffect (AppM appState)

derive newtype instance monadAffAppM :: MonadAff (AppM appState)

instance monadAskAppM :: TypeEquals e (Env appState) => MonadAsk e (AppM appState) where
  ask = AppM $ asks from

class
  Monad m <= ManageState m a | m -> a where
  getState :: m a
  putState :: a -> m Unit

instance manageStateHalogenM :: ManageState m appState => ManageState (HalogenM st act slots msg m) appState where
  getState = lift getState
  putState = lift <<< putState

instance manageStateAppM :: ManageState (AppM appState) appState where
  getState = do
    env <- ask
    liftEffect $ Ref.read env.appState
  putState appState' = do
    env <- ask
    liftEffect $ Ref.write appState' env.appState
